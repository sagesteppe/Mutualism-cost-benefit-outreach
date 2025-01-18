setwd('~/Documents/Mutualism-cost-benefit-outreach/scripts')
source('functions.R')

library(sf)
library(ggiraph)
library(tidyverse)

plants <- st_read("../data/solidago_presences/solidago_presences.shp") |>
  group_by(CmbndID) |>
  summarize(
    Presence = max(Presenc), 
    Count = if_else(is.na(sum(N_pls_c)), 0,  sum(N_pls_c)),
    geometry = st_union(geometry)
  ) |>
  sf::st_centroid()
  
occs <- filter(plants, Presence > 0)
occs <- mutate(
  occs, 
  cluster = factor(fpc::dbscan(st_coordinates(occs), eps = 110, MinPts = 3)$cluster), 
  .before = geometry) 
  
# create concave hulls to encapsulate our clusters. 
hulls <- occs %>% 
   # filter(cluster != 0) %>% 
    st_buffer(50) %>% 
    group_by(cluster) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_concave_hull(allow_holes = FALSE, ratio = 0.2) %>% 
    mutate(cluster = as_factor(cluster))  


hulls <- smoothr::smooth(hulls, method = "ksmooth")
link_collection <- graphLines(occs, d2_dist = 500) |>
  filter(length > 100 & length < 400)

# we will show what estimated proportion of the population is within each hull. 

clust_ct <- occs |>
  group_by(cluster) |>
  st_drop_geometry() |>
  mutate(Count = if_else(Count==0, 1, Count)) |>
  summarize(Total_pls = sum(Count))

hulls <- left_join(hulls, clust_ct)

rm(clust_ct)
# we can also add another dimension to the plot, a simple idw, where we predict
# the number of plants per unit area. # EDIT THIS SUCKS 

surf <- terra::rast( terra::ext(plants), ncol = 100, nrow = 100, crs = 'epsg:26910')

df <- data.frame(
  cbind(plants$Count, sf::st_coordinates(plants))
) |>
  setNames(c('count', 'x', 'y'))
mod <- gstat::gstat(formula = count ~ 1, locations = ~x+y, data = df)
nn <- terra::interpolate(surf, mod)

nn <- terra::ifel(nn < 1, NA, nn)
plot(nn)







ggplot() +
  geom_sf(data = link_collection, aes(color = length), alpha = 0.5) + 
   geom_sf(data = hulls, aes(fill = cluster), alpha = 0.5) +
  geom_sf(data = occs,  color = "darkorchid3") 
  labs(title = "DBSCAN Clusters") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
  
# https://r-graph-gallery.com/package/ggiraph.html


int_map <- ggplot() +
  geom_sf(data = link_collection, aes(color = length), alpha = 0.5) + 
  geom_sf_interactive(
    data = hulls, 
    aes(fill = cluster, data_id = cluster, tooltip = paste("Cluster:", cluster)), 
    lwd = 0.05, alpha = 0.5, color = "white") +
  geom_sf(data = occs,  color = "darkorchid3") 
  labs(title = "DBSCAN Clusters") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
  
  
  
  htmltools::save_html(interactive_plot, "../plots/ggiraph-6.html")

  
  hulls <- mutate(hulls, 
         Area = st_area(hulls), .before = geometry)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # we will add a legend which indicates what proportion of the population, by area, 
  # is in each cluster. 
  
  
  legend <- ggplot(hulls, aes(group_hdi, share, fill = group_hdi)) +
    geom_col_interactive(aes(data_id = data_id, tooltip = paste("Share:", label))) +
    geom_hline(yintercept = 0) +
    geom_text_interactive(
      aes(y = y_text, label = label, color = group_hdi, data_id = data_id),
      size = 2
    ) +
    coord_flip() +
    scale_x_discrete(labels = x_labels) +
    scale_fill_brewer(palette = "YlGnBu") +
    scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
    guides(fill = "none", color = "none") +
    labs(
      title = "",
      x = NULL,
      y = NULL
    ) +
    theme_void() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 8), # Reduced title size
      axis.text.y = element_text(size = 5), # Reduced y-axis text size
      axis.text.x = element_blank(),
      aspect.ratio = 1.5
    )
  
  
  
  p_hdi_atlas <- pmap + pcol + plot_layout(widths = c(3, 1))
  p_hdi_atlas <- pmap + inset_element(pcol, left = 0.5, bottom = 0, right = 1, top = 0.5)
  
  interactive_plot <- ggiraph:: girafe(
    ggobj = int_map,
    options = list(
      opts_hover(css = "fill:orange;"),
      opts_hover_inv(css = "opacity:0.5;"),
      opts_selection(type = "single", only_shiny = FALSE)
    )
  )
  
  htmltools::save_html(interactive_plot, "../plots/ggiraph-6.html")
  
  