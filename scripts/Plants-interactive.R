setwd('~/Documents/assoRted/Mutualism-cost-benefit-outreach/scripts')
source('functions.R')

library(sf)
library(ggiraph)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

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
  cluster = fpc::dbscan(st_coordinates(occs), eps = 110, MinPts = 3)$cluster, 
  .before = geometry) 
  
# create concave hulls to encapsulate our clusters. 
hulls <- occs %>% 
   # filter(cluster != 0) %>% 
    st_buffer(50) %>% 
    group_by(cluster) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_concave_hull(allow_holes = FALSE, ratio = 0.2) %>% 
    mutate(cluster = cluster)  


hulls <- smoothr::smooth(hulls, method = "ksmooth")
link_collection <- graphLines(occs, d2_dist = 500) |>
  filter(length > 100 & length < 400)

# we will show what estimated proportion of the population is within each hull. 

clust_ct <- occs |>
  group_by(cluster) |>
  st_drop_geometry() |>
  mutate(Count = if_else(Count==0, 1, Count)) |>
  summarize(Total_pls = sum(Count))

hulls <- left_join(hulls, clust_ct) |>
  mutate(
    cluster = if_else(cluster==0, 'none', as.character(cluster))
  )

rm(clust_ct)


# these will plot in order by the putative deme with the most plants. 
# plants not assigned to a deme must be plotted last
ord <- hulls[ order(hulls$Total_pls), 'cluster'] |> pull(cluster)
ord <- c(
  ord[which(ord=='none')], 
  ord[which(ord!='none')]
  )

hulls <- mutate(hulls, cluster = factor(cluster, ord))
hulls1 <- filter(hulls, cluster != 'none')
  


# https://r-graph-gallery.com/package/ggiraph.html

int_col <- 
  ggplot(data = st_drop_geometry(hulls), aes(Total_pls, y = cluster, fill = cluster)) +
  scale_fill_brewer(palette = "Set3", direction = - 1) +
  geom_col_interactive(
    aes(data_id = cluster, tooltip = paste0("Deme: ", cluster, ', ', Total_pls, ' plants'))) + 
  geom_text_interactive(
    aes(label = Total_pls), color = '#57ACDC', size = 2,
  )  + 
  labs(x = 'No. Plants', y = 'Deme', title = 'Total number of plants\ncounted per deme') + 
  theme_minimal() + 
  xlim(0, max(hulls$Total_pls)*1.2) + 
  theme(
    legend.position = 'none', 
    aspect.ratio = 2/1.25,
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm")),
    axis.title = element_text(colour = '#276BB0', size = 7), 
    axis.text = element_text(colour = '#57ACDC', size = 7),
    plot.title  = element_text(colour = '#272AB0', size = 8, hjust = 0.5),
    panel.grid.minor = element_line(size = 0.01), 
    panel.grid.major = element_line(size = 0.02)
    )

int_map <- ggplot() +
  geom_sf(data = link_collection, aes(color = length), alpha = 0.5, lwd = 0.1) + 
  geom_sf(data = occs,  color = "#9C27B0") +
  geom_sf_interactive(
    data = hulls1, 
    aes(fill = cluster, data_id = cluster, tooltip = paste("Deme:", cluster)), 
    lwd = 0.05, alpha = 0.6, color = "white") +
  
  scale_fill_brewer(palette = "Set3", direction = - 1) +
  labs(title = "Possible genetic neighborhoods (demes) of\nCoast Goldenrod at Lanphere & Ma-le'l Dunes") +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5, colour = '#272AB0')
    )
  

blurb = paste(
  "Each point represents a plot where Coast Goldenrod (Solidago \n",
  "spathulata) plants were observed. Nearly all populations may be\n",
  "further subdivided into genetic neighborhoods, or demes, each\n",
  "compromised of more closely related individuals. However, the\n",
  "identification of demes requires molecular approaches, which\n",
  "while becoming cheaper and faster to implement, still have\n",
  "significant bottlenecks to high throughput usage in wildland\n",
  "settings. On the other hand the rapid development of\n",
  "computational capacities and algorithms offer an\n",
  "alternative approach which can be rapidly scaled\n",
  "for the more than 350,000 vascular plant species\n", 
  "around the globe.\n",
  "\n",
  "Using the distances regularly travelled by insect\n", 
  "pollinators, the portions of a landscape which\n",
  "have the focal plant species growing in them\n", 
  "can be clustered into provisional demes.\n",
  "\n", 
  "In this project we use field and LiDAR data\n", 
  "alongsides several machine learning\n",
  "methods to estimate the abundance of Coast\n",
  "Goldenrod across a landscape in Northern\n",
  "California. We then use these estimates to\n",
  "gauge how the populations of solitary\n", 
  "bee which interacts with the Coast\n", 
  "Goldenrod may be limited by the\n",
  "availabilty of this species abundance"
  )


explan <- ggplot()   + 
 theme_void() + 
  xlim(0, 5) + 
  annotate("text", x = 0, y = 0, size=2, label = blurb, hjust = 0, color = '#57ACDC')


blurb1 = paste(
  "Here we have naively clusted portions\n",
  "of this population using DBSCAN set\n",
  "to an eps of 110 meters. The edges\n",
  "between the nodes cover distances\n",
  "of 100-400m. While the entire\n",
  "population covers a larger expanse, just\n", 
  "a few clusters contain most individuals.\n", 
  "For applying ex situ conservation\n", 
  "these clusters may prove the most viable\n", 
  "to sample in order to obtain an adequate\n",
  "amount of a populations allelic diversity"
)

blurb1 <- ggplot()   + 
    theme_void() + 
    xlim(0, 5) + 
    annotate("text", x = 5, y = 0, size=2, label = blurb1, hjust = 1, color = '#57ACDC')
  
# so the number of rows counts DOWN from the top....i.e. top is 0 ,bottom
# can be set anywhere, but the fn doesn't really seem to respect floats (?)
# so run as ints. 

# to deal with aspect ratios, the ranges of top and bottom should be UNEQUAL
# for the long plots here, we need about 3:1
layout <- c(
  area(t = 0, l = 0, b = 75, r = 25), # the map
  area(t = 40, l = 10, b = 65, r = 26), # the bars 
  area(t = 0, l = 1, b = 60, r = 13), # big text left
  area(t = 2, l = 16, b = 38, r = 25)
)

p_hdi_atlas <- int_map + int_col + explan + blurb1 +
  plot_layout(design = layout) 
p_hdi_atlas


tooltip_css <- "
  background: linear-gradient(145deg, #f0f0f0, #e6e6e6);
  border: none;
  border-radius: 12px;
  box-shadow: 3px 3px 10px #d1d1d1, -3px -3px 10px #ffffff;
  color: #333;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  font-size: 52px;
  padding: 12px;
  transition: all 0.5s ease-out;
"

interactive_plot <- girafe(
  ggobj = p_hdi_atlas,
  options = list(
    opts_hover(css = "fill:orange;"),
    opts_hover_inv(css = "opacity:0.5;"),
    opts_selection(type = "single", only_shiny = FALSE), 
    opts_tooltip(css = tooltip_css)
  )
)

htmltools::save_html(interactive_plot, "../index.html")
