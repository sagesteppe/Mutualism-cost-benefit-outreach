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
    filter(cluster != 0) %>% 
    st_buffer(50) %>% 
    group_by(cluster) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_concave_hull(allow_holes = FALSE, ratio = 0.2) %>% 
    mutate(cluster = as_factor(cluster))  


hulls <- smoothr::smooth(hulls, method = "ksmooth")

link_collection <- graphLines(occs, d2_dist = 500) |>
  filter(length > 100 & length < 400)



# https://r-graph-gallery.com/package/ggiraph.html


ggplot() +
  geom_sf(data = link_collection, aes(color = length), alpha = 0.5) + 
 # geom_sf(data = hulls, aes(fill = cluster), alpha = 0.5) +
  geom_sf_interactive(
    data = hulls, 
    aes(fill = cluster, data_id = cluster, tooltip = paste("Cluster:", cluster)), 
    lwd = 0.05, alpha = 0.5, color = "white") +
  geom_sf(data = occs,  color = "darkorchid3") 
  labs(title = "DBSCAN Clusters") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

  
  
  