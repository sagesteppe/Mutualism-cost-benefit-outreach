#' return the links between nodes in a network at specified distances as sf linestrings
#' 
#' @description Create a spatial network using spdep::dnearneigh and record the links
#' between nodes as linestring objects for use on an sf plot. 
#' @param d2_dist maximum distance to search for neighbors in, defaults to 500m. 
graphLines <- function(x, d2_dist){
  
  if(missing(d2_dist)){d2_dist <- 500}
  
  dat1 <- data.frame(
    cbind(
      1:nrow(x), 
      st_coordinates(x)
    )
  ) |>
    setNames(c('quad', 'longitude', 'latitude'))
  
  neighborhood_object <- spdep::dnearneigh(x, d1 = 1, d2 = d2_dist) 
  
  links <- neighborhood_object %>% 
    # unlist all of the neighborhoods within the distance seached by
    # `dnearneigh` this gets the ID of each point which is a member. 
    purrr::map(., as.integer) %>% 
    purrr::map(., as_tibble) %>% 
    
    # now recover each neighbor of each row  
    map2_df(1:nrow(x), ~ mutate(.x, Focal_id = .y)) %>% 
    rename(Neighbor = value) %>% 
    filter(Neighbor > 0) %>% 
    
    # now we join the focal points coordinates on 
    left_join(., dat1, by = c('Focal_id' = 'quad'))  %>% 
    rename( 
      longitude.id = longitude,
      latitude.id = latitude) %>% 
    
    # now join the coordinates for each neighboring pt in the neighborhood
    left_join(., dat1, by = c('Neighbor' = 'quad')) %>% 
    rename(
      longitude.nb = longitude,
      latitude.nb = latitude)  %>% 
    
    drop_na() %>% 
    mutate(Link = as.numeric(paste0(Focal_id, Neighbor)))
  
  links <- as.matrix(links)
  ex_linestring <- list(0)
  for (i in 1:nrow(links)){
    ex_linestring[[i]] <- rbind(
      c(links[i, 3], links[i, 4]), 
      c(links[i, 5], links[i, 6]),
      c(links[i, 3], links[i, 4])
    )
  }
  
  ex_linestring <- lapply(ex_linestring, st_linestring)
  
  link_collection <- ex_linestring %>%
    st_sfc(crs = 32610) %>%
    st_sf(geometry = .)
  
  link_collection <-  link_collection %>% 
    mutate(length = as.numeric(st_length(link_collection))) 
  
  return(link_collection)
  
}
