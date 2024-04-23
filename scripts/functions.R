
# world basemap ---------------------------------------------------------------------

gg_world <- function() {
  
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(ggplot2)
  require(sf)
  
  
  world_sf <- ne_countries(type = "countries", 
                           scale = "medium", returnclass = "sf") %>% 
    # selecting columns of interest
    reframe(NAME = sovereignt, 
            geometry = geometry) %>% 
    st_as_sf()
  
  # replacing default India with Indian India boundaries
  world_sf <- world_sf %>% 
    filter(NAME != "India") %>% 
    # since the row is at the end, will lay over other disputed territories
    bind_rows(india_sf %>% 
                dplyr::select(geometry) %>% 
                mutate(NAME = "India"))
  
  world_sf <- world_sf %>% 
    # converting to Robinson projection
    st_transform(crs = "ESRI:54030") %>% 
    # fill colours and line widths
    mutate(FILL = case_when(NAME == "India" ~ "#ffffff",
                            TRUE ~ "#e5d8ca"),
           LINEWIDTH = case_when(NAME == "India" ~ 0.65,
                                 TRUE ~ 0.5))
  
  
  world_gg <- ggplot(world_sf) +
    geom_sf(aes(fill = FILL, linewidth = LINEWIDTH)) +
    scale_fill_identity() +
    scale_linewidth_identity() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "#a9d5e0", colour = NA))
  
  return(world_gg)
  
}

