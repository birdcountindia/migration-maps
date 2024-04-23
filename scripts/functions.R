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



# calculate reporting frequency in India as a whole ---------------------------------

# outputs list

calc_repfreq_IN <- function(data, species, mig_status) {
  
  # filter data to only those grids and seasons where species reported
  data_mig <- data %>%
    filter(COMMON.NAME == species) %>%
    { if (migstatus %in% c("S","W","P")){
      distinct(GRID.G3) 
    } else if (migstatus == "LM") {
      distinct(GRID.G3, SEASON)
    } else {
      .
    }} %>% 
    left_join(data)
  
  
  # daily freq
  totdays <- tibble(DAY.Y = 1:365)
  
  data_freq_day <- map_df(totdays$DAY.Y, ~ {
    
    # filtering data for day of interest
    data_day <- data_mig %>% filter(DAY.Y == .x)
    
    # calculate species-detected and total checklists for that day
    temp1 <- data_day %>% 
      filter(COMMON.NAME == species) %>% 
      reframe(DET.DAY = n_distinct(GROUP.ID))

    temp2 <- data_day %>% 
      reframe(LISTS.DAY = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS.DAY = case_when(LISTS.DAY == 0 ~ 1,
                                  TRUE ~ LISTS.DAY))

    to_return <- tibble(DAY.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      mutate(REP.FREQ.DAY = 100*DET.DAY/LISTS.DAY)
    
    return(to_return)
    
  })
  
  
  # fortnightly freq
  totforts <- tibble(FORT.Y = 1:27)
  
  data_freq_fort <- map_df(totforts$FORT.Y, ~ {
    
    # filtering data for day of interest
    data_fort <- data_mig %>% filter(FORT.Y == .x)
    
    # calculate species-detected and total checklists for that day
    temp1 <- data_fort %>% 
      filter(COMMON.NAME == species) %>% 
      reframe(DET.FORT = n_distinct(GROUP.ID))
    
    temp2 <- data_fort %>% 
      reframe(LISTS.FORT = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS.FORT = case_when(LISTS.FORT == 0 ~ 1,
                                  TRUE ~ LISTS.FORT))
    
    to_return <- tibble(FORT.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      mutate(REP.FREQ.FORT = 100*DET.FORT/LISTS.FORT)
    
    return(to_return)
    
  })
  
  
  # return both
  return(list(data_freq_day, data_freq_fort))

}



