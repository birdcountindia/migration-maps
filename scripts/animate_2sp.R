gg_migrate <- function(
    spec1, spec2, pos_gr, pos_im, name_col, dot_size, 
    SHOW.FREQ, min_lat, min_long, max_lat, max_long,
    window_days = 30, step_days = 3     
){
  
  require(tidyverse)
  require(sf)
  require(magick)
  require(cowplot)
  require(gifski)
  require(extrafont)
  require(tictoc)
  require(progress)
  
  # Settings
  plot_res <- 100; plot_fps <- 10; 
  plot_col1 <- "#5e488a"; plot_col2 <- "#449966"

  # Define and Create Temporary Directory
  temp_dir <- "temp_frames"
  if (!dir.exists(temp_dir)) dir.create(temp_dir)
  file.remove(list.files(temp_dir, full.names = TRUE))
  
  # --- LOAD ASSETS (Original Design Prep) ---
  
  spec1_photo <- get_spec_photo() %>% filter(SPECIES == spec1)
  spec2_photo <- get_spec_photo() %>% filter(SPECIES == spec2)
  
  mugshot1 <- image_read(paste0("data/mugshots/", spec1_photo$PHOTO.FILE)) %>% 
    image_scale("200") %>% image_border(plot_col1, "8x8") %>%
    image_annotate(spec1_photo$PHOTO.CREDIT, size=18, location="+10+6", 
                   color= name_col, font="Gill Sans")
  mugshot2 <- image_read(paste0("data/mugshots/", spec2_photo$PHOTO.FILE)) %>% 
    image_scale("200") %>% image_border(plot_col2, "8x8") %>%
    image_annotate(spec2_photo$PHOTO.CREDIT, size=18, location="+10+6", 
                   color= name_col, font="Gill Sans")
  spacer1 <- image_blank(width = 216, height = 10, color = "none")
  
  mugshot <- image_append(c(mugshot1, spacer1, mugshot2), stack = TRUE)
  
  logo1 <- image_read("birdcountindia_logo.png") %>% image_resize("x45")  
  logo2 <- image_read("eBird_India_Logo.png") %>% image_resize("x45")  
  spacer2 <- image_blank(width = 10, height = image_info(logo1)$height, color = "none")
  combined_logo <- image_append(c(logo1, spacer2, logo2), stack = FALSE)   
  
  t1 <- image_blank(800, 35, "none") %>%
    image_annotate(text = paste(spec1) , font = "Gill Sans", size = 30, color = plot_col1, gravity = "center", weight = 900) 
  t2 <- image_blank(800, 35, "none") %>%
    image_annotate(text = paste0(spec2), font = "Gill Sans", size = 30, color = plot_col2, gravity = "center", weight = 900) 
  
  title_card <- image_append(c(t1, t2), stack = TRUE)
  title_card
  # --- DATA PREP ---
  message(paste("Preparing data for:", spec1, "&", spec2))
  
  # Map Data (Calculated FIRST)
  data_sf <- data_spec %>% 
    filter(COMMON.NAME %in% c(spec1, spec2), YEAR >= 2020) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(india_sf)) %>% 
    st_transform(crs = "ESRI:54030")
  
# --- MAP BOUNDS CALCULATION (Dynamic 3:2 Ratio) ---
  
  if (is.na(min_long)){
    bbox <- st_bbox(data_sf)
    raw_xlim <- c(bbox["xmin"], bbox["xmax"])
    raw_ylim <- c(bbox["ymin"], bbox["ymax"])
  } else {
    bounds_sf <- data.frame(
      lon = c(min_long, max_long),
      lat = c(min_lat, max_lat)
    ) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
      st_transform(crs = "ESRI:54030")
    bounds_coords <- st_coordinates(bounds_sf)
    raw_xlim <- range(bounds_coords[,"X"])
    raw_ylim <- range(bounds_coords[,"Y"])
  }
  
  # 2. Add 10% Padding
  x_range <- diff(raw_xlim) * 1.2
  y_range <- diff(raw_ylim) * 1.2
  mid_x <- mean(raw_xlim)
  mid_y <- mean(raw_ylim)
  
  # 3. Enforce 3:2 Aspect Ratio (1.5) by EXPANDING only
  target_ratio <- 1.5 # (10.5 / 7)
  current_ratio <- x_range / y_range
  
  if (current_ratio > target_ratio) {
    # Too wide? Increase height to match
    y_range <- x_range / target_ratio
  } else {
    # Too tall? Increase width to match
    x_range <- y_range * target_ratio
  }
  
  # 4. Final Limits
  xmin <- mid_x - x_range/2
  xmax <- mid_x + x_range/2
  ymin <- mid_y - y_range/2
  ymax <- mid_y + y_range/2
# ---------------------------------------------------------
  # Freq Data
  tic("Smoothing frequency")
  raw_freq <- calc_repfreq_IN(data_IN, spec1) %>%
    bind_rows(calc_repfreq_IN(data_IN, spec2))
  species_list <- c(spec1, spec2)
  
  freq_smooth <- raw_freq %>%
    filter(SPECIES %in% species_list,PERIOD == "DAY.Y") %>%
    rename(DAY.Y = NUMBER) %>%
    bind_rows(mutate(., DAY.Y = DAY.Y + 365), bind_rows(mutate(., DAY.Y = DAY.Y - 365))) %>%
    group_by(SPECIES) %>%
    nest() %>%
    mutate(model = map(data, ~ loess(REP.FREQ ~ DAY.Y, data = ., span = 0.25)),
           pred_y = map(model, ~ predict(., newdata = data.frame(DAY.Y = 1:365)))) %>%
    unnest(pred_y) %>%
    mutate(DAY.Y = 1:365, REP.FREQ = ifelse(pred_y < 0, 0, pred_y)) |> 
    ungroup()
  
  toc ()

  max_freq <- max(freq_smooth$REP.FREQ, na.rm = TRUE)
  month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")  
  # --- LOOP ---
  starts <- c(seq(101, 365, step_days), seq(1, 100, step_days))
  pb <- txtProgressBar(0, length(starts), style=3)
  
  
  for (k in seq_along(starts)) {
    i <- starts[k]; if (k%%10==0) gc(verbose=F)
    
    days <- c(1:365, 1:window_days)[i:(i + window_days - 1)]
    mid <- c(1:365, 1:window_days)[i + round(window_days/2)]
    
    # Map
    p_map <- basemap + 
      geom_sf(data = filter(data_sf, DAY.Y %in% days), 
        aes(color = COMMON.NAME), size = dot_size, alpha = 0.5, stroke = 0) +
        scale_color_manual(values = c(plot_col1, plot_col2)) + 
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
      theme(legend.position = "none")
    
    # Graph 
    p_graph <- ggplot(freq_smooth, aes(DAY.Y, REP.FREQ)) +
      geom_line(aes(color = SPECIES), linewidth=1.5, show.legend = FALSE) +
      scale_color_manual(values = c(plot_col1, plot_col2)) + 
      geom_vline(xintercept=mid,color = "black", linewidth=0.5) +
      scale_x_continuous(breaks=month_breaks, labels=paste0("\n", month_labels)) +
      scale_y_continuous(limits = c(0, max_freq*1.1),expand = expansion(mult = c(0.02, 0.02)))+
      labs(title=paste0("Frequency in India (max. ", round(max_freq), "%)")) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "plain", margin = margin(t=5, b=5)),
        plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        axis.text.x = element_text(face = "plain", size = 12, color = "black", vjust=2) 
      )
    
    final <- if (SHOW.FREQ == 1) {
      ggdraw(p_map) +
        draw_plot(p_graph,
                  x = if (pos_gr == "R") 0.68 else 0.02, y = 0.02, width = 0.3, height = 0.2)
    } else {
      label <- format(as.Date(mid-1, origin="2023-01-01"), "%B")
      ggdraw(p_map)+
        annotate("text", 
                 x = if (pos_gr == "R") 0.8 else 0.02, y = 0.05, 
                 label = label,
                 size = 8, fontface = "bold", color = "black", hjust = 0.5)}
    
    ggsave(sprintf("%s/frame_%03d.png", temp_dir, k), final, width=10.5, height=7, dpi=plot_res, device="png")
    setTxtProgressBar(pb, k)
  }
  close(pb)
  
  # --- STITCHING ---
  message("Stitching...")
  frames <- image_read(list.files(temp_dir, full.names=T))
  mug_off <- if(pos_im=="L") "+20+20" else "+880+20"
  
  final <- frames %>%
    image_composite(image_scale(mugshot, "150"), offset=mug_off) %>%
    image_composite(image_scale(title_card, "800"), gravity="north") %>%
    image_composite(image_scale(combined_logo, "x45"), offset = paste0("+", if(pos_gr == "R") 20 else 360, "+635")) %>%
    image_animate(fps=plot_fps)
  image_write(final, paste0("outputs/", spec1,"&", spec2, "Migration Map.gif"))
  unlink(temp_dir, recursive=T)
  message("Done.")
}