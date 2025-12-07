library(tidyverse)
library(glue)
library(skimmr) # for soib dataset
library(purrr)
library(readxl)

source("scripts/functions.R")

# # import and process data
# uncomment this if new data has to be processed.

#source("scripts/01_import_data.R") 
load("data/01_import_data.RData")
load("../india-maps/outputs/maps_sf.RData", envir = .GlobalEnv)

# create world basemap
basemap <- gg_world()


# species ------------------------------------------------
sp_ls <- read_xlsx("data/species_mapping.xlsx", sheet = 1)
#sp_ls <- sp_ls %>% slice(53)
#sp_ls <- sp_ls %>% slice(1:6, 16)

#execution------------------------------------------------ 
purrr::walk(seq_len(nrow(sp_ls)), function(i) {
  
  spec1     <- sp_ls$SPECIES1[i]
  spec2     <- sp_ls$SPECIES2[i]
  pos_gr  <- sp_ls$pos_gr[i]
  pos_im  <- sp_ls$pos_im[i]
  name_col <- sp_ls$name_col[i]
  dot_size <- sp_ls$dot_size[i]
  SHOW.FREQ <- sp_ls$SHOW.FREQ[i]
  min_lat <- sp_ls$min_lat[i]
  min_long <- sp_ls$min_long[i]
  max_lat <- sp_ls$max_lat[i]
  max_long <- sp_ls$max_long[i]
  
  
  # NA → NULL for two-species check
  if (is.na(spec2)) spec2 <- NULL
  
  if (is.null(spec2)) {
    source("scripts/animate_1sp.R")
    gg_migrate(spec1, pos_gr, pos_im, name_col, dot_size, 
               SHOW.FREQ, min_lat, min_long, max_lat, max_long)
    
  } else {
    source("scripts/animate_2sp.R")
    gg_migrate(spec1, spec2, pos_gr, pos_im, name_col, dot_size, 
               SHOW.FREQ, min_lat, min_long, max_lat, max_long)
  }
})
