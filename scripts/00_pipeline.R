library(tidyverse)
library(glue)
library(skimmr) # for soib dataset
library(purrr)
library(readxl)

source("scripts/functions.R")
# full list species to be processed------------------------------------------------
sp_ls <- read_xlsx("data/species_mapping.xlsx", sheet = 1)
sp_ls <- sp_ls |>
  filter(print == 1) |>
  mutate(BATCH = ceiling(row_number() / 50))

#Use this if you want to print India only or secondary maps
#sp_ls <- sp_ls |> filter(print == 2) |> 
#mutate(SPECIES1 = str_remove(SPECIES1, "_IN"),SPECIES2 = str_remove(SPECIES2, "_IN"))

# # import and process data
# uncomment this if new data has to be processed.

#source("scripts/01_import_data.R") 
load("data/01_IN_data.RData")

# create world basemap
basemap <- gg_world()

load(paste0("data/spec_data_batch.RData"))
load("../india-maps/outputs/maps_sf.RData", envir = .GlobalEnv)

sp_ls <- sp_ls %>% slice (71,74,63)
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
  print <- sp_ls$print[i]
  
  # NA → NULL for two-species check
  if (is.na(spec2)) spec2 <- NULL
  
  if (is.null(spec2)) {
    source("scripts/animate_1sp.R")
    gg_migrate(spec1, pos_gr, pos_im, name_col, dot_size, 
               SHOW.FREQ, min_lat, min_long, max_lat, max_long, print)
    
  } else {
    source("scripts/animate_2sp.R")
    gg_migrate(spec1, spec2, pos_gr, pos_im, name_col, dot_size, 
               SHOW.FREQ, min_lat, min_long, max_lat, max_long, print)
  }
})
