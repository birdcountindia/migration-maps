library(tidyverse)
library(glue)

source("scripts/functions.R")


###


# import and process data
source("scripts/01_import_data.R")


load("data/01_import_data.RData")

# create world basemap
basemap <- gg_world()


# calculate repfreq
data_rf <- calc_repfreq_IN(data_IN, "Amur Falcon", "P") %>% 
  bind_rows(calc_repfreq_IN(data_IN, "Blyth's Reed Warbler", "W"))
