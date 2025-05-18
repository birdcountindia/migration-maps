library(tidyverse)
library(glue)
library(skimmr) # for soib dataset

source("scripts/functions.R")


###


# # import and process data
# source("scripts/01_import_data.R")
load("data/01_import_data.RData")


# create world basemap
basemap <- gg_world()


# single-species terrestrial species ------------------------------------------------

gg_migrate("Amur Falcon")

