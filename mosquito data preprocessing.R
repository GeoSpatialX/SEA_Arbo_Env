#                              Parts of the code are revised from the teaching materials from Prof.Junying Lim from National University of Singapore (BL5324-Ecological Perspectives on Global Change)                              

library(terra)
library(geodata)
library(raster)
library(tidyverse)
library(reshape2)
library(sf)
library(ggplot2)
library(rgbif)
library(rgdal)
library(CoordinateCleaner)

global <- read.csv("mosquito_record.csv") # please define the specific mosquito record you want to preprocess

sea <- read.csv("mosquito_record_sea.csv") # please define the specific mosquito record in southeast asia that you want to preprocess

global_flag <- clean_coordinates(global, lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 tests = c("capitals", "centroids",
                                           "equal", "gbif",
                                           "institutions", "seas",
                                           "zeros"))

sea_flag <- clean_coordinates(sea, lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              tests = c("capitals", "centroids",
                                        "equal", "gbif",
                                        "institutions", "seas",
                                        "zeros"))


global_clean <- global[
  global_flag$.equ == TRUE &
    global_flag$.zer == TRUE &
    global_flag$.cap == TRUE &
    global_flag$.cen == TRUE &
    global_flag$.sea == TRUE &
    global_flag$.gbf == TRUE & global_flag$.inst == TRUE,]


sea_clean <- sea[
  sea_flag$.equ == TRUE &
    sea_flag$.zer == TRUE &
    sea_flag$.cap == TRUE &
    sea_flag$.cen == TRUE &
    sea_flag$.sea == TRUE &
    sea_flag$.gbf == TRUE & sea_flag$.inst == TRUE,]


sea_clean_idx <-  sample(1:nrow(sea_clean),8*nrow(sea_clean)/10)

sea_clean_trn <-  sea_clean[sea_clean_idx,]

sea_clean_tst <-  sea_clean[-sea_clean_idx,]

sea_id <- sea_clean_tst$gbifID

newglobal <- global_clean %>% filter(! gbifID %in% sea_id )

write.csv(newglobal,file = "training.csv")
write.csv(sea_clean_tst,file = "validation.csv")