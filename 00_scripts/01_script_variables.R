#' ---
#' title: euterpe hybridization
#' author: mauricio vancine
#' date: 2024-03-19
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(sf)
library(terra)
library(tmap)
library(CoordinateCleaner)

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# source
source("https://raw.githubusercontent.com/annack84/STMdevelopment/main/R/calc_soil_trapezoidal.R")

# import data -------------------------------------------------------------

# vector
neo <- sf::st_read("01_data/02_variables/00_limit/neotropic_dissolved_fill_holes.shp") %>% 
    dplyr::mutate(FID = 1)
neo
plot(neo, col = "gray")

# raster
wc <- dir(path = "01_data/02_variables/01_climate/raw/", pattern = "bio", full.names = TRUE) %>% 
    .[c(1, 12:19, 2:11)] %>% 
    terra::rast()
wc

sr <- dir(path = "01_data/02_variables/01_climate/raw/", pattern = "srad", full.names = TRUE) %>% 
    terra::rast()
sr

soil <- dir(path = "/media/mude/afe69132-ffdb-4892-b809-a0f7d2b8f423/spatial_data_base/02_raster/soilgrids/soil_world", 
            pattern = "0-5cm|5-15cm|15-30cm", full.names = TRUE) %>% 
    stringr::str_subset(pattern = "bdod|phh2o|clay|sand|soc") %>% 
    terra::rast()
soil

# crop
wc_neo <- terra::crop(wc, neo) %>% 
    terra::mask(neo)
names(wc_neo) <- c(paste0("bio", 1:9), paste0("bio1", 0:9))
wc_neo
plot(wc_neo[[1]])

sr_neo <- terra::crop(sr, neo) %>% 
    terra::mask(neo)
sr_neo <- terra::app(sr_neo, mean, cores = 10)
sr_neo
plot(sr_neo)

soil_neo <- terra::crop(soil, neo) %>% 
    terra::mask(neo)
soil_neo
plot(soil_neo[[1]])

soil_neo_25m <- terra::resample(soil_neo, sr_neo)
soil_neo_25m

plot(sr_neo)
plot(soil_neo_25m[[1]])

soil_neo_25m_depth_mean <- NULL
for(i in c("bdod", "phh2o", "clay", "sand", "soc")){
    
    soil_neo_25m_depth_mean_i <- terra::app(soil_neo_25m[[grep(i, names(soil_neo_25m))]][[c(1, 3, 2)]], mean, na.rm = TRUE, cores = 10)
    soil_neo_25m_depth_mean <- c(soil_neo_25m_depth_mean, soil_neo_25m_depth_mean_i)
    
}

soil_neo_25m_depth_mean <- terra::rast(soil_neo_25m_depth_mean)
names(soil_neo_25m_depth_mean) <- c("bdod", "phh2o", "clay", "sand", "soc")
soil_neo_25m_depth_mean

# export
writeRaster(wc_neo, "01_data/02_variables/01_climate/wc2.1_2.5m_bio.tif")
writeRaster(sr_neo, "01_data/02_variables/01_climate/wc2.1_2.5m_srad_mean.tif")
writeRaster(soil_neo_25m_depth_mean, "01_data/02_variables/02_soil/soil_neo_2_5m_depth_mean.tif")

# end ---------------------------------------------------------------------
