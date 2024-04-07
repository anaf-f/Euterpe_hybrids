#' ---
#' title: euterpe hybridization
#' author: mauricio vancine
#' date: 2024-03-19
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(terra)
library(tmap)

# import data -------------------------------------------------------------

# occs
occs <- readr::read_csv("01_data/01_occurrences/02_cleaned/occ_euterpe_cleaned_edited.csv")
occs

occs_v <- sf::st_as_sf(occs, coords = c("longitude", "latitude"), crs = 4326) %>% 
    terra::vect()
occs_v

# variables
soil <- terra::rast("01_data/02_variables/soil_neo_2_5m_depth_mean.tif")
soil

# data
occs_v_soil <- terra::extract(soil, occs_v) %>% 
    dplyr::bind_cols(occs, .) %>% 
    dplyr::select(species, bdod, phh2o, clay, sand, soc) %>% 
    tidyr::pivot_longer(cols = -species, names_to = "variables", values_to = "values") %>% 
    dplyr::mutate(species = stringr::str_replace_all(species, "Euterpe", "E."))
occs_v_soil

# plot
ggplot(occs_v_soil, aes(x = species, y = values, fill = species)) +
    geom_violin() +
    geom_boxplot(color = "gray", fill = NA) +
    scale_fill_manual(values = c("forestgreen", "#922fa7", "#5b3caf")) +
    facet_wrap(~variables, scales = "free") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(size = 15, face = "italic"))
ggsave("02_results/soil/fig_extract_soil_points.png", width = 40, height = 20, units = "cm", dpi = 300)

terra::writeVector(occs_v, "01_data/01_occurrences/02_cleaned/occ_euterpes.shp")
