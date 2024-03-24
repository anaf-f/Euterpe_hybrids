#' ---
#' title: euterpe hybridization
#' author: mauricio vancine
#' date: 2024-03-18
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

# import data -------------------------------------------------------------

# species list
species_list <- c("Euterpe edulis", "Euterpe oleracea", "Euterpe precatoria")
species_list

# import south america limite
li <- spData::world
li

# sibbr ----------------------------------------------------------------

# download
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_01",
#               destfile = "01_data/01_occurrences/00_raw/sibbr_plantas_vasc01.zip",
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_02",
#               destfile = "01_data/01_occurrences/00_raw/sibbr_plantas_vasc02.zip",
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_03",
#               destfile = "01_data/01_occurrences/00_raw/sibbr_plantas_vasc03.zip",
#               mode = "wb")
# download.file(url = "https://ipt.sibbr.gov.br/sibbr/resource?r=sibbr_plantas_vasc_04",
#               destfile = "01_data/01_occurrences/00_raw/sibbr_plantas_vasc04.zip",
#               mode = "wb")

# import
occ_data_sibbr_plants_files <- dir(path = "01_data/01_occurrences/00_raw/sibbr", pattern = ".txt$", full.names = TRUE, recursive = TRUE)
occ_data_sibbr_plants_files

occ_data_sibbr <- purrr::map_dfr(occ_data_sibbr_plants_files, readr::read_delim, delim = "\t") %>% 
    dplyr::mutate(species = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  source = "sibbr",
                  year = as.numeric(year)) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(species, longitude, latitude, source, year) %>% 
    dplyr::filter(species %in% species_list) %>% 
    dplyr::filter(longitude > -180, 
                  longitude < 180,
                  latitude > -90,
                  latitude < 90)
occ_data_sibbr

readr::write_csv(occ_data_sibbr, "01_data/01_occurrences/00_raw/occ_raw_sibbr.csv")

# data paper --------------------------------------------------------------

## atlantic frugivory ----
fru_flora_occ <- readr::read_csv("01_data/01_occurrences/00_raw/data_paper/ATLANTIC_frugivory.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(species = plant_species,
                  year = NA,
                  source = "atlantic_frugivory") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% species_list) %>% 
    tidyr::drop_na(longitude, latitude)
fru_flora_occ

## atlantic flower–invertebrate interactions ----
flo_inv_occ_flora <- readr::read_csv("01_data/01_occurrences/00_raw/data_paper/AtlanticForestInvertFloInteractionData_2022-07.csv") %>%
    janitor::clean_names() %>% 
    dplyr::mutate(species = plant_species,
                  longitude = longitude_x, 
                  latitude = latitude_y,
                  year = campain_year_finish,
                  source = "atlantic_flower_invertebrate") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% species_list) %>% 
    tidyr::drop_na(longitude, latitude)
flo_inv_occ_flora

occ_data_papers <- dplyr::bind_rows(flo_inv_occ_flora, fru_flora_occ)
occ_data_papers

readr::write_csv(occ_data_papers, "01_data/01_occurrences/00_raw/occ_raw_data_papers.csv")

# portal biodiversidade ----
list_files_portalbio <- dir(path = "01_data/01_occurrences/00_raw/portal_biodiversidade/", pattern = ".csv", recursive = TRUE, full.names = TRUE) %>% 
    stringr::str_subset(pattern = "portalbio_export")
list_files_portalbio

occ_portalbio <- readr::read_csv("01_data/01_occurrences/00_raw/portal_biodiversidade/portalbio_one_species.csv")
for(i in list_files_portalbio){
    
    occ_portalbio_i <- readr::read_delim(i, delim = ";", col_types = cols()) %>% 
        janitor::clean_names() %>%
        dplyr::mutate(species = especie,
                      longitude = as.numeric(longitude),
                      latitude = as.numeric(latitude),
                      year = lubridate::year(lubridate::dmy(data_do_registro)),
                      source = "portalbio") %>% 
        dplyr::select(species, longitude, latitude, year, source)
    occ_portalbio <- dplyr::bind_rows(occ_portalbio, occ_portalbio_i)
    
}
occ_portalbio

# filter
occ_portalbio_flora <- occ_portalbio %>% 
    dplyr::filter(species %in% species_list)
occ_portalbio_flora

# export
readr::write_csv(occ_portalbio_flora, "01_data/01_occurrences/00_raw/occ_raw_portalbio.csv")


# specieslink, spocc and bien ----

# download
for(i in species_list){
    
    ## species
    print(i)
    
    ## specieslink ----
    # information
    print("splink")
    
    # download
    occ_splink <- jsonlite::fromJSON(
        paste0("https://specieslink.net/ws/1.0/search?scientificname=", 
               tolower(gsub(" ", "+", paste0(stringr::str_split(i, " ", simplify = TRUE)[, 1:2], collapse = " "))), 
               "&apikey=aXGEJtnQW12sPuSyKMX7&offset=0&limit=5000"))$features$properties
    
    # conditional without data
    if(length(occ_splink) == 0){
        
        occ_splink_data <- tibble::tibble(species = i,
                                          longitude = NA,
                                          latitude = NA,
                                          source = "specieslink",
                                          year = NA)
        
        
        
        # conditional with data and year and key
    } else if("yearcollected" %in% colnames(occ_splink) & "catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species = i,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          source = "specieslink",
                          year = as.numeric(yearcollected)) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
        
        # conditional with data and year and not key
    } else if("yearcollected" %in% colnames(occ_splink) & !"catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species = i,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          source = "specieslink",
                          year = as.numeric(yearcollected)) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
        
        # conditional with data and not year and key
    } else if(!"yearcollected" %in% colnames(occ_splink) & "catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species = i,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          source = "specieslink",
                          year = NA) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
        
        # conditional with data and not year and not key
    } else if(!"yearcollected" %in% colnames(occ_splink) & !"catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species = i,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          source = "specieslink",
                          year = NA) %>% 
            dplyr::select(species, longitude, latitude, source, year)
    }
    
    
    ## spocc ----
    # information
    print("spocc")
    
    # download
    occ_spocc <- spocc::occ(query = i, 
                            from = c("gbif", "vertnet", "idigbio", "ecoengine"),
                            has_coords = TRUE,
                            limit = 1e6,
                            throw_warnings = FALSE)
    
    # data
    occ_spocc_data <- spocc::occ2df(occ_spocc)
    
    # conditional without data
    if(nrow(occ_spocc_data) == 0){
        
        occ_spocc_data <- tibble::tibble(species = i,
                                         longitude = NA,
                                         latitude = NA,
                                         source = "spocc",
                                         year = NA)
        
    # conditional without year  
    } else if(!"date" %in% colnames(occ_spocc_data)){
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          year = NA,
                          source = "spocc") %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
    # conditional with data and year
    } else{
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          year = lubridate::year(occ_spocc_data$date),
                          source = prov) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
    }
    
    ## bien ----
    
    # download
    occ_bien_data <- BIEN::BIEN_occurrence_species(species = i)
    
    # conditional without data
    if(nrow(occ_bien_data) == 0){
        
        occ_bien_data <- tibble::tibble(species = i,
                                        longitude = NA,
                                        latitude = NA,
                                        source = "bien",
                                        year = NA)
        
        # conditional without year  
    } else if(!"date_collected" %in% colnames(occ_bien_data)){
        
        occ_bien_data <- occ_bien_data %>% 
            dplyr::mutate(species = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          source = paste0("bien_", datasource),
                          year = NA) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
        # conditional with data and year
    } else{
        
        occ_bien_data <- occ_bien_data %>% 
            dplyr::mutate(species = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          source = paste0("bien_", datasource),
                          year = lubridate::year(occ_bien_data$date_collected)) %>% 
            dplyr::select(species, longitude, latitude, source, year)
        
    }
    
    ## combine data ----
    occ_data <- dplyr::bind_rows(occ_splink_data, occ_spocc_data, occ_bien_data)
    
    # export 
    readr::write_csv(occ_data, 
                     paste0("01_data/01_occurrences/00_raw/splink_spocc_bien/occ_raw_splink_spocc_bien_flora_", sub(" ", "_", tolower(i)), ".csv"))
    
}

# import
occ_data_specieslink_spocc_bien <- dir(path = "01_data/01_occurrences/00_raw/splink_spocc_bien/", 
                                       pattern = ".csv", full.names = TRUE) %>% 
    purrr::map_dfr(readr::read_csv, col_types = "cddcd")
occ_data_specieslink_spocc_bien

# export
readr::write_csv(occ_data_specieslink_spocc_bien, "01_data/01_occurrences/00_raw/occ_raw_splink_spocc_bien.csv")

# integrated data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

## specieslink and spocc ----
occ_specieslink_spocc_bien <- readr::read_csv("01_data/01_occurrences/00_raw/occ_raw_splink_spocc_bien.csv")
occ_specieslink_spocc_bien

## sibbr ----
occ_sibbr <- readr::read_csv("01_data/01_occurrences/00_raw/occ_raw_sibbr.csv")
occ_sibbr

## data papers ----
occ_data_papers <- readr::read_csv("01_data/01_occurrences/00_raw/occ_raw_data_papers.csv")
occ_data_papers

## portal biodiversidade ----
occ_portalbio <- readr::read_csv("01_data/01_occurrences/00_raw/occ_raw_portalbio.csv")
occ_portalbio

## combine ----
occ_combined <- dplyr::bind_rows(occ_specieslink_spocc_bien, occ_data_papers,
                                 occ_sibbr, occ_portalbio) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::filter(longitude > -180 & longitude < 180) %>% 
    dplyr::filter(latitude > -90 & latitude < 90)
occ_combined

occ_combined_table <- dplyr::count(occ_combined, species, source, sort = TRUE) %>% 
    dplyr::mutate(source = forcats::as_factor(source), 
                  source = forcats::fct_infreq(source), 
                  n_log = log10(n))
occ_combined_table

ggplot(occ_combined_table, aes(x = source, y = n_log, label = n)) +
    geom_col() +
    geom_text(nudge_y = .3, size = 5) +
    coord_flip() +
    facet_grid(~species, scales = "free") +
    labs(x = "Source", y = "log10") +
    theme_bw(base_size = 20)

occ_combined_v <- sf::st_as_sf(occ_combined, coords = c("longitude", "latitude"), crs = 4326)
occ_combined_v

# map
sa <- li[li$subregion %in% c("South America", "Central America"),]
tm_shape(li, bbox = sa) +
    tm_polygons() +
    tm_shape(occ_combined_v) +
    tm_bubbles(size = .4, 
               fill = "species",
               fill.legend = tm_legend(title = "Species",
                                       position = tm_pos_in("left", "bottom")))

## export ----
readr::write_csv(occ_combined, "01_data/01_occurrences/01_integrated/00_occ_raw.csv")

# cleaning data --------------------------------------------------------

# import
occ_euterpe <- readr::read_csv("01_data/01_occurrences/01_integrated/00_occ_raw.csv") %>% 
    tibble::rowid_to_column(var = "id")
occ_euterpe

## temporal ----
occ_euterpe_filter_date <- occ_euterpe %>% 
    dplyr::mutate(date_filter = ifelse(year >= 1980 & year <= 2024 | is.na(year), TRUE, FALSE))
occ_euterpe_filter_date

## precision ----
occ_euterpe_filter_date_precision <- occ_euterpe_filter_date %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE)) %>% 
    dplyr::mutate(precision_filter_year = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_euterpe_filter_date_precision

## neotropic filter -----

# vector
neo <- sf::st_read("01_data/02_variables/00_limit/neotropic_dissolved_fill_holes.shp") %>% 
    dplyr::mutate(FID = 1)
neo
plot(neo, col = "gray")

# filter
occ_euterpe_filter_date_precision_neotropic <- occ_euterpe_filter_date_precision %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    sf::st_join(neo) %>% 
    dplyr::mutate(neotropic_filter = ifelse(is.na(FID), FALSE, TRUE)) %>% 
    dplyr::select(-FID) %>% 
    sf::st_drop_geometry()
occ_euterpe_filter_date_precision_neotropic

## spatial filter --------

# raster
wc <- terra::rast("01_data/02_variables/01_climate/wc2.1_2.5m_bio_1.tif")
wc

plot(wc)

# filter
occ_euterpe_filter_date_precision_neotropic_spatial_values <- terra::extract(x = wc, 
                                                                             y = occ_euterpe_filter_date_precision_neotropic[, c("longitude", "latitude")], 
                                                                             ID = FALSE, cells = TRUE) %>% 
    tibble::as_tibble()
occ_euterpe_filter_date_precision_neotropic_spatial_values

occ_euterpe_filter_date_precision_neotropic_spatial_filter <- occ_euterpe_filter_date_precision_neotropic %>%
    dplyr::mutate(spatial_values = occ_euterpe_filter_date_precision_neotropic_spatial_values[, 1],
                  spatial_cells = occ_euterpe_filter_date_precision_neotropic_spatial_values[, 2]) %>% 
    tidyr::drop_na(spatial_values) %>% 
    dplyr::distinct(species, spatial_cells, .keep_all = TRUE) %>% 
    dplyr::mutate(spatial_filter = TRUE) %>% 
    dplyr::select(c(id, spatial_filter))
occ_euterpe_filter_date_precision_neotropic_spatial_filter

occ_euterpe_filter_date_precision_neotropic_spatial <- dplyr::left_join(occ_euterpe_filter_date_precision_neotropic, occ_euterpe_filter_date_precision_neotropic_spatial_filter) %>% 
    dplyr::mutate(spatial_filter = ifelse(is.na(spatial_filter), FALSE, TRUE))
occ_euterpe_filter_date_precision_neotropic_spatial

## bias filter --------

# bias
occ_euterpe_filter_date_precision_neotropic_spatial_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_euterpe_filter_date_precision_neotropic_spatial, 
    species = "species",
    lon = "longitude", 
    lat = "latitude",
    tests = c("capitals", # radius around capitals
              "centroids", # radius around country and province centroids
              "duplicates", # records from one species with identical coordinates
              "equal", # equal coordinates
              "gbif", # radius around GBIF headquarters
              "institutions", # radius around biodiversity institutions
              "outliers", # remove outliers
              "seas", # in the sea
              "urban", # within urban area
              "validity", # outside reference coordinate system
              "zeros" # plain zeros and lat = lon
    ),
    capitals_rad = 2000,
    centroids_rad = 2000,
    centroids_detail = "both",
    inst_rad = 100,
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(.cen = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .cen),
                  .summary = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .summary)) %>%     dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_euterpe_filter_date_precision_neotropic_spatial_bias

## filter ------

# filter
occ_euterpe_cleaned <- occ_euterpe_filter_date_precision_neotropic_spatial_bias %>% 
    dplyr::filter(precision_filter_year == TRUE,
                  neotropic_filter = TRUE, 
                  bias_filter == TRUE,
                  spatial_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_euterpe_cleaned

# map
sa <- li[li$subregion %in% c("South America", "Central America"),]
tm_shape(neo) +
    tm_polygons() +
    tm_shape(li) +
    tm_borders() +
    tm_shape(occ_euterpe_cleaned) +
    tm_bubbles(size = .4, 
               fill = "species") +
    tm_facets_wrap("species") +
    tm_layout(legend.show = FALSE)

# count
occ_euterpe_cleaned %>% 
    sf::st_drop_geometry() %>% 
    dplyr::count(species)

# export ------------------------------------------------------------------

# export
occ_euterpe_filter_date_precision_neotropic_spatial_bias %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(-c(.val:.summary)) %>% 
    readr::write_csv("01_data/01_occurrences/02_cleaned/occ_euterpe_cleaned_raw.csv")

readr::write_csv(occ_euterpe_cleaned, "01_data/01_occurrences/02_cleaned/occ_euterpe_cleaned.csv")

# end ---------------------------------------------------------------------