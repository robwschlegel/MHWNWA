# code/download.R
# This script houses the code used to download the obs/reanalysis data
# used in this project


# Libraries etc. ----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

# library(vctrs, lib.loc = "../R-packages/")
# library(pillar, lib.loc = "../R-packages/")
# library(tibble, lib.loc = "../R-packages/")
library(forcats, lib.loc = "../R-packages/")
library(rerddap, lib.loc = "../R-packages/")
library(tidync, lib.loc = "../R-packages/")
library(tidyverse, lib.loc = "../R-packages/")

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
NWA_corners <- readRDS("data/NWA_corners.Rda")

# Sub-region coordinates
NWA_NAPA_info <- readRDS("data/NWA_NAPA_info.Rda")

# Create smaller corners to use less RAM
# This also better matches the previous South African work
# The Tasmania work had corners of roughly 2 degrees greater than the study area
NWA_corners_sub <- c(NWA_corners[1]+8, NWA_corners[2]-8, NWA_corners[3]+8, NWA_corners[4]-8)

# Round the corners for 1 degree grid use
NWA_corners_sub_1_degree <- round(NWA_corners_sub)
NWA_corners_sub_1_degree <- c(NWA_corners_sub_1_degree[1]-0.5, NWA_corners_sub_1_degree[2]+0.5,
                              NWA_corners_sub_1_degree[3]-0.5, NWA_corners_sub_1_degree[4]+0.5)

# Individual regions
NWA_coords <- readRDS("data/NWA_coords_cabot.Rda")

# Year index for downloading OAFlux ERDDAP data
dl_years <- data.frame(date_index = 1:4,
                       start = as.Date(c("1993-01-01", "2000-01-01",
                                         "2007-01-01", "2014-01-01")),
                       end = as.Date(c("1999-12-31", "2006-12-31",
                                       "2013-12-31", "2018-12-31")))


# Download WHOI OAFlux data -----------------------------------------------

# Function for consistent access to the various OAFlux products
# testers...
# product_id <- "hawaii_soest_33b7_e2df_ef2b"
# chosen_fields <- c("lhtfl", "err")
# time_range <- dl_years[1,]
OAFlux_dl_func <- function(time_range, product_id, chosen_fields){
  res <- griddap(x = product_id,
                 url = "http://apdrc.soest.hawaii.edu/erddap/",
                 time = c(time_range$start, time_range$end),
                 latitude = NWA_corners_sub_1_degree[3:4],
                 longitude = (NWA_corners_sub_1_degree[1:2]+360),
                 fields = chosen_fields)$data %>%
    mutate(time = as.Date(str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time) %>%
    select(lon, lat, t, everything()) %>%
    na.omit()
}

# Wrapper convenience function for the above function
OAFlux_dl_wrap <- function(product_id, chosen_fields){
  res <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = product_id,
                         chosen_fields = chosen_fields)) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
}

# daily mean surface latent heat flux, positive upward [w/m/m]
system.time(OAFlux_lhtfl <- OAFlux_dl_wrap("hawaii_soest_33b7_e2df_ef2b", "lhtfl")) # 104 seconds
system.time(
  OAFlux_lhtfl <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = "hawaii_soest_33b7_e2df_ef2b",
                         chosen_fields = "lhtfl")) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
) # 55 seconds

# daily mean specific humidity at 2m [g/kg]
system.time(
  OAFlux_hum2m <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = "hawaii_soest_38c8_5dc2_a69b",
                         chosen_fields = "hum2m")) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
) # 89 seconds

# daily mean surface sensible heat flux, positive upward [w/m/m]
system.time(
  OAFlux_shtfl <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = "hawaii_soest_b6e0_963a_b40f",
                         chosen_fields = "shtfl")) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
) # xxx seconds

# daily mean surface sensible heat flux, positive upward [w/m/m]
system.time(
  OAFlux_shtfl <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = "hawaii_soest_b6e0_963a_b40f",
                         chosen_fields = "shtfl")) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
) # xxx seconds
