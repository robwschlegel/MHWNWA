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
# product_id <- "hawaii_soest_82a2_f4b4_d15c"
# chosen_fields <- "tmp2m"
# product_id <- "hawaii_soest_33b7_e2df_ef2b"
# chosen_fields <- "lhtfl"
# time_range <- dl_years[1,]
OAFlux_dl_func <- function(time_range, product_id, chosen_fields){
  # Annoyingly, the dimension names are not consistent for this product
  if(chosen_fields[1] == "tmp2m"){
    res1 <- griddap(x = "hawaii_soest_82a2_f4b4_d15c",
                   url = "http://apdrc.soest.hawaii.edu/erddap/",
                   # time2 = c(time_range$start, time_range$end),
                   time2 = c("1985-01-01T00:00:00Z", "1985-01-01T00:00:00Z"),
                   # latitude = NWA_corners_sub_1_degree[3:4],
                   latitude = c(-89.5, 89.5),
                   # longitude = (NWA_corners_sub_1_degree[1:2]+360),
                   longitude = c(0.5, 359.5),
                   # fields = chosen_fields)$data
                   fields = "tmp2m")
  } else {
    res1 <- griddap(x = product_id,
                   url = "http://apdrc.soest.hawaii.edu/erddap/",
                   time = c(time_range$start, time_range$end),
                   latitude = NWA_corners_sub_1_degree[3:4],
                   longitude = (NWA_corners_sub_1_degree[1:2]+360),
                   fields = chosen_fields)$data
  }
  res2 <- res1 %>%
    mutate(time = as.Date(str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time) %>%
    select(lon, lat, t, everything()) %>%
    na.omit()
return(res2)
}


# Wrapper convenience function for the above function
OAFlux_dl_wrap <- function(product_id_wrap, chosen_fields_wrap){
  res <- dl_years %>%
    group_by(date_index) %>%
    nest() %>%
    mutate(dl_data = map(data, OAFlux_dl_func,
                         product_id = product_id_wrap,
                         chosen_fields = chosen_fields_wrap)) %>%
    ungroup() %>%
    select(-date_index, -data) %>%
    unnest()
}

# daily mean surface latent heat flux, positive upward [w/m/m]
system.time(OAFlux_lhtfl <- OAFlux_dl_wrap("hawaii_soest_33b7_e2df_ef2b", "lhtfl")) # 104 seconds

# daily mean surface sensible heat flux, positive upward [w/m/m]
system.time(OAFlux_shtfl <- OAFlux_dl_wrap("hawaii_soest_b6e0_963a_b40f", "shtfl")) # 82 seconds

# daily mean specific humidity at 2m [g/kg]
system.time(OAFlux_hum2m <- OAFlux_dl_wrap("hawaii_soest_38c8_5dc2_a69b", "hum2m")) # 82 seconds

# daily mean neutral wind speed at 10m [m/s]
system.time(OAFlux_wnd10 <- OAFlux_dl_wrap("hawaii_soest_26fa_a77c_2ce7", "wnd10")) # 75 seconds

# daily mean air temperature at 2m [degc]
# system.time(OAFlux_tmp2m <- OAFlux_dl_wrap("hawaii_soest_82a2_f4b4_d15c", "tmp2m")) # xxx seconds

# Join all of the data
OAFlux <- left_join(OAFlux_lhtfl, OAFlux_shtfl, by = c("lon", "lat", "t")) %>%
  left_join(OAFlux_hum2m, by = c("lon", "lat", "t")) %>%
  left_join(OAFlux_wnd10, by = c("lon", "lat", "t"))

# Save
write_rds(OAFlux, "data/OAFlux.Rda")

