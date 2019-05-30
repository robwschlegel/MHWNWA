# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# Packages used in this script
library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse) # Base suite of functions
library(ncdf4) # For opening and working with NetCDF files
library(lubridate) # For convenient date manipulation
# unloadNamespace("jsonlite")
library(heatwaveR, lib.loc = "../R-packages/")
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
NWA_corners <- readRDS("data/NWA_corners.Rda")

# Individual regions
NWA_coords <- readRDS("data/NWA_coords_cabot.Rda")

# The NAPA data location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA model lon/lat values
NAPA_coords <- readRDS("data/NAPA_coords.Rda")

# The NAPA model lon/lat values for the study area
NAPA_coords_sub <- readRDS("data/NAPA_coords_sub.Rda")

# Load NAPA bathymetry/lon/lat
NAPA_bathy <- readRDS("data/NAPA_bathy.Rda")

# Load NAPA bathymetry/lon/lat for study area only
NAPA_bathy_sub <- readRDS("data/NAPA_bathy_sub.Rda")

# Load MHW results
NAPA_MHW_sub <- readRDS("data/NAPA_MHW_sub.Rda")

# Load NAPA variables
NAPA_vars <- readRDS("data/NAPA_vars.Rda")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  select(-region, -subregion)


# Extract one variable ----------------------------------------------------

# testers...
# nc_var <- "sst"
# nc_var <- "taum"
# file_name <- NAPA_files[1]
extract_one_var <- function(file_name, nc_var, coords = NAPA_coords_sub){
  # Open connection
  nc <- nc_open(as.character(file_name))

  # Extract dates from file name
  date_start <- ymd(str_sub(basename(as.character(file_name)), start = 29, end = 36))
  date_end <- ymd(str_sub(basename(as.character(file_name)), start = 38, end = 45))
  date_seq <- seq(date_start, date_end, by = "day")

  # Extract and process data
  want_var <- as.data.frame(ncvar_get(nc, varid = nc_var)) %>%
    mutate(lon_index = as.numeric(nc$dim$x$vals)) %>%
    gather(-lon_index, key = lat_index, value = var) %>%
    mutate(t = rep(date_seq, each = 388080),
           lat_index = rep(rep(as.numeric(nc$dim$y$vals), each = 528), times = 5)) %>%
    select(lon_index, lat_index, t, var)

  # Subset pixels to study area
  want_var <- right_join(want_var, coords, by = c("lon_index", "lat_index")) %>%
    na.omit() %>%
    group_by(lon, lat) %>%
    filter(mean(var) != 0) %>%
    # filter(max(t, na.rm = T) == date_end) %>%
    ungroup()
  colnames(want_var)[4] <- nc_var

  test <- filter(want_var, t == date_end)
  ggplot(NAPA_coords, aes(x = lon, y = lat)) +
    # geom_polygon(data = map_base, aes(group = group), show.legend = F) +
    geom_point(data = test, aes(colour = taum),
               shape = 15, size = 0.5) +
    coord_cartesian(xlim = NWA_corners[1:2],
                    ylim = NWA_corners[3:4])

  # Close connection and exit
  nc_close(nc)
  return(want_var)
}


# Calculate clims for one variable ----------------------------------------

# testers...
# one_var <- "sst"
clim_one_var <- function(one_var, chosen_files = NAPA_files){
  # Load all of the data within the study area for one variable
  system.time(
    all_one_var <- plyr::ldply(chosen_files, extract_one_var, nc_var = one_var, .parallel = T)
  ) # 200 seconds

  # test <- filter(all_one_var, lon_index == 116, lat_index == 340)
  # colnames(test)[4] <- "temp"
  # test <- ts2clm(test, climatologyPeriod = c("1994-01-01", "2015-12-29"), clmOnly = T, roundClm = FALSE)

  # test <- all_one_var %>%
    # group_by(lon, lat) %>%
    # filter(max(t, na.rm = T) == "2015-12-29") %>%
    # ungroup()
    # filter(t == "2015-12-29")

  # ggplot(NWA_coords, aes(x = lon, y = lat)) +
  #   geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  #   geom_point(data = test, aes(colour = sst),
  #              shape = 15, size = 0.5) +
  #   coord_cartesian(xlim = NWA_corners[1:2],
  #                   ylim = NWA_corners[3:4])

  # Screen out incomplete time series
  # system.time(
  #   all_one_var_sub <- plyr::ddply(all_one_var, c("lon", "lat"), screen_ts, .parallel = T)
  # ) # 200 seconds

  # Calculate climatologies
    # Change the column name to 'temp' for ease of use
  colnames(all_one_var)[4] <- "temp"
  system.time(
    all_one_clim <- plyr::ddply(all_one_var, c("lon", "lat"), ts2clm, .parallel = T,
                                clmOnly = T,  roundClm = FALSE,
                                climatologyPeriod = c("1994-01-01", "2015-12-29"))
  ) # xxx seconds
  colnames(all_one_var)[4] <- one_var
}
