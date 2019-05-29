# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# Packages used in this script
library(tidyverse) # Base suite of functions
library(ncdf4) # For opening and working with NetCDF files
library(lubridate) # For convenient date manipulation
library(heatwaveR, lib.loc = "../R-packages/")

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
NWA_corners <- readRDS("data/NWA_corners.Rda")

# The NAPA data location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# The NAPA model lon/lat values
NAPA_coords <- readRDS("data/NAPA_coords.Rda")

# The NAPA model lon/lat values for the study area
NAPA_coords_sub <- readRDS("data/NAPA_coords_sub.Rda")

# Load NAPA bathymetry/lon/lat
NAPA_bathy <- readRDS("data/NAPA_bathy.Rda")

# Load MHW results
NAPA_MHW_sub <- readRDS("data/NAPA_MHW_sub.Rda")

# Load NAPA variables
NAPA_vars <- readRDS("data/NAPA_vars.Rda")



# Extract one variable ----------------------------------------------------

# testers...
# nc_var <- "sst"
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
  colnames(want_var)[4] <- nc_var

  # Subset pixels to study area
  want_var <- right_join(want_var, coords, by = c("lon_index", "lat_index")) %>%
    na.omit()

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
  ) # 201 seconds

  test <- filter(all_one_var, lon_index == 116, lat_index == 340)
  colnames(test)[4] <- "temp"
  test <- ts2clm(test, climatologyPeriod = c("1994-01-01", "2015-12-29"), clmOnly = T)

  # Calculate climatologies
    # Change the column name to 'temp' for ease of use
  colnames(all_one_var)[4] <- "temp"
  system.time(
    all_one_clim <- plyr::ddply(all_one_var, c("lon", "lat"), ts2clm, .parallel = T, clmOnly = T,
                                climatologyPeriod = c("1994-01-01", "2015-12-29"))
  ) # xxx seconds
  colnames(all_one_var)[4] <- one_var
}
