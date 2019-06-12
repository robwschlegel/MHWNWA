# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# Packages used in this script
# library(rlang)
library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse) # Base suite of functions
library(ncdf4) # For opening and working with NetCDF files
library(lubridate) # For convenient date manipulation
library(heatwaveR, lib.loc = "../R-packages/")
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(tidync, lib.loc = "../R-packages/")
library(akima) # For finding pixels next to missing pixels
library(FNN) # For finding pixels next to missing pixels

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

# The NAPA 5 day U vector data location
NAPA_U_files <- dir("../../data/NAPA025/5d_grid_U", full.names = T)

# The NAPA 5 day V vector data location
NAPA_V_files <- dir("../../data/NAPA025/5d_grid_V", full.names = T)

# The NAPA 5 day W vector data location
NAPA_W_files <- dir("../../data/NAPA025/5d_grid_W", full.names = T)

# The NAPA vector file dates
NAPA_vector_files_dates <- readRDS("data/NAPA_vector_files_dates.Rda")

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

# Load the NAPA clim vector files
NAPA_clim_vecs <- readRDS("data/NAPA_clim_vecs.Rda")

# MHW Events
NAPA_MHW_event <- NAPA_MHW_sub %>%
  select(-clims, -cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 0) %>%
  unnest(events)

# Load NAPA variables
NAPA_vars <- readRDS("data/NAPA_vars.Rda")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  select(-region, -subregion)

# The OISST land mask
land_mask_OISST <- readRDS("data/land_mask_OISST.Rda")

# Filter it to the smaller domain only
land_mask_OISST_sub <- land_mask_OISST%>%
  filter(lon >= NWA_corners_sub[1], lon <= NWA_corners_sub[2],
         lat >= NWA_corners_sub[3], lat <= NWA_corners_sub[4])

# Create a subsetted water only mask
land_mask_OISST_sub_water <- land_mask_OISST_sub %>%
  filter(lsmask == 1)


# Extract one variable ----------------------------------------------------

# testers...
# nc_var <- "sst"
# nc_var <- "taum"
# nc_var <- "qla_oce"
# file_name <- NAPA_files[1]
extract_one_var <- function(file_name, nc_var, coords = NAPA_bathy_sub){
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
    select(lon_index, lat_index, t, var) %>%
    right_join(coords, by = c("lon_index", "lat_index")) %>%
    na.omit()
  colnames(want_var)[4] <- nc_var


  # test <- filter(want_var, t == date_end)
  # ggplot(NAPA_coords, aes(x = lon, y = lat)) +
  #   # geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  #   geom_point(data = test, aes(colour = sst),
  #              shape = 15, size = 0.5) +
  #   coord_cartesian(xlim = NWA_corners[1:2],
  #                   ylim = NWA_corners[3:4])

  # Close connection and exit
  nc_close(nc)
  return(want_var)
}


# Calculate clims for one variable ----------------------------------------

# testers...
# one_var <- "sst"
clim_one_var <- function(one_var, chosen_files = NAPA_files){

  print(paste0("Began run on ",one_var," at ",Sys.time()))
  # Load all of the data within the study area for one variable
  # system.time(
  all_one_var <- plyr::ldply(chosen_files, extract_one_var, nc_var = one_var, .parallel = T)
  # ) # 200 seconds
  print(paste0("Finished loading ",one_var," at ",Sys.time()))

  # Calculate climatologies
    # Change variable column name to 'temp' for ease of use
  colnames(all_one_var)[4] <- "temp"
  # system.time(
  all_one_clim <- plyr::ddply(all_one_var, c("lon", "lat"), ts2clm, .parallel = T,
                              clmOnly = T,  roundClm = FALSE,
                              climatologyPeriod = c("1994-01-01", "2015-12-29"))
  # ) # 230 seconds
  colnames(all_one_clim)[4] <- one_var
  all_one_clim$thresh <- NULL
  print(paste0("Finished calculating ",one_var," clims at ",Sys.time()))

  # Save, clean-up and exit
  saveRDS(all_one_clim, paste0("data/NAPA_clim_",one_var,".Rda"))
  rm(all_one_var, all_one_clim); gc()
  print(paste0("Finished run on ",one_var," at ",Sys.time()))
}


# Extract one vector ------------------------------------------------------

# testers...
# nc_vec <- "depthu"
# nc_vec <- "depthv"
# nc_vec <- "depthw"
# file_name <- NAPA_U_files[1]
# file_name <- NAPA_V_files[1]
# file_name <- NAPA_W_files[276]
extract_one_vec <- function(file_name, coords = NAPA_bathy_sub){

  # List of names of various unwanted columns
  bad_col <- c("avt", "e3v", "e3u", "bathy", "depthu", "depthv", "depthw")

  # Find the name of the depth variable
  depth_var <- tidync(file_name) %>% hyper_dims()
  depth_var <- depth_var$name[grepl(pattern = "depth", depth_var$name)]

  # Extract data
  # ncdump::NetCDF(file_name)
  want_vec <- tidync(file_name) %>%
    activate() %>%
    hyper_filter(!!sym(depth_var) := !!sym(depth_var) < 1,
                 x = between(x, min(coords$lon_index), max(coords$lon_index)),
                 y = between(y, min(coords$lat_index), max(coords$lat_index))) %>%
    hyper_tibble() %>%
    dplyr::rename(lon_index = x, lat_index = y, t = time_counter) %>%
    right_join(coords, by = c("lon_index", "lat_index")) %>%
    mutate(t = as.Date(as.POSIXct(t, origin = "1900-01-01", tz = "UTC"))) %>%
    select(colnames(.)[!colnames(.) %in% bad_col]) %>%
    select(lon_index, lat_index, lon, lat, t, everything())

  # Test visuals
  # ggplot(NAPA_coords, aes(x = lon, y = lat)) +
  #   # geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  #   geom_point(data = want_vec, aes(colour = voce),
  #              shape = 15, size = 0.5) +
  #   coord_cartesian(xlim = NWA_corners[1:2],
  #                   ylim = NWA_corners[3:4])

  # Exit
  return(want_vec)
}

# test <- extract_one_vec(NAPA_U_files[1])


# Calculate clims for one vector ------------------------------------------

# testers...
# chosen_files <- NAPA_W_files
clim_one_vec <- function(chosen_files){

  # Find variable being processed
  var_ref <- sapply(str_split(basename(chosen_files[1]), "_"), "[[", 4)

  # Load all of the data within the study area for one variable
  print(paste0("Began run on ",var_ref," at ",Sys.time()))
  # system.time(
  all_one_vec <- plyr::ldply(chosen_files, extract_one_vec, .parallel = T)
  # ) # 156 seconds
  print(paste0("Finished loading ",var_ref," at ",Sys.time()))

  # Calculate climatologies
  # Change variable column name to 'temp' for ease of use
  name_hold <- colnames(all_one_vec)[6]
  colnames(all_one_vec)[6] <- "temp"
  # system.time(
  all_one_clim <- plyr::ddply(all_one_vec, c("lon", "lat"), ts2clm, .parallel = T,
                              clmOnly = T,  roundClm = FALSE, maxPadLength = 6,
                              climatologyPeriod = c("1994-01-01", "2015-12-27"))
  # ) # 147 seconds
  colnames(all_one_clim)[4] <- name_hold
  all_one_clim$thresh <- NULL
  print(paste0("Finished calculating ",var_ref," clims at ",Sys.time()))

  # Save, clean-up and exit
  saveRDS(all_one_clim, paste0("data/NAPA_clim_",var_ref,".Rda"))
  rm(all_one_vec, all_one_clim); gc()
  print(paste0("Finished run on ",var_ref," at ",Sys.time()))
}


# Function to extract all vectors at a file step --------------------------

# testers...
# file_number <- 1376
extract_all_vec <- function(file_number){

  # Extract and join variables with a for loop
  # NB: This should be optimised...
  u_vecs <- extract_one_vec(NAPA_U_files[file_number])
  v_vecs <- extract_one_vec(NAPA_V_files[file_number])
  w_vecs <- extract_one_vec(NAPA_W_files[file_number])

  NAPA_vecs_extracted <- left_join(u_vecs, v_vecs, by = colnames(u_vecs)[1:5]) %>%
    left_join(w_vecs, by = colnames(u_vecs)[1:5])

  # Exit
  return(NAPA_vecs_extracted)
}


# Build vector data packets -----------------------------------------------

# testers...
# event_sub <- NAPA_MHW_event[23,]
data_vec_packet <- function(event_sub){

  # Create date and file index for loading
  date_idx <- seq(event_sub$date_start, event_sub$date_end, by = "day")
  file_idx <- which(NAPA_vector_files_dates$date %in% date_idx)

  # Load required base data
  # system.time(
  packet_base <- plyr::ldply(file_idx, extract_all_vec) %>%
    filter(t %in% date_idx) %>%
    mutate(doy = format(t, "%m-%d"))
  # ) # 16 seconds for seven files

  # Join to climatologies
  packet_join <- left_join(packet_base, NAPA_clim_vecs, by = c("lon", "lat", "doy"))

  # Create anomaly values and remove clim columns
  packet_anom <- packet_join %>%
    mutate(uoce_anom = uoce - uoce_clim,
           voce_anom = voce - voce_clim,
           wo_anom = wo - wo_clim) %>%
    dplyr::select(lon, lat, doy, uoce:wo_anom,
                  -c(colnames(NAPA_clim_vecs)[-c(1:3)]))

  # Create mean synoptic values
  packet_mean <- packet_anom %>%
    select(-doy) %>%
    group_by(lon, lat) %>%
    summarise_all(mean, na.rm = T) %>%
    arrange(lon, lat) %>%
    ungroup() %>%
    nest(.key = "synoptic")

  # Combine results with MHW dataframe
  packet_res <- cbind(event_sub, packet_mean)

  # Test visuals
  # ggplot(packet_mean, aes(x = lon, y = lat)) +
  #   geom_point(aes(colour = uoce_anom)) +
  #   scale_colour_gradient2(low = "blue", high = "red") +
  #   coord_cartesian(xlim = NWA_corners[1:2],
  #                   ylim = NWA_corners[3:4])

  # Exit
  return(packet_res)
}


# Function to create the OISST landmask -----------------------------------

# The land mask NetCDF file is downloaded here:
# https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
OISST_land_mask_func <- function(){
  lmask <- want_vec <- tidync("data/lsmask.oisst.v2.nc") %>%
    activate() %>%
    hyper_tibble() %>%
    dplyr::select(-time) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon))

  # Test visuals
  # ggplot(lmask, aes(x = lon, y = lat)) +
  # geom_raster(aes(fill = lsmask))

  saveRDS(lmask, "data/land_mask_OISST.Rda")
}


# Functions for fillling in missing pixels --------------------------------

# Find nearest neighbours and take average
  # NB: For some reason this does not work well
#testers...
# df <- synoptic_states_anom_cartesian %>%
#   ungroup() %>%
#   filter(region == "cbs", sub_region == "(0,50]", event_no == 1) %>%
#   select(-region, -sub_region, -event_no)
# Order by lon/lat and exit
fill_grid_func <- function(df){

  # Create base dataframe
  df_base <- left_join(land_mask_OISST_sub_water, df, by = c("lon", "lat")) %>%
    mutate(row_index = 1:n()) %>%
    select(row_index, everything())

  # Par it down for later
  df_bare <- df_base %>%
    dplyr::select(-lon, -lat, -lsmask)

  # Take only rows with missing pixels
  df_NA <- df_base[!complete.cases(df_base),]
  # Find their nine nearest neighbours
    # We intentionally use the land mask with land pixels for merging to feel the coastline
  pixel_index <- FNN::knnx.index(data = as.matrix(land_mask_OISST_sub[,c("lon", "lat")]),
                                 query = as.matrix(df_NA[,c("lon", "lat")]), k = 9)

  # Create the means of the nearest pixels
  merge_index <- cbind(df_NA[,c("lon", "lat", "lsmask")], pixel_index) %>%
    gather(key = "pixel", value = "row_index", -lon, -lat, -lsmask) %>%
    dplyr::select(-lsmask) %>%
    left_join(mutate(land_mask_OISST_sub, row_index = 1:n()), by = "row_index") %>%
    filter(lsmask == 1) %>%  # Remove land pixels
    left_join(df_bare, by = "row_index") %>%
    dplyr::select(-pixel, -row_index, -lsmask, -lon.y, -lat.y) %>%
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean", na.rm = T) %>%
    ungroup() %>%
    na.omit()

  # Combine and exit
  df_res <- df %>%
    na.omit() %>%
    bind_rows(merge_index) %>%
    group_by(lon, lat) #%>%
    # summarise_all(.funs = "mean", na.rm = T) %>%
    # ungroup()

  # Check
  # length(unique(paste(df_res$lon, df_res$lat)))
}


# Function for interpolating with kima
# testers...
# interpp_stat <- "sst_anom"
interpp_data <- function(df_base, df_grid, interpp_stat){
  df_base <- data.frame(df_base)
  suppressWarnings(
    res <- as.data.frame(interpp(x = as.vector(df_base$lon), y = as.vector(df_base$lat),
                                 as.vector(df_base[,colnames(df_base) == interpp_stat]),
                                 xo = as.vector(df_grid$lon), yo = as.vector(df_grid$lat), linear = T,
                                 extrap = FALSE, duplicate = "mean"))
  )
  colnames(res) <- c("lon_O_corrected", "lat_O", "interp")
  suppressMessages(
    df_res <- right_join(df_base, res)
  )
  return(df_res)
}
