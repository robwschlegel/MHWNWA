# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))

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
# library(cowplot) # For gridding multiple figures together

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

# Individual regions
NWA_coords <- readRDS("data/NWA_coords_cabot.Rda")

# The NAPA data location
NAPA_files <- dir("../../data/NAPA025/1d_grid_T_2D", full.names = T)

# Load NAPA file date index
NAPA_files_dates <- readRDS("data/NAPA_files_dates.Rda")

# Load full variable climatology file
NAPA_clim_vars <- readRDS("data/NAPA_clim_vars.Rda")

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

# Load grid for converting NAPA to OISST coordinates
load("data/lon_lat_NAPA_OISST.Rdata")

# Change to fit with this project
lon_lat_NAPA_OISST <- lon_lat_NAPA_OISST %>%
  dplyr::select(-lon, -lat, -dist, -nav_lon_corrected) %>%
  dplyr::rename(lon = nav_lon, lat = nav_lat) %>%
  mutate(lon = round(lon, 4),
         lat = round(lat, 4)) %>%
  mutate(lon_O = ifelse(lon_O > 180, lon_O-360, lon_O))


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
                              climatologyPeriod = c("1998-01-01", "2015-12-29"))
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
                              climatologyPeriod = c("1998-01-01", "2015-12-27"))
  # ) # 147 seconds
  colnames(all_one_clim)[4] <- name_hold
  all_one_clim$thresh <- NULL
  print(paste0("Finished calculating ",var_ref," clims at ",Sys.time()))

  # Save, clean-up and exit
  saveRDS(all_one_clim, paste0("data/NAPA_clim_",var_ref,".Rda"))
  rm(all_one_vec, all_one_clim); gc()
  print(paste0("Finished run on ",var_ref," at ",Sys.time()))
}


# Extract the desired variables from a given NetCDF file ------------------

# testers...
# file_name <- NAPA_files[1]
extract_all_var <- function(file_name){

  # Extract and join variables with a for loop
  # NB: This should be optimised...
  NAPA_vars_extracted <- data.frame()
  system.time(
    for(i in 1:length(NAPA_vars$name)){
      extract_one <- extract_one_var(NAPA_vars$name[i], file_name = file_name)
      if(nrow(NAPA_vars_extracted) == 0){
        NAPA_vars_extracted <- rbind(extract_one, NAPA_vars_extracted)
      } else {
        NAPA_vars_extracted <- left_join(NAPA_vars_extracted, extract_one,
                                         by = c("lon_index", "lat_index", "lon", "lat", "bathy", "t"))
      }
    }
  ) # 18 seconds for one
  NAPA_vars_extracted <- dplyr::select(NAPA_vars_extracted,
                                       lon_index, lat_index, lon, lat, t, bathy, everything())

  # Exit
  return(NAPA_vars_extracted)
}


# Build variable data packets ---------------------------------------------

# testers...
# event_sub <- NAPA_MHW_event[1,]
data_packet <- function(event_sub){

  # Create date and file index for loading
  date_idx <- seq(event_sub$date_start, event_sub$date_end, by = "day")
  file_idx <- filter(NAPA_files_dates, date %in% date_idx) %>%
    mutate(file = as.character(file)) %>%
    select(file) %>%
    unique()

  # Load required base data
  # system.time(
  packet_base <- plyr::ldply(file_idx$file, extract_all_var) %>%
    filter(t %in% date_idx) %>%
    mutate(doy = format(t, "%m-%d"))
  # ) # 125 seconds for seven files

  # Join to climatologies
  packet_join <- left_join(packet_base, NAPA_clim_vars, by = c("lon", "lat", "doy"))

  # Create anomaly values and remove clim columns
  packet_anom <- packet_join %>%
    mutate(emp_oce_anom = emp_oce - emp_oce_clim,
           fmmflx_anom = fmmflx - fmmflx_clim,
           mldkz5_anom = mldkz5 - mldkz5_clim,
           mldr10_1_anom = mldr10_1 - mldr10_1_clim,
           qemp_oce_anom = qemp_oce - qemp_oce_clim,
           qns_anom = qns - qns_clim,
           qt_anom = qt - qt_clim,
           ssh_anom = ssh - ssh_clim,
           sss_anom = sss - sss_clim,
           sst_anom = sst - sst_clim,
           taum_anom = taum - taum_clim) %>%
    dplyr::select(lon, lat, doy, emp_oce:taum_anom,
                  -c(colnames(NAPA_clim_vars)[-c(1:3)]))
  # dplyr::select(-c(colnames(packet_base)[-c(3,4,ncol(packet_base))]),
  #               -c(colnames(NAPA_clim_vars)[-c(1:3)]))

  # Create mean synoptic values
  packet_mean <- packet_anom %>%
    select(-doy) %>%
    # NB: The lowest pixels are a forcing edge and shouldn't be included
    # We can catch these out by filtering pixels whose SST is exactly 0
    filter(sst != 0) %>%
    group_by(lon, lat) %>%
    summarise_all(mean, na.rm = T) %>%
    arrange(lon, lat) %>%
    ungroup() %>%
    nest(.key = "synoptic")

  # Combine results with MHW dataframe
  packet_res <- cbind(event_sub, packet_mean)

  # Test visuals
  # ggplot(packet_mean, aes(x = lon, y = lat)) +
  #   geom_point(aes(colour = sst_anom)) +
  #   scale_colour_gradient2(low = "blue", high = "red") +
  #   coord_cartesian(xlim = NWA_corners[1:2],
  #                   ylim = NWA_corners[3:4])

  # Exit
  return(packet_res)
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



# Determine node indexes --------------------------------------------------

# testers...
# data_packet <- all_anom; som_output <- som_all_anom
event_node_index <- function(data_packet, som_output){

  # Count the number of events per node
  node_count <- as.data.frame(table(som_output$classif)) %>%
    dplyr::rename(node = Var1,
                  count = Freq) %>%
    mutate(node = as.numeric(as.character(node)))

  # Create a more complete data.frame of info
  event_node <- data.frame(event_ID = data_packet[,"event_ID"],
                           node = som_output$classif) %>%
    separate(event_ID, into = c("region", "sub_region", "event_no"), sep = "BBB") %>%
    left_join(node_count, by = "node")

  # NB: This is potentially where the season of the event would be inserted

  return(event_node)
}


# Unpack SOM results ------------------------------------------------------

# Create mean results from initial data frame based on node clustering
# testers...
# data_packet <- all_anom; som_output <- som_all_anom
som_unpack_mean <- function(data_packet, som_output){

  # Determine which event goes in which node and melt
  data_packet_long <- data.frame(event_ID = data_packet[,"event_ID"],
                                 node = som_output$classif) %>%
    separate(event_ID, into = c("region", "sub_region", "event_no"), sep = "BBB") %>%
    cbind(data_packet[,-1]) %>%
    data.table() %>%
    reshape2::melt(id = c("region", "sub_region", "event_no", "node"),
                   measure = c(colnames(.)[-c(1:4)]),
                   variable.name = "variable", value.name = "value")

  # Create the mean values that serve as the unscaled results from the SOM
  var_unscaled <- data_packet_long[, .(val = mean(value, na.rm = TRUE)),
                                   by = .(node, variable)] %>%
    separate(variable, into = c("lon", "lat", "var"), sep = "BBB") %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
  return(var_unscaled)
}


# Create a summary figure for a chosen node -------------------------------
## NB: This function contains objects created in "IMBeR_2019_figures.R"

node_figure <- function(node_number){
  sub_synoptic_var_state <- synoptic_states_anom_cartesian %>%
    ungroup() %>%
    left_join(node_index_all_anom, by = c("region", "sub_region", "event_no")) %>%
    filter(node == node_number) %>%
    dplyr::select(-region, -sub_region, -event_no, -node, -count) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean")
  sub_synoptic_vec_state <- synoptic_vec_states_anom_cartesian %>%
    ungroup() %>%
    left_join(node_index_all_anom, by = c("region", "sub_region", "event_no")) %>%
    filter(node == node_number) %>%
    dplyr::select(-region, -sub_region, -event_no, -node, -count) %>%
    group_by(lon, lat) %>%
    summarise_all(.funs = "mean")
  sub_synoptic_state <- left_join(sub_synoptic_var_state, sub_synoptic_vec_state,
                                  by = colnames(sub_synoptic_var_state)[1:2]) %>%
    dplyr::rename(u_anom = uoce_anom, v_anom = voce_anom) %>%
    group_by(lon, lat) %>%
    mutate(arrow_size = ((abs(u_anom*v_anom)/ max(abs(u_anom*v_anom)))+0.2)/6) %>%
    ungroup()

  # Reduce wind/ current vectors
  lon_sub <- seq(min(sub_synoptic_state$lon), max(sub_synoptic_state$lon), by = 1)
  lat_sub <- seq(min(sub_synoptic_state$lat), max(sub_synoptic_state$lat), by = 1)
  vec_sub <- sub_synoptic_state %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) %>%
    na.omit()

  # Establish the vector scalar for the currents
  current_uv_scalar <- 4

  # SST and ocean current panel
  sst_U_V_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
    geom_raster(data = sub_synoptic_state, aes(fill = sst_anom)) +
    geom_segment(data = vec_sub, aes(xend = lon + u_anom * current_uv_scalar, yend = lat + v_anom * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.2, alpha = 0.5) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Colour scale
    scale_fill_gradient2(low = "blue", high = "red") +
    # Improve on the x and y axis labels
    scale_x_continuous(breaks = seq(-70, -50, 10),
                       labels = c("70°W", "60°W", "50°W"),
                       position = "bottom") +
    scale_y_continuous(breaks = seq(35, 55, 10),
                       labels = scales::unit_format(suffix = "°N", sep = "")) +
    # Slightly shrink the plotting area
    coord_equal(xlim = NWA_corners_sub[1:2],
                ylim = NWA_corners_sub[3:4], expand = F) +
    labs(x = NULL, y = NULL, fill = "SST anom. (°C)") +
    theme(legend.position = "bottom")
  # sst_U_V_panel

  # The net downward heatflux panel
  qt_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
    geom_raster(data = sub_synoptic_state, aes(fill = qt_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  # Colour scale
    scale_fill_gradient2(low = "blue", high = "red") +
    # Improve on the x and y axis labels
    scale_x_continuous(breaks = seq(-70, -50, 10),
                       labels = c("70°W", "60°W", "50°W"),
                       position = "bottom") +
    scale_y_continuous(breaks = seq(35, 55, 10),
                       labels = scales::unit_format(suffix = "°N", sep = "")) +
    # Slightly shrink the plotting area
    # coord_cartesian(xlim = NWA_corners_sub[1:2], ylim = NWA_corners_sub[3:4], expand = F) +
    coord_equal(xlim = NWA_corners_sub[1:2],
                ylim = NWA_corners_sub[3:4], expand = F) +
    labs(x = NULL, y = NULL,
         fill = "Net downward\nheat flux\nanom. (W/m2)") +
    theme(legend.position = "bottom")
  # qt_panel

  # The wind stress and mixed layer depth panel
  taum_mld_panel <- ggplot(sub_synoptic_state, aes(x = lon, y = lat)) +
    geom_raster(data = sub_synoptic_state, aes(fill = taum_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # The MLD contours
    geom_contour(aes(z = round(mldr10_1_anom, 2), colour = ..level..),
                 breaks = c(seq(-0.5, 0.5, 0.1)), size = 1) +
    # Colour scale
    scale_fill_gradient2(low = "blue", high = "red") +
    scale_colour_gradient2("MLD\nrel. anom.", low = "yellow", high = "green") +
    # Improve on the x and y axis labels
    scale_x_continuous(breaks = seq(-70, -50, 10),
                       labels = c("70°W", "60°W", "50°W"),
                       position = "bottom") +
    scale_y_continuous(breaks = seq(35, 55, 10),
                       labels = scales::unit_format(suffix = "°N", sep = "")) +
    # Slightly shrink the plotting area
    coord_equal(xlim = NWA_corners_sub[1:2],
                ylim = NWA_corners_sub[3:4], expand = F) +
    labs(x = NULL, y = NULL,
         fill = "Wind stress \n(N/m2)") +
    theme(legend.position = "bottom")
  # taum_mld_panel

  # The proportion plot
  # MHW season of (peak) occurrence and other meta-data
  NAPA_MHW_meta <- NAPA_MHW_event %>%
    mutate(month_peak = lubridate::month(date_peak, label = T),
           season_peak = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                                   month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                                   month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                                   month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
           sub_region = as.character(sub_region)) %>%
    left_join(node_index_all_anom, by = c("region", "sub_region", "event_no")) %>%
    filter(node == node_number)

  # Proportion of MHWs in each season in each node
  node_prop_info <- NAPA_MHW_meta %>%
    dplyr::select(region:event_no, month_peak:count) %>%
    group_by(node, season_peak) %>%
    mutate(node_season_prop = round(n()/count, 2)) %>%
    select(season_peak:node_season_prop) %>%
    unique() %>%
    ungroup()

  # Fill in the blanks
  node_prop_grid <- expand.grid(unique(node_prop_info$season_peak), 1:12) %>%
    dplyr::rename(season_peak = Var1, node = Var2) %>%
    mutate(season_peak = as.character(season_peak)) %>%
    # left_join(NWA_coords, by = "") %>%
    left_join(node_prop_info, by = c("node", "season_peak")) %>%
    mutate(count = replace_na(count, 0),
           node_season_prop = replace_na(node_season_prop, 0))

  # Proportion of MHWs in each season in each region
  region_prop_info <- NAPA_MHW_meta %>%
    dplyr::select(region:event_no, month_peak:count) %>%
    group_by(node, region) %>%
    mutate(region_node_prop = round(n()/count, 2)) %>%
    select(region, node, count, region_node_prop) %>%
    unique() %>%
    ungroup() #%>%
  # right_join(data.frame(node = 1:12), by = "node")
  # right_join(NWA_coords, by = "region")

  # Fill in the blanks
  region_prop_grid <- expand.grid(unique(region_prop_info$region), 1:12) %>%
    dplyr::rename(region = Var1, node = Var2) %>%
    mutate(region = as.character(region)) %>%
    left_join(NWA_coords, by = "region") %>%
    left_join(region_prop_info, by = c("region", "node")) %>%
    mutate(count = replace_na(count, 0),
           region_node_prop = replace_na(region_node_prop, 0)) %>%
    filter(node == node_number)

  som_prop_plot <- ggplot() +
    # geom_point(aes(colour = val)) +
    # geom_raster(aes(fill  = val)) +
    geom_polygon(data = map_base, aes(group = group, x = lon, y = lat), show.legend = F) +
    geom_polygon(data = region_prop_grid, aes(group = region, x = lon, y = lat, fill = region_node_prop),
                 colour = "black", alpha = 0.8) +
    geom_label(data = region_prop_grid, aes(x = -60, y = 35, label = paste0("n = ", count,"/",nrow(NAPA_MHW_event)))) +
    # Improve on the x and y axis labels
    scale_x_continuous(breaks = seq(-70, -50, 10),
                       labels = c("70°W", "60°W", "50°W"),
                       position = "bottom") +
    scale_y_continuous(breaks = seq(35, 55, 10),
                       labels = scales::unit_format(suffix = "°N", sep = "")) +
    # Slightly shrink the plotting area
    coord_equal(xlim = NWA_corners_sub[1:2],
                ylim = NWA_corners_sub[3:4], expand = F) +
    scale_fill_distiller(palette = "BuPu", direction = -1) +
    labs(x = NULL, y = NULL, fill = "Proportion of events\nper region") +
    theme(legend.position = "bottom")
  # som_prop_plot

  # The meta plot
  # Calculate mean and median per node for plotting
  node_h_lines <- NAPA_MHW_meta %>%
    filter(node == node_number) %>%
    group_by(node) %>%
    summarise(mean_int_cum = mean(intensity_cumulative, na.rm = T),
              median_int_cum = median(intensity_cumulative, na.rm = T),
              mean_int_max = mean(intensity_max, na.rm = T),
              median_int_max = median(intensity_max, na.rm = T))

  # Create the lollis showing season and cum.int.
  seas_cum_lolli_plot <- ggplot(data = NAPA_MHW_meta, aes(x = date_peak, y = intensity_cumulative)) +
    geom_lolli() +
    geom_point(aes(colour = season_peak)) +
    geom_label(aes(x = mean(date_peak), y = max(intensity_cumulative), label = paste0("n = ", count,"/",nrow(NAPA_MHW_event))),
               size = 3, label.padding = unit(0.5, "lines")) +
    geom_smooth(method = "lm", se = F, aes(colour = season_peak)) +
    geom_hline(data = node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
    scale_x_date(labels = scales::date_format("%b-%Y")) +
    labs(x = "", y = "Cumulative intensity (°C x days)", colour = "Season") +
    theme(legend.position = "bottom")
  # seas_cum_lolli_plot

  # Create the lollis showing max.int and region
  region_max_lolli_plot <- ggplot(data = NAPA_MHW_meta, aes(x = date_peak, y = intensity_max)) +
    geom_lolli() +
    geom_point(aes(colour = region)) +
    geom_label(aes(x = mean(date_peak), y = max(intensity_max), label = paste0("n = ", count,"/",nrow(NAPA_MHW_event))),
               size = 3, label.padding = unit(0.5, "lines")) +
    geom_smooth(method = "lm", se = F, aes(colour = region)) +
    scale_x_date(labels = scales::date_format("%b-%Y")) +
    geom_hline(data = node_h_lines, aes(yintercept = mean_int_max), linetype = "dashed") +
    labs(x = "", y = "Max. intensity (°C)", colour = "Region") +
    theme(legend.position = "bottom")
  # region_max_lolli_plot

  # Create title
  title <- cowplot::ggdraw() + cowplot::draw_label(paste0("Node: ",node_number), fontface = 'bold')

  # Stick them together
  fig_all <- cowplot::plot_grid(sst_U_V_panel, qt_panel, taum_mld_panel,
                                som_prop_plot, seas_cum_lolli_plot, region_max_lolli_plot,
                                labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                                nrow = 2, rel_heights = c(1, 1), align = "h")#+
    # cowplot::draw_figure_label(label = paste0("Node: ",node_number), size = 20)
  fig_all_title <- cowplot::plot_grid(title, fig_all, ncol = 1, rel_heights = c(0.05, 1))
  # fig_all_title
  ggsave(fig_all_title, filename = paste0("output/node_",node_number,"_panels.png"), height = 12, width = 16)
  ggsave(fig_all_title, filename = paste0("output/node_",node_number,"_panels.pdf"), height = 12, width = 16)
}


# Extract data from GLORYS NetCDF -----------------------------------------

file_name <- "../data/GLORYS/CMEMS_global-reanalysis-phy-001-030-daily_1993-01.nc"

test <- tidync(file_name) %>%
  activate() %>%
  # hyper_tbl_cube()
  hyper_tibble() %>%
  mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT")))

test_date <- as.POSIXct(3600 * (test$dims$time), origin = '1950-01-01', tz = "GMT")
head(test_date)
length(unique(test_date))

# Test visuals
test %>%
  filter(time == "1993-01-31") %>%
  ggplot(aes(x = longitude, y = latitude, fill = mlotst)) +
  geom_raster()
