# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

# Packages used in this script
# library(rlang)
library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse) # Base suite of functions
library(lubridate) # For convenient date manipulation
library(data.table) # For faster mean values
library(heatwaveR, lib.loc = "../R-packages/")
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(ncdf4)
library(tidync, lib.loc = "../R-packages/")
library(yasomi, lib.loc = "../R-packages/")
library(ggforce, lib.loc = "../R-packages/")

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
  # Created in 'analysis/polygon-prep.Rmd'
NWA_corners <- readRDS("data/NWA_corners.Rda")

# Individual regions
  # Created in 'analysis/polygon-prep.Rmd'
NWA_coords <- readRDS("data/NWA_coords.Rda")

# The pixels in each region
  # Created in 'analysis/polygon-prep.Rmd'
NWA_info <- readRDS("data/NWA_info.Rda")

# MHW results
  # Created in 'analysis/sst-prep.Rmd'
OISST_region_MHW <- readRDS("data/OISST_region_MHW.Rda")

# MHW Events
OISST_MHW_event <- OISST_region_MHW %>%
  select(-cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 0) %>%
  unnest(events)

# MHW Categories
suppressWarnings( # Don't need warning about different names for events
  OISST_MHW_cats <- OISST_region_MHW %>%
    select(-events) %>%
    unnest(cats)
)

# The base land polygon
  # Created in 'analysis/polygon-prep.Rmd'
map_base <- readRDS("data/map_base.Rda")

# The base map frame used for all figures
frame_base <- ggplot(map_base, aes(x = lon, y = lat)) +
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "top") +
  scale_y_continuous(breaks = c(40, 50),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  coord_cartesian(xlim = c(NWA_corners[1:2]), ylim = c(NWA_corners[3:4]), expand = F) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))

# Easy join for DOY values
doy_index <- data.frame(doy_int = 1:366,
                        doy = format(seq(as.Date("2016-01-01"),
                                         as.Date("2016-12-31"),
                                         by = "day"), "%m-%d"),
                        stringsAsFactors = F)

# Establish the vector scalar for the currents
current_uv_scalar <- 4

# Establish the vector scalar for the wind
wind_uv_scalar <- 0.5

# Reduced wind/ current vector grid
lon_sub <- seq(NWA_corners[1], NWA_corners[2], by = 1)
lat_sub <- seq(NWA_corners[3], NWA_corners[4], by = 1)

# Bathymetry data
# NB: This was created in a previous version of the polygon-prep vignette
bathy <- readRDS("data/NWA_bathy_lowres.Rda")

# Load anomaly data as necessary
# This also scales each MLD pixel to 1
# NB: It was decided not to first scale the MLD data
# system.time(
#   if(!exists("ALL_anom")) ALL_anom <- readRDS("data/ALL_anom.Rda") #%>%
#     #group_by(lon, lat) %>%
#     #mutate(mld_anom = mld_anom/max(abs(mld_anom), na.rm = T)) %>%
#     #ungroup()
# ) # 99 seconds

# The OISST land mask
# land_mask_OISST <- readRDS("data/land_mask_OISST.Rda")

# Filter it to the smaller domain only
# land_mask_OISST_sub <- land_mask_OISST%>%
#   filter(lon >= NWA_corners[1], lon <= NWA_corners[2],
#          lat >= NWA_corners[3], lat <= NWA_corners[4])


# Filter out desired eddy trajectories ------------------------------------

# nc <- nc_open("data/eddy_trajectory_2.0exp_19930101_20180118.nc")
# eddies <- tibble(lat = round(ncvar_get(nc, varid = "latitude"), 4), # observation longitude
#                  lon = round(ncvar_get(nc, varid = "longitude"), 4), # observation latitude
#                  # days since 1950-01-01 00:00:00 UTC:
#                  time = as.Date(ncvar_get(nc, varid = "time"), origin = "1950-01-01"),
#                  # magnitude of the height difference between (Obs) cm the extremum
#                  # of SLA within the eddy andthe SLA around the contour defining the
#                  # eddy perimeter:
#                  amplitude = round(ncvar_get(nc, varid = "amplitude"), 3),
#                  # flow orientation of eddies -1 is Cyclonic and 1 is Anticyclonic:
#                  cyclonic_type = ncvar_get(nc, varid = "cyclonic_type"),
#                  # observation sequence number, days from eddy start:
#                  observation_number = ncvar_get(nc, varid = "observation_number"),
#                  # flag indicating if the value is interpolated between two observations
#                  # or not (0: observed, 1: interpolated):
#                  observed_flag = ncvar_get(nc, varid = "observed_flag"),
#                  # average speed of the contour defining the radius scale L:
#                  speed_average = ncvar_get(nc, varid = "speed_average"),
#                  # radius of a circle whose area is equal to that enclosed by the
#                  # contour of maximum circum-average speed:
#                  speed_radius = ncvar_get(nc, varid = "speed_radius"),
#                  # eddy identification number:
#                  track = ncvar_get(nc, varid = "track")) %>%
#   mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
#   filter(lon >= NWA_corners[1], lon <= NWA_corners[2],
#          lat >= NWA_corners[3], lat <= NWA_corners[4],)
# nc_close(nc); rm(nc)
# saveRDS(eddies, "data/eddy_tracks.Rds")
eddy_tracks <- readRDS("data/eddy_tracks.Rds")


# Function to create the OISST landmask -----------------------------------

# The land mask NetCDF file is downloaded here:
# https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# OISST_land_mask_func <- function(){
#   lmask <- want_vec <- tidync("data/lsmask.oisst.v2.nc") %>%
#     hyper_tibble() %>%
#     dplyr::select(-time) %>%
#     mutate(lon = ifelse(lon > 180, lon-360, lon))
#   saveRDS(lmask, "data/land_mask_OISST.Rda")
# }

# Test visuals
# ggplot(lmask, aes(x = lon, y = lat)) +
# geom_raster(aes(fill = lsmask))


# Extract data from NOAA OISST NetCDF -------------------------------------

# tester...
# file_name <- "../data/OISST/avhrr-only-v2.ts.0001.nc"
load_OISST <- function(file_name){
  res <- tidync(file_name) %>%
    hyper_filter(lat = dplyr::between(lat, NWA_corners[3]-0.125, NWA_corners[4]-0.125),
                 time = dplyr::between(time, as.integer(as.Date("1993-01-01")),
                                       as.integer(as.Date("2018-12-31")))) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(time, origin = "1970-01-01")) %>%
    dplyr::rename(temp = sst, t = time) %>%
    select(lon, lat, t, temp)
  return(res)
}

load_all_OISST <- function(file_names){
  # system.time(
  res_all <- plyr::ldply(file_names, load_OISST, .parallel = T)
  # ) # 0.5 seconds for one slice, 34 seconds for all
}


# Extract data from GLORYS NetCDF -----------------------------------------

# 1/4 degree data from 1993 to 2015
# testers...
# file_name <- "../data/GLORYS/MHWNWA_GLORYS_quarter_degree_daily_1993-01.nc"
load_GLORYS <- function(file_name){
  res_uv <- tidync(file_name) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT"))) %>%
    select(-depth)
  res_mld <- tidync(file_name) %>%
    activate(mlp) %>% # Need to explicitly grab MLD as it doesn't use the depth dimension
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT")))
  res_all <- left_join(res_uv, res_mld, by = c("longitude", "latitude", "time")) %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time) %>%
    select(lon, lat, t, everything()) %>%
    dplyr::rename(mld = mlp)
  res_round <- res_all %>%
    mutate(mld = round(mld),
           u = round(u, 6), v = round(v, 6))
  return(res_round)
}

load_all_GLORYS <- function(file_names){
  # system.time(
  res_all <- plyr::ldply(file_names, load_GLORYS, .parallel = T)
  # ) # 0.8 seconds for one slice, 45 seconds for all
}

# 1/12 degree data from 2016 to 2018
# testers...
# file_name <- "../data/GLORYS/MHWNWA_GLORYS_twelfth_degree_daily_2016-01.nc"
load_GLORYS_hires <- function(file_name){
  # Process U and V
  res_uv <- tidync(file_name) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT"))) %>%
    select(-depth) %>%
    mutate(longitude = plyr::round_any(longitude, 0.25),
           latitude = plyr::round_any(latitude, 0.25))
  # Switch to data.table for faster means
  res_uv <- data.table(res_uv)
  setkey(res_uv, longitude, latitude, time)
  res_uv <- res_uv[, lapply(.SD, mean), by = list(longitude, latitude, time)]
  # Process MLD
  res_mld <- tidync(file_name) %>%
    activate(mlotst) %>% # Need to explicitly grab MLD as it doesn't use the depth dimension
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT"))) %>%
    mutate(longitude = plyr::round_any(longitude, 0.25),
           latitude = plyr::round_any(latitude, 0.25))
  res_mld <- data.table(res_mld)
  setkey(res_mld, longitude, latitude, time)
  res_mld <- res_mld[, lapply(.SD, mean), by = list(longitude, latitude, time)]
  # Combine
  res_all <- left_join(res_uv, res_mld, by = c("longitude", "latitude", "time")) %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time, u = uo, v = vo) %>%
    select(lon, lat, t, everything()) %>%
    dplyr::rename(mld = mlotst)
  res_round <- res_all %>%
    mutate(mld = round(mld),
           u = round(u, 6), v = round(v, 6))
  return(res_round)
}

load_all_GLORYS_hires <- function(file_names){
  # system.time(
  res_all <- plyr::ldply(file_names, load_GLORYS_hires, .parallel = T)
  # ) # 3.5 seconds for one slice, 19 seconds for all
}

# Test visuals
# res_all %>%
#   filter(t == "1993-01-31") %>%
#   ggplot(aes(x = lon, y = lat, fill = mld)) +
#   geom_raster()


# Extract data from ERA 5 NetCDF ------------------------------------------

# Function for loading a single ERA 5 NetCDF file
# testers...
# file_name <- "../../oliver/data/ERA/ERA5/T2M/ERA5_T2M_1993.nc"
# lon_slice <- -80.5
load_ERA5 <- function(file_name){
  res <- tidync(file_name) %>%
    hyper_filter(latitude = dplyr::between(latitude, 31.5, 63.5),
                 longitude = dplyr::between(longitude, -80.5+360, -40.5+360)) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1900-01-01', tz = "GMT"))) %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time)
  # Switch to data.table for faster means
  res_dt <- data.table(res)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  return(res_mean)
}

# Function to load all of the NetCDF files for one ERA 5 variable
load_all_ERA5 <- function(file_names){
  # system.time(
  res_all <- plyr::ldply(file_names, load_ERA5, .parallel = T)
  # ) # 73 seconds for one year, 119 seconds for two
}


# Calculate climatologies from single variable data.frame -----------------

ts2clm_one <- function(df, GLORYS = F){
  if(ncol(df) > 4) stop("Too many columns")
  if(GLORYS){
    clim_end <- "2018-12-25"
  } else{
    clim_end <- "2018-12-31"
  }
  colnames(df)[4] <- "temp"
  # system.time(
  res <- df %>%
    group_by(lon, lat) %>%
    nest() %>%
    mutate(clims = map(data, ts2clm,
                       climatologyPeriod = c("1993-01-01", clim_end),
                       clmOnly = T, roundClm = FALSE)) %>%
    select(-data) %>%
    unnest() %>%
    left_join(doy_index, by = c("doy" = "doy_int")) %>%
    select(-doy) %>%
    dplyr::rename(doy = doy.y)
  # ) # 1139 seconds
  res$thresh <- NULL
  return(res)
}


# Calculate anomalies for single variable ---------------------------------

anom_one <- function(df, df_clim, point_accuracy){
  if(ncol(df) > 4) stop("Too many columns")
  var_name <- colnames(df)[4]
  colnames(df)[4] <- "temp"
  # point_accuracy <- max(nchar(strsplit(as.character(df$temp)[1:10], "\\.")[[1]][2]))
  res <- df %>%
    mutate(doy = format(t, "%m-%d")) %>%
    left_join(df_clim, by = c("lon", "lat", "doy")) %>%
    mutate(anom = round(temp-seas, point_accuracy)) %>%
    ungroup() %>%
    select(lon, lat, t, anom)
  colnames(res)[4] <- paste0(var_name,"_anom")
  return(res)
}


# Load anomaly data -------------------------------------------------------

# This function loads and filters the anomaly data.frames
# This just exists to reduce redundancy and keep the code/workflow.R script tidy
load_anom <- function(file_name, OISST = F){
  if(OISST){
    res <- readRDS(file_name) %>%
      mutate(lon = lon-0.125, lat = lat+0.125)
  } else{
    res <- readRDS(file_name) %>%
      mutate(lon = ifelse(lon > 180, lon-360, lon))
  }
  res <- res %>% filter(lon >= NWA_corners[1], lon <= NWA_corners[2],
                        lat >= NWA_corners[3], lat <= NWA_corners[4])
  res <- setkey(data.table(res, key = c("lon", "lat", "t")))
  return(res)
}


# Build data packets ------------------------------------------------------

# testers...
# event_sub <- OISST_MHW_event[1,]
data_packet_func <- function(event_sub, df = ALL_anom){

  # Filter base anomally range
  packet_base <- df %>%
    filter(t >= event_sub$date_start, t <= event_sub$date_end,
           lon >= NWA_corners[1], lon <= NWA_corners[2],
           lat >= NWA_corners[3], lat <= NWA_corners[4])
  rm(df); gc()

  # Create mean synoptic values
  packet_mean <- packet_base %>%
    # select(-doy) %>%
    group_by(lon, lat) %>%
    summarise_all(mean, na.rm = T) %>%
    arrange(lon, lat) %>%
    ungroup()
  packet_mean[is.na(packet_mean)] <- NA
  packet_mean <- nest(packet_mean, .key = "synoptic")

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

# Function for casting wide the data packets
wide_packet_func <- function(df){

  # Cast the data to a single row
  res <- select(df, -t) %>%
    data.table::data.table() %>%
    reshape2::melt(id = c("region", "event_no", "lon", "lat"),
                   measure = c(colnames(.)[-c(1:4)]),
                   variable.name = "var", value.name = "val") %>%
    dplyr::arrange(var, lon, lat) %>%
    unite(coords, c(lon, lat, var), sep = "BBB") %>%
    unite(event_ID, c(region, event_no), sep = "BBB") %>%
    reshape2::dcast(event_ID ~ coords, value.var = "val")

  # Remove columns (pixels) with missing data
  res_fix <- res[,colSums(is.na(res))<1]

  # Remove columns (pixels) with no variance
  # This may occur in pixels where there is no variance in MLD anomaly
  no_var <- data.frame(min = sapply(res_fix[,-1], min),
                       max = sapply(res_fix[,-1], max)) %>%
    mutate(col_name = row.names(.)) %>%
    filter(min == max)
  res_filter <- res_fix[,!(colnames(res_fix) %in% no_var$col_name)]

  # Exit
  return(res_filter)
}


# Visualise a single data packet ------------------------------------------

# This function will create a summary figure for a single data packet
# It is based on 'data/SOM/synoptic_states.Rda' and 'data/SOM/synoptic_states_other.Rda'
#testers...
# event_region <- "mab"
# event_num <- 20
fig_data_packet <- function(event_region, event_num){

  # Load and prep mean anom states
  if(!exists("synoptic_states")){
    synoptic_states <- readRDS("data/SOM/synoptic_states.Rda")
  }
  if(!exists("synoptic_states_other")){
    synoptic_states_other <- readRDS("data/SOM/synoptic_states_other.Rda")
  }
  single_packet <- synoptic_states %>%
    filter(region == event_region,
           event_no == event_num) %>%
    unnest(cols = "synoptic")
  single_packet_other <- synoptic_states_other %>%
    filter(region == event_region,
           event_no == event_num) %>%
    unnest(cols = "synoptic")
  single_packet_full <- left_join(single_packet, single_packet_other)

  # Reduce wind/ current vectors
  lon_sub <- seq(min(single_packet_full$lon), max(single_packet_full$lon), by = 1)
  lat_sub <- seq(min(single_packet_full$lat), max(single_packet_full$lat), by = 1)
  vec_sub <- single_packet_full %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) %>%
    na.omit()

  # Establish the vector scalar for the currents
  current_uv_scalar <- 4

  # SUbset region polygon
  NWA_coords_sub <- NWA_coords %>%
    filter(region == event_region)

  # Load and prep SST time series
  OISST_region <- readRDS("data/OISST_region.Rda")
  OISST_region_sub <- OISST_region %>%
    filter(region == event_region) %>%
    ts2clm(climatologyPeriod = c("1993-01-01", "2018-12-31")) %>%
    filter(t >= single_packet_full$date_start[1]-5,
           t <= single_packet_full$date_end[1]+5)

  # MHW segment data.frame
  OISST_segments <- data.frame(date_start = single_packet_full$date_start[1],
                               date_end = single_packet_full$date_end[1],
                               date_peak = single_packet_full$date_peak[1],
                               temp_start = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_start[1]],
                               temp_end = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_end[1]],
                               temp_peak = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_peak[1]])

  # Determine date breaks
  if(nrow(OISST_region_sub) < 60){
    date_breaks_op <- "1 week"
    date_labels_op <- "%d %b %Y"
  } else if(nrow(OISST_region_sub) < 120){
    date_breaks_op <- "1 month"
    date_labels_op <- "%b %Y"
  } else if(nrow(OISST_region_sub) >= 120){
    date_breaks_op <- "2 months"
    date_labels_op <- "%b %Y"
  }

  # SST ts
  ts_panel <- ggplot(data = OISST_region_sub, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh), fill = "salmon") +
    geom_line() +
    geom_line(aes(y = seas), col = "skyblue") +
    geom_line(aes(y = thresh), col = "navy") +
    geom_segment(data = OISST_segments, colour = "springgreen",
                 aes(x = date_start-1, xend = date_start-1,
                     y = temp_start-1, yend = temp_start+1)) +
    geom_segment(data = OISST_segments, colour = "springgreen",
                 aes(x = date_end+1, xend = date_end+1,
                     y = temp_end-1, yend = temp_end+1)) +
    geom_segment(data = OISST_segments, colour = "forestgreen",
                 aes(x = date_peak, xend = date_peak,
                     y = temp_peak-1, yend = temp_peak+1)) +
    scale_x_date(expand = c(0, 0), date_breaks = date_breaks_op, date_labels = date_labels_op, ) +
    labs(x = "Date", y = "Temp. (°C)  ")
  # ts_panel

  # SST + U + V (anom)
  sst_u_v_anom <- frame_base +
    geom_raster(data = single_packet_full, aes(fill = sst_anom)) +
    # The bathymetry
    stat_contour(data = bathy[bathy$depth > -2000,],
    aes(x = lon, y = lat, z = depth), alpha = 0.5,
    colour = "black", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = vec_sub,
                 aes(xend = lon + u_anom * current_uv_scalar,
                     yend = lat + v_anom * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "purple", size = 1.5, alpha = 1) +
    # Diverging gradient
    scale_fill_gradient2(name = "SST\nanom. (°C)", low = "blue", high = "red")
  # sst_u_v_anom

  # Air temp + U10 + V10 + MSLP
  air_u_v_mslp_anom <- frame_base +
    # The air temperature
    geom_raster(data = single_packet_full, aes(fill = t2m_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = single_packet_full,
                 aes(z = msl_anom, colour = stat(level)), size = 1) +
    # The wind vectors
    geom_segment(data = vec_sub,
                 aes(xend = lon + u10_anom * wind_uv_scalar,
                     yend = lat + v10_anom * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "purple", size = 1.5, alpha = 1) +
    # Colour scale
    scale_fill_gradient2(name = "Air temp.\nanom. (°C)", low = "blue", high = "red") +
    scale_colour_gradient2("MSLP anom.\n(hPa)", guide = "legend",
                           low = "green", mid = "grey", high = "yellow")
  # air_u_v_mslp_anom

  # Qnet + MLD
  qnet_mld_anom <- frame_base +
    # The MLD
    geom_raster(data = single_packet_full, aes(fill = mld_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # The net downward heat flux contours
    geom_contour(data = single_packet_full, binwidth = 50,
                 aes(z = qnet_anom, colour = stat(level)), size = 1) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "purple", size = 1.5, alpha = 1) +
    # Colour scale
    scale_fill_gradient2("MLD anom. (m)",low = "blue", high = "red") +
    scale_colour_gradient2("Net downward\nheat flux\nanom. (W/m2)", guide = "legend",
                           low = "green", mid = "grey", high = "yellow")
  # qnet_mld_anom

  # Merge the panels together
  bottom_row <- cowplot::plot_grid(sst_u_v_anom, air_u_v_mslp_anom, qnet_mld_anom, labels = c('B', 'C', 'D'),
                                   align = 'h', rel_widths = c(1, 1, 1), nrow = 1)
  top_row <- cowplot::plot_grid(NULL, ts_panel, NULL, labels = c('', 'A', ''), nrow = 1, rel_widths = c(1,2,1))
  fig_all <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1.3, 3))
  # fig_all
  ggsave(fig_all, filename = paste0("talk/graph/synoptic_",event_region,"_",event_num,".png"), height = 6, width = 16)
  # ggsave(fig_all, filename = paste0("talk/graph/synoptic_",event_region,"_",event_num,".pdf"), height = 6, width = 16)
}


# Run SOM and create summary output ---------------------------------------

som_model_PCI <- function(data_packet, xdim = 4, ydim = 3){
  # Create a scaled matrix for the SOM
  # Cancel out first column as this is the reference ID of the event per row
  data_packet_matrix <- as.matrix(scale(data_packet[,-1]))

  # Create the grid that the SOM will use to determine the number of nodes
  som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal")

  # Run the SOM with PCI
  som_model <- batchsom(data_packet_matrix,
                        somgrid = som_grid,
                        init = "pca",
                        max.iter = 100)

  # Create a data.frame of info
  node_info <- data.frame(event_ID = data_packet[,"event_ID"],
                          node = som_model$classif) %>%
    separate(event_ID, into = c("region", "event_no"), sep = "BBB") %>%
    group_by(node) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    mutate(event_no = as.numeric(as.character(event_no))) %>%
    left_join(select(OISST_MHW_cats, region, event_no, category, peak_date),
              by = c("region", "event_no")) %>%
    mutate(month_peak = lubridate::month(peak_date, label = T),
           season_peak = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                                   month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                                   month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                                   month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn")) %>%
    select(-peak_date, -month_peak)

  # Determine which event goes in which node and melt
  data_packet_long <- cbind(node = som_model$classif, data_packet) %>%
    separate(event_ID, into = c("region", "event_no"), sep = "BBB") %>%
    data.table() %>%
    reshape2::melt(id = c("node", "region", "event_no"),
                   measure = c(colnames(.)[-c(1:3)]),
                   variable.name = "variable", value.name = "value")

  # Create the mean values that serve as the unscaled results from the SOM
  node_data <- data_packet_long[, .(val = mean(value, na.rm = TRUE),
                                    sd = sd(value, na.rm = TRUE)),
                                by = .(node, variable)] %>%
    separate(variable, into = c("lon", "lat", "var"), sep = "BBB") %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat),
           val = round(val, 4),
           sd = round(sd, 4))

  # Load the other synoptic state data as necessary
  if(!exists("synoptic_states_other_unnest")){
    # system.time(
    synoptic_states_other_unnest <- readRDS("data/SOM/synoptic_states_other.Rda") %>%
      select(region, event_no, synoptic) %>%
      unnest(cols = "synoptic")
    # ) # 5 seconds
  }

  # Create mean node states for data not used in the calculation
  other_data <- data_packet_long %>%
    select(node, region, event_no) %>%
    unique() %>%
    mutate(event_no = as.numeric(event_no)) %>%
    left_join(synoptic_states_other_unnest, by = c("region", "event_no")) %>%
    select(-t, -region, -event_no) %>%
    gather(key = "var", value = "val", -c(node:lat)) %>%
    as.data.table() %>%
    .[, .(val = mean(val, na.rm = TRUE),
          sd = sd(val, na.rm = TRUE)),
      by = .(node, lon, lat, var)] %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(val = round(val, 4),
           sd = round(sd, 4))

  # ANOSIM for goodness of fit for node count
  node_data_wide <- node_data %>%
    unite(coords, c(lon, lat, var), sep = "BBB") %>%
    dplyr::select(-sd) %>%
    data.table() %>%
    dcast(node~coords, value.var = "val")

  # Calculate similarity
  # NB: As of Nov 2019 Vegan changed the anosim function
  # it will no longer run if all values are unique, which they are here
  # som_anosim <- vegan::anosim(as.matrix(node_data_wide[,-"node"]),
  #                             node_data_wide$node, distance = "euclidean")$signif

  # Combine and exit
  res <- list(data = node_data,
              other_data = other_data,
              info = node_info)#,
              #ANOSIM = paste0("p = ",som_anosim))
  return(res)
}


# Exract eddy tracks by date range ----------------------------------------

eddy_track_extract <- function(df){
  date_range <- seq(df$date_start, df$date_end, by = "day")
  eddy_track_sub <- filter(eddy_tracks, time %in% date_range)
  return(eddy_track_sub)
}


# Figure data processing --------------------------------------------------

fig_data_prep <- function(data_packet){

  ## Cast the data wide
  # Base data
  som_data_wide <- data_packet$data %>%
    dplyr::select(-sd) %>%
    spread(var, val) #%>%
    # mutate(mld_anom_cut = cut(mld_anom, breaks = seq(-0.5, 0.5, 0.1)))
  other_data_wide <- data_packet$other_data %>%
    dplyr::select(-sd) %>%
    spread(var, val)
  # Variance data
  som_sd_wide <- data_packet$data %>%
    dplyr::select(-val) %>%
    spread(var, sd) #%>%
  # mutate(mld_anom_cut = cut(mld_anom, breaks = seq(-0.5, 0.5, 0.1)))
  other_sd_wide <- data_packet$other_data %>%
    dplyr::select(-val) %>%
    spread(var, sd)

  # currents <- currents[(currents$lon %in% lon_sub & currents$lat %in% lat_sub),]
  som_data_sub <- som_data_wide %>%
    select(node, lon, lat, u_anom, v_anom, u10_anom, v10_anom) %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) #%>%
    # mutate(arrow_size = 0.1)
  # Creating dynamic arrow sizes does not work as ggplot cannot match up the vectors correctly

  # MHW season of (peak) occurrence and other meta-data
  OISST_MHW_meta <- OISST_MHW_event %>%
    left_join(data_packet$info, by = c("region", "event_no"))

  # Grid of complete node x season matrix
  node_season_grid <- expand.grid(seq(1:max(OISST_MHW_meta$node, na.rm = T)),
                                  c("Summer", "Autumn", "Winter", "Spring"),
                                  stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, season_peak = Var2)

  # Proportion of MHWs in each season in each node
  node_season_info <- OISST_MHW_meta %>%
    dplyr::select(region:event_no, node:season_peak) %>%
    group_by(node, season_peak) %>%
    mutate(node_season_count = n(),
           node_season_prop = round(n()/count, 2)) %>%
    select(node, count, season_peak:node_season_prop) %>%
    unique() %>%
    na.omit() %>%
    right_join(node_season_grid, by = c("node", "season_peak")) %>%
    mutate(node_season_count = ifelse(is.na(node_season_count), 0, node_season_count),
           node_season_prop = ifelse(is.na(node_season_prop), 0, node_season_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()

  # Grid of complete node x region matrix
  node_region_grid <- expand.grid(seq(1:max(OISST_MHW_meta$node, na.rm = T)),
                                  unique(OISST_MHW_meta$region),
                                  stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, region = Var2)

  # Proportion of MHWs in each region in each node
  node_region_info <- OISST_MHW_meta %>%
    dplyr::select(region:event_no, node:count) %>%
    group_by(node, region) %>%
    mutate(node_region_count = n(),
           node_region_prop = round(n()/count, 2)) %>%
    select(node, count, region, node_region_count, node_region_prop) %>%
    unique() %>%
    na.omit() %>%
    right_join(node_region_grid, by = c("node", "region")) %>%
    mutate(node_region_count = ifelse(is.na(node_region_count), 0, node_region_count),
           node_region_prop = ifelse(is.na(node_region_prop), 0, node_region_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()

  # Create labels for number of states per region per node
  region_prop_label <- NWA_coords %>%
    group_by(region) %>%
    mutate(lon_center = mean(lon), lat_center = mean(lat)) %>%
    left_join(node_region_info, by = "region") %>%
    na.omit()

  # Calculate mean and median intensities/durations per node for plotting
  node_h_lines <- OISST_MHW_meta %>%
    group_by(node) %>%
    summarise(mean_int_cum = mean(intensity_cumulative, na.rm = T),
              median_int_cum = median(intensity_cumulative, na.rm = T),
              mean_int_max = mean(intensity_max, na.rm = T),
              median_int_max = median(intensity_max, na.rm = T),
              mean_dur = mean(duration, na.rm = T),
              median_dur = median(duration, na.rm = T)) %>%
    mutate_all(round, digits = 2)

  # Load MHW data
  OISST_region_MHW <- readRDS("data/OISST_region_MHW.Rda")
  OISST_MHW_event <- OISST_region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 0) %>%
    unnest(events)

  # Find the eddy tracks present during each MHW
  eddy_data <- data_packet$info %>%
    left_join(OISST_MHW_event, by = c("region", "event_no")) %>%
    group_by(node, region, event_no) %>%
    nest() %>%
    mutate(res = map(data, eddy_track_extract)) %>%
    select(-data) %>%
    unnest(cols = "res") %>%
    mutate(cyclonic_type = factor(cyclonic_type, labels = c("Cyclonic", "Anticyclonic")))

  # Combine and exit
  res <- list(som_data_wide = som_data_wide,
              som_data_sub = som_data_sub,
              other_data_wide = other_data_wide,
              som_sd_wide = som_sd_wide,
              other_sd_wide = other_sd_wide,
              OISST_MHW_meta = OISST_MHW_meta,
              node_season_info = node_season_info,
              node_region_info = node_region_info,
              region_prop_label = region_prop_label,
              node_h_lines = node_h_lines,
              eddy_data = eddy_data)
  return(res)
}


# Create summary figures of all nodes together ----------------------------

# testers...
# som_packet <- readRDS("data/SOM/som.Rda")
# col_num = 4
# fig_height = 9
# fig_width = 13
fig_all_som <- function(som_packet,
                        col_num = 4,
                        fig_height = 9,
                        fig_width = 13){

  # Base data for all figures
  base_data <- fig_data_prep(data_packet = som_packet)

  # Events per nregion and season per node
  region_season <- fig_region_season(base_data, col_num, fig_height, fig_width)

  # SST + U + V (anom)
  sst_u_v_anom <- fig_sst_u_v_anom(base_data, col_num, fig_height, fig_width)

  # Air Temp + U + V (anom)
  air_u_v_mslp_anom <- fig_air_u_v_mslp_anom(base_data, col_num, fig_height, fig_width)

  # Net downward heat flux and MLD (anom)
  qnet_mld_anom <- fig_qnet_mld_anom(base_data, col_num, fig_height, fig_width)

  # Lollis showing cum. int. + season
  cum_int_season <- fig_cum_int_season(base_data, col_num, fig_height, fig_width)

  # Lollis showing max. int. + region
  max_int_region <- fig_max_int_region(base_data, col_num, fig_height, fig_width)

  # SST + U + V (real)
  sst_u_v_real <- fig_sst_u_v_real(base_data, col_num, fig_height, fig_width)

  # Air Temp + U + V (real)
  air_u_v_mslp_real <- fig_air_u_v_mslp_real(base_data, col_num, fig_height, fig_width)

  # Lollis showing duration + rate onset
  duration_rate_onset <- fig_duration_rate_onset(base_data, col_num, fig_height, fig_width)

  # Create SD per pixel plots for env. variables
  sd_col_names <- c(colnames(base_data$som_data_wide),
                    colnames(base_data$other_data_wide))
  sd_col_names <- sd_col_names[-which(sd_col_names %in% c("node", "lon", "lat"))]
  plyr::l_ply(sd_col_names, .fun = fig_sd, .parallel = T,
              fig_data = base_data, col_num = col_num, fig_height, fig_width)

  # Create individual node summaries
  # doMC::registerDoMC(cores = 4)
  plyr::l_ply(1:max(base_data$OISST_MHW_meta$node, na.rm = T), .fun = fig_single_node,
              .progress = "text", .parallel = T,
              fig_packet = base_data, fig_height = fig_height, fig_width = fig_width)
}


# Create a summary figure for a chosen node -------------------------------
## NB: This function contains objects created in "IMBeR_2019_figures.R"

# testers...
# fig_packet <- fig_data_prep(readRDS("data/SOM/som.Rda"))
# node_number = 8
fig_single_node <- function(node_number, fig_packet, fig_height, fig_width){

  # Filter out non-target nodes
  fig_packet$som_data_wide <- filter(fig_packet$som_data_wide, node == node_number)
  fig_packet$som_data_sub <- filter(fig_packet$som_data_sub, node == node_number)
  fig_packet$other_data_wide <- filter(fig_packet$other_data_wide, node == node_number)
  fig_packet$OISST_MHW_meta <- filter(fig_packet$OISST_MHW_meta, node == node_number)
  fig_packet$node_season_info <- filter(fig_packet$node_season_info, node == node_number)
  fig_packet$node_region_info <- filter(fig_packet$node_region_info, node == node_number)
  fig_packet$region_prop_label <- filter(fig_packet$region_prop_label, node == node_number)
  fig_packet$node_h_lines <- filter(fig_packet$node_h_lines, node == node_number)
  fig_packet$eddy_data <- filter(fig_packet$eddy_data, node == node_number)

  # Events per region and season per node
  region_season_sub <- fig_region_season(fig_packet, 1, fig_height, fig_width)

  # SST + U + V (anom)
  sst_u_v_anom_sub <- fig_sst_u_v_anom(fig_packet, 1, fig_height, fig_width)

  # Air Temp + U + V (anom)
  air_u_v_mslp_anom_sub <- fig_air_u_v_mslp_anom(fig_packet, 1, fig_height, fig_width)

  # Net downward heat flux and MLD (anom)
  qnet_mld_anom_sub <- fig_qnet_mld_anom(fig_packet, 1, fig_height, fig_width)

  # SST + U + V (real)
  sst_u_v_real_sub <- fig_sst_u_v_real(fig_packet, 1, fig_height, fig_width)

  # Air Temp + U + V (real)
  air_u_v_mslp_real_sub <- fig_air_u_v_mslp_real(fig_packet, 1, fig_height, fig_width)

  # Lollis showing cum.int. + season
  cum_int_season_sub <- fig_cum_int_season(fig_packet, 1, fig_height, fig_width)

  # Lollis showing max.int + region
  max_int_region_sub <- fig_max_int_region(fig_packet, 1, fig_height, fig_width)

  # Lollis showing duration + rate onset
  duration_rate_onset_sub <- fig_duration_rate_onset(fig_packet, 1, fig_height, fig_width)

  # Create title
  title <- cowplot::ggdraw() + cowplot::draw_label(paste0("Node: ",node_number), fontface = 'bold')

  # Stick them together
  fig_all_sub <- cowplot::plot_grid(region_season_sub, sst_u_v_anom_sub, air_u_v_mslp_anom_sub,
                                    qnet_mld_anom_sub, sst_u_v_real_sub, air_u_v_mslp_real_sub,
                                    cum_int_season_sub, max_int_region_sub, duration_rate_onset_sub,
                                    labels = c('A)', 'B)', 'C)', 'D)', 'E)', 'F)', 'G)', 'H)', 'I)'),
                                    nrow = 3, rel_heights = c(1, 1), axis = "lt", align = "hv")#+
    # cowplot::draw_figure_label(label = paste0("Node: ",node_number), size = 20)
  fig_all_title <- cowplot::plot_grid(title, fig_all_sub, ncol = 1, rel_heights = c(0.05, 1))
  # fig_all_title
  # ggsave(fig_all_title, filename = paste0("output/node_",node_number,"_panels.png"), height = 12, width = 16)
  ggsave(fig_all_title, filename = paste0("output/SOM/node_",node_number,"_panels.pdf"), height = 14, width = 21)
  ggsave(fig_all_title, filename = paste0("output/SOM/node_",node_number,"_panels.png"), height = 14, width = 21)
}


# Counts per region and season figure -------------------------------------

# testers...
# fig_data <- fig_data_prep(readRDS("data/SOM/som.Rda"))
# col_num = 4
fig_region_season <- function(fig_data, col_num, fig_height = fig_height, fig_width = fig_width){
  # The figure
  region_season <- frame_base +
    # The regions
    geom_polygon(data = fig_data$region_prop_label,
                 aes(group = region, fill = node_region_prop), colour = "black") +
    # Eddy tracks
    # NB: This looks aweful. I turned it off for some figures for a talk
    # geom_point(data = fig_data$eddy_data, aes(x = lon, y = lat),
               # colour = "darkorange", alpha = 0.3, size = 0.1) +
    # The base map
    geom_polygon(data = map_base, aes(group = group), show.legend = F) +
    # Count per region
    geom_label(data = fig_data$region_prop_label,
               aes(x = lon_center, y = lat_center, label = node_region_count)) +
    # Overall node count
    geom_label(data = fig_data$region_prop_label,
               aes(x = -68, y = 36, label = paste0("n = ",count))) +
    # Winter count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Winter"),
               aes(x = -58, y = 38, fill = node_season_prop,
                   label = paste0("Winter\n n = ",node_season_count))) +
    # Spring count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Spring"),
               aes(x = -48, y = 38, fill = node_season_prop,
                   label = paste0("Spring\n n = ",node_season_count))) +
    # Summer count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Summer"),
               aes(x = -58, y = 34, fill = node_season_prop,
                   label = paste0("Summer\n n = ",node_season_count))) +
    # Autumn count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Autumn"),
               aes(x = -48, y = 34, fill = node_season_prop,
                   label = paste0("Autumn\n n = ",node_season_count))) +
    # Colour scale
    scale_fill_distiller("Proportion\nof events per\nregion/season\nper node",
                         palette = "BuPu", direction = 1) +
    # The facets
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/region_season.pdf", region_season, height = fig_height, width = fig_width)
    ggsave("output/SOM/region_season.png", region_season, height = fig_height, width = fig_width)
  }
  return(region_season)
}


# SST + U + V (anom) figure -----------------------------------------------

fig_sst_u_v_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  sst_u_v_anom <- frame_base +
    # The ocean temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = sst_anom)) +
    # The region polygons
    geom_polygon(data = NWA_coords, aes(group = region),
                 fill = NA, colour = "black", size = 1, alpha = 0.2) +
    # The bathymetry
    # stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,],
    # aes(x = lon, y = lat, z = depth), alpha = 0.5,
    # colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = fig_data$som_data_sub,
                 aes(xend = lon + u_anom * current_uv_scalar,
                     yend = lat + v_anom * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Diverging gradient
    scale_fill_gradient2(name = "SST\nanom. (°C)", low = "blue", high = "red") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/sst_u_v_anom.pdf", sst_u_v_anom, height = fig_height, width = fig_width)
    ggsave("output/SOM/sst_u_v_anom.png", sst_u_v_anom, height = fig_height, width = fig_width)
  }
  return(sst_u_v_anom)
}


# Air temp + U + V + MSLP (anom) figure -----------------------------------

fig_air_u_v_mslp_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  air_u_v_mslp_anom <- frame_base +
    # The air temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = t2m_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = fig_data$other_data_wide,
                 aes(z = msl_anom, colour = stat(level)), size = 1) +
    # The wind vectors
    geom_segment(data = fig_data$som_data_sub,
                 aes(xend = lon + u10_anom * wind_uv_scalar,
                     yend = lat + v10_anom * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # Colour scale
    scale_fill_gradient2(name = "Air temp.\nanom. (°C)", low = "blue", high = "red") +
    scale_colour_gradient2("MSLP anom.", guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/air_u_v_mslp_anom.pdf", air_u_v_mslp_anom, height = fig_height, width = fig_width)
    ggsave("output/SOM/air_u_v_mslp_anom.png", air_u_v_mslp_anom, height = fig_height, width = fig_width)
  }
  return(air_u_v_mslp_anom)
}


# QNET + MLD (anom) figure ------------------------------------------------

fig_qnet_mld_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  qnet_mld_anom <- frame_base +
    # The MLD
    geom_raster(data = fig_data$som_data_wide, aes(fill = mld_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # The net downward heat flux contours
    geom_contour(data = fig_data$som_data_wide, binwidth = 50,
                 aes(z = qnet_anom, colour = stat(level)), size = 1) +
    # Colour scale
    scale_fill_gradient2("MLD anom. (m)",low = "blue", high = "red") +
    scale_colour_gradient2("Net downward\nheat flux\nanom. (W/m2)", guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  ggsave("output/SOM/qnet_mld_anom.pdf", qnet_mld_anom, height = fig_height, width = fig_width)
  ggsave("output/SOM/qnet_mld_anom.png", qnet_mld_anom, height = fig_height, width = fig_width)
  return(qnet_mld_anom)
}


# SST + U + V (real) figure -----------------------------------------------

fig_sst_u_v_real <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  sst_u_v_real <- frame_base +
    # The ocean temperature
    geom_raster(data = fig_data$other_data_wide, aes(fill = sst)) +
    # The bathymetry
    # stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,],
    # aes(x = lon, y = lat, z = depth), alpha = 0.5,
    # colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = fig_data$other_data_wide,
                 aes(xend = lon + u * current_uv_scalar,
                     yend = lat + v * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Diverging gradient
    scale_fill_viridis_c(name = "SST (°C)", option = "D") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/sst_u_v_real.pdf", sst_u_v_real, height = fig_height, width = fig_width)
    ggsave("output/SOM/sst_u_v_real.png", sst_u_v_real, height = fig_height, width = fig_width)
  }
  return(sst_u_v_real)
}


# Air temp + U + V + MSLP (real) figure -----------------------------------

fig_air_u_v_mslp_real <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  air_u_v_mslp_real <- frame_base +
    # The air temperature
    geom_raster(data = fig_data$other_data_wide, aes(fill = t2m)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = fig_data$other_data_wide,
                 aes(z = msl, colour = stat(level)), size = 1) +
    # The wind vectors
    geom_segment(data = fig_data$other_data_wide,
                 aes(xend = lon + u10 * wind_uv_scalar,
                     yend = lat + v10 * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # Colour scale
    scale_fill_viridis_c(name = "Air temp. (°C)", option = "A") +
    scale_colour_gradient("MSLP", guide = "legend", low = "white", high =  "black") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/air_u_v_mslp_real.pdf", air_u_v_mslp_real, height = fig_height, width = fig_width)
    ggsave("output/SOM/air_u_v_mslp_real.png", air_u_v_mslp_real, height = fig_height, width = fig_width)
  }
  return(air_u_v_mslp_real)
}



# SD figures --------------------------------------------------------------

fig_sd <- function(col_name, fig_data, col_num, fig_height, fig_width){

  # Pick correct data.frame from list
  if({{col_name}} %in% colnames(fig_data$som_sd_wide)){
    sub_data <- fig_data$som_sd_wide
  } else{
    sub_data <- fig_data$other_sd_wide
  }

  # The figure
  sd_heat_map <- frame_base +
    # The MLD
    geom_raster(data = sub_data, aes_string(fill = {{col_name}})) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # Colour scale
    scale_fill_gradient2(paste0("SD: ",{{col_name}}),low = "white", high = "red") +
    # The facets
    facet_wrap(~node, ncol = col_num) +
    # Etc.
    theme(legend.position = "bottom")
  if(col_num != 1){
    ggsave(paste0("output/SOM/",col_name,"_sd.pdf"), sd_heat_map, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/",col_name,"_sd.png"), sd_heat_map, height = fig_height, width = fig_width)
  }
  # return(sd_heat_map)
}


# Cum. int. + season lolli ------------------------------------------------

fig_cum_int_season <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  cum_int_seas <- ggplot(data = fig_data$OISST_MHW_meta,
                  aes(x = date_peak, y = intensity_cumulative)) +
    geom_smooth(method = "lm", se = F, aes(colour = season_peak)) +
    # Count label
    geom_label(aes(x = mean(range(date_peak)),
                   y = max(intensity_cumulative),
                   label = paste0("n = ", count))) +
    # Mean stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$intensity_cumulative)*0.9,
                   label = paste0("mean = ", mean_int_cum))) +
    # Median stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$intensity_cumulative)*0.8,
                   label = paste0("median = ", median_int_cum))) +
    geom_lolli() +
    geom_point(aes(colour = season_peak)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_int_cum), linetype = "dotted") +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    labs(x = "", y = "Cumulative intensity (°C x days)", colour = "Season") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30)) +
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/cum_int_season.pdf", cum_int_seas, height = fig_height, width = fig_width)
    ggsave("output/SOM/cum_int_season.png", cum_int_seas, height = fig_height, width = fig_width)
  }
  return(cum_int_seas)
}


# Max. int. + region lolli ------------------------------------------------

fig_max_int_region <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  max_int_region <- ggplot(data = fig_data$OISST_MHW_meta,
                  aes(x = date_peak, y = intensity_max)) +
    geom_smooth(method = "lm", se = F, aes(colour = region)) +
    # Count label
    geom_label(aes(x = mean(range(date_peak)),
                   y = max(intensity_max),
                   label = paste0("n = ", count))) +
    # Mean stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$intensity_max)*0.9,
                   label = paste0("mean = ", mean_int_max))) +
    # Median stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$intensity_max)*0.8,
                   label = paste0("median = ", median_int_max))) +
    geom_lolli() +
    geom_point(aes(colour = region)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_max), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_int_max), linetype = "dotted") +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    labs(x = "", y = "Max. intensity (°C)", colour = "Region") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30)) +
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/max_int_region.pdf", max_int_region, height = fig_height, width = fig_width)
    ggsave("output/SOM/max_int_region.png", max_int_region, height = fig_height, width = fig_width)
  }
  return(max_int_region)
}


# Rate onset + duration lolli ---------------------------------------------

fig_duration_rate_onset <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  duration_rate_onset <- ggplot(data = fig_data$OISST_MHW_meta,
                           aes(x = date_peak, y = duration)) +
    # geom_smooth(method = "lm", se = F, aes(colour = region)) +
    # Count label
    geom_label(aes(x = mean(range(date_peak)),
                   y = max(duration),
                   label = paste0("n = ", count))) +
    # Mean stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$duration)*0.9,
                   label = paste0("mean = ", mean_dur))) +
    # Median stat label
    geom_label(data = fig_data$node_h_lines,
               aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
                   y = max(fig_data$OISST_MHW_meta$duration)*0.8,
                   label = paste0("median = ", median_dur))) +
    geom_lolli() +
    geom_point(aes(colour = rate_onset)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_dur), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_dur), linetype = "dotted") +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    scale_colour_gradient(low = "white", high = "darkorange") +
    labs(x = "", y = "Duration (days)", colour = "Rate onset (°C x days)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30)) +
    facet_wrap(~node, ncol = col_num)
  if(col_num != 1){
    ggsave("output/SOM/duration_rate_onset.pdf", duration_rate_onset, height = fig_height, width = fig_width)
    ggsave("output/SOM/duration_rate_onset.png", duration_rate_onset, height = fig_height, width = fig_width)
  }
  return(duration_rate_onset)
}


# Summary stat heat map ---------------------------------------------------

# Show with this figure the different mean and median values for the main
# summary stats as a proportion (0 -- 1) so that nodes may be quickly
# compared visually
fig_heat_stat <- function(){
  heat_data <- fig_data_prep(readRDS("data/SOM/som.Rda"))$node_h_lines
  heat_data_long <- heat_data %>%
    gather(key = "stat", value = "val", -node) %>%
    group_by(stat) %>%
    mutate(prop = val/max(val))
  heat_stat <- ggplot(heat_data_long, aes(x = 1, y = stat)) +
    geom_tile(aes(fill = prop), colour = "black") +
    geom_hline(aes(yintercept = 3.5)) +
    scale_fill_gradient(low = "white", high = "red") +
    facet_wrap(~node, ncol = 4) +
    labs(y = NULL, x = NULL, fill = "prop.") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  heat_stat
}
# fig_heat_stat()
# ggsave("output/SOM/heat_stat.pdf", height = 4, width = 6)
# ggsave("output/SOM/heat_stat.png", height = 4, width = 6)
# ggsave("docs/assets/SOM/heat_stat.png", height = 4, width = 6)


# Schematic of all variables at once --------------------------------------

# Create custom data.frames to create schematic figure
# This will use a series of different types of shapes to denote the primary pattern seen in each of the important abiotic variables
# Use an ellipse to show where the occurrence of MHWs is centred
# Could use colour fill gradient to make it more apparent
# Use two centre points to stretch the ellipse, with one of them always on the ss region
# Use curvy arrows to show general wind patterns
# Use coloured contours to show QNET and/or MSLP
# Use +&- signs to show MLD
# Perhaps denote current anomalies, but they aren’t very remarkable
# The broad changes in patterns can be shown with large arrows running along the sides of the figure
# Look into the possibility of using different types of cross-hatching within ellipses
# To show the season of occurrence include a rug plot or density polygon at the bottom of the season/date_peak of each event
# May be good to include a label showing summary stats somehow
# First copy into code the stuff written on the white board
# Then draw pre-renders on the white boars based on the node summary text written down

fig_schematic <- function(){
  region_data <- data.frame(node = 1,
                            lon = -56,
                            lat = 47,
                            a = 15,
                            b = 5,
                            angle = 170)

  # schematic_data <- dta.frame(node = 1,
  #                             region = c(),
  #                             wind_arrows(),
  #                             mslp = c(),
  #                             qnet = c(),
  #                             mld = c())
  schematic <- frame_base +
  # schematic <- ggplot() +
    # The land mass
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), alpha = 0.9,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # stat_ellipse(data = region_data) +
    geom_ellipse(data = region_data, aes(x0 = lon, y0 = lat, a = a, b = b, angle = angle*pi/180), fill = "purple", alpha = 0.2) +
    # geom_polygon() +
    # ggforce::geom_circle(data = region_data, aes(x0 = lon, y0 = lat, r = r), fill = "black") +
    facet_wrap(~node)
  schematic


}
