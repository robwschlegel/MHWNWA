# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))

# Packages used in this script
# library(rlang)
library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse) # Base suite of functions
# library(ncdf4) # For opening and working with NetCDF files
library(lubridate) # For convenient date manipulation
library(data.table) # For faster mean values
library(heatwaveR, lib.loc = "../R-packages/")
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(tidync, lib.loc = "../R-packages/")
# library(akima) # For finding pixels next to missing pixels
# library(FNN) # For finding pixels next to missing pixels
# library(cowplot) # For gridding multiple figures together

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
NWA_corners <- readRDS("data/NWA_corners.Rda")

# Individual regions
NWA_coords <- readRDS("data/NWA_coords_cabot.Rda")

# The pixels in each region
NWA_info <- readRDS("data/NWA_info.Rda")

# MHW results
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

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  select(-region, -subregion)

# Easy join for DOY values
doy_index <- data.frame(doy_int = 1:366,
                        doy = format(seq(as.Date("2016-01-01"),
                                         as.Date("2016-12-31"),
                                         by = "day"), "%m-%d"),
                        stringsAsFactors = F)

# Load anomaly data as necessary
# This also scales each MLD pixel to 1
# system.time(
#   if(!exists("ALL_anom")) ALL_anom <- readRDS("data/ALL_anom.Rda") %>%
#     group_by(lon, lat) %>%
#     mutate(mld_anom = mld_anom/max(abs(mld_anom), na.rm = T)) %>%
#     ungroup()
# ) # 99 seconds

# The OISST land mask
# land_mask_OISST <- readRDS("data/land_mask_OISST.Rda")

# Filter it to the smaller domain only
# land_mask_OISST_sub <- land_mask_OISST%>%
#   filter(lon >= NWA_corners[1], lon <= NWA_corners[2],
#          lat >= NWA_corners[3], lat <= NWA_corners[4])


# Function to create the OISST landmask -----------------------------------

# The land mask NetCDF file is downloaded here:
# https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
OISST_land_mask_func <- function(){
  lmask <- want_vec <- tidync("data/lsmask.oisst.v2.nc") %>%
    hyper_tibble() %>%
    dplyr::select(-time) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon))
  saveRDS(lmask, "data/land_mask_OISST.Rda")
}

# Test visuals
# ggplot(lmask, aes(x = lon, y = lat)) +
# geom_raster(aes(fill = lsmask))


# Extract data from NOAA OISST NetCDF -------------------------------------

# file_name <- "../data/OISST/avhrr-only-v2.ts.0001.nc"
load_OISST <- function(file_name){
  res <- tidync(file_name) %>%
    hyper_filter(lat = dplyr::between(lat, 31.5-0.125, 63.5-0.125),
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
# Cycles through one lon slice at a time as the hourly data are too cumbersome otherwise
# file_name <- "../../oliver/data/ERA/ERA5/T2M/ERA5_T2M_1993.nc"
# lon_slice <- -80.5
load_ERA5 <- function(lon_slice, file_name){
  res <- tidync(file_name) %>%
    hyper_filter(latitude = dplyr::between(latitude, 31.5, 63.5),
                 longitude = longitude == lon_slice+360) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1900-01-01', tz = "GMT"))) %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time)
  # Switch to data.table for faster means
  res_dt <- data.table(res)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  return(res_mean)
}

# Lon range: -80.5 to -40.5
load_one_ERA5 <- function(file_name){
  # system.time(
  res_one <- plyr::ldply(seq(-80.5, -40.5, by = 0.25), load_ERA5,
                         .parallel = T, file_name = file_name)
  # ) # 117 seconds
}

# Function to load all of the NetCDF files for one ERA 5 variable
load_all_ERA5 <- function(file_names){
  # system.time(
  res_all <- plyr::ldply(file_names, load_one_ERA5,
                         .parallel = F, .progress = "text")
  # ) # 116 seconds for one year, 222 seconds for two
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
  res <- res %>% filter(lon >= -80, lon <= -41,
                        lat >= 32, lat <= 63)
  res <- setkey(data.table(res, key = c("lon", "lat", "t")))
  return(res)
}


# Build data packets ------------------------------------------------------

# testers...
# event_sub <- OISST_MHW_event[1,]
data_packet <- function(event_sub){

  # Filter base anomally range
  packet_base <- ALL_anom %>%
    filter(t >= event_sub$date_start, t <= event_sub$date_end)

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
  node_data <- data_packet_long[, .(val = mean(value, na.rm = TRUE)),
                                by = .(node, variable)] %>%
    separate(variable, into = c("lon", "lat", "var"), sep = "BBB") %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat),
           val = round(val, 4))

  # Combine and exit
  res <- list(data = node_data, info = node_info)
  return(node_data)
}


# Create a summary figure for a chosen node -------------------------------
## NB: This function contains objects created in "IMBeR_2019_figures.R"

node_figure <- function(node_number, node_index){
  sub_synoptic_state <- synoptic_states %>%
    ungroup() %>%
    left_join(node_index, by = c("region", "event_no")) %>%
    filter(node == node_number) %>%
    dplyr::select(-region, -event_no, -node, -count) %>%
    group_by(lon, lat) %>%
    # dplyr::rename(u_anom = uoce_anom, v_anom = voce_anom) %>%
    # group_by(lon, lat) %>%
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
