# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


# Required bits -----------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))

# Packages used in this script
# library(rlang)
library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse) # Base suite of functions
library(lubridate) # For convenient date manipulation
library(data.table) # For faster mean values
library(heatwaveR, lib.loc = "../R-packages/")
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(tidync, lib.loc = "../R-packages/")

# Set number of cores
doMC::registerDoMC(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# Corners of the study area
NWA_corners <- readRDS("data/NWA_corners.Rda")

# Corners of the study area without Labrador Sea
NWA_nols <- c(-80, -41, 32, 52.5)

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

# The base land polygon
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  select(-region, -subregion)

# The base map frame
frame_base <- ggplot(map_base, aes(x = lon, y = lat)) +
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "top") +
  scale_y_continuous(breaks = c(40, 50),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  coord_cartesian(xlim = c(NWA_nols[1:2]), ylim = c(NWA_nols[3:4]), expand = F) +
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

# Bathymetry data
# NB: This was created in a previous version of the polygon-prep vignette
bathy <- readRDS("data/NWA_bathy_lowres.Rda")

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

# Function for casting wide the data packets
wide_packet <- function(df){
  # Cast the data to a single row
  res <- data.table::data.table(df) %>%
    select(-t) %>%
    reshape2::melt(id = c("region", "event_no", "lon", "lat"),
                   measure = c(colnames(.)[-c(1:4)]),
                   variable.name = "var", value.name = "val") %>%
    dplyr::arrange(var, lon, lat) %>%
    unite(coords, c(lon, lat, var), sep = "BBB") %>%
    unite(event_ID, c(region, event_no), sep = "BBB") %>%
    reshape2::dcast(event_ID ~ coords, value.var = "val")
  # Remove columns (pixels) with missing data
  res_fix <- res[,colSums(is.na(res))<1]
  return(res_fix)
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

  ## ANOSIM for goodness of fit for node count
  node_data_wide <- node_data %>%
    unite(coords, c(lon, lat, var), sep = "BBB") %>%
    data.table() %>%
    dcast(node~coords, value.var = "val")

  # Calculate similarity
  som_anosim <- vegan::anosim(as.matrix(node_data_wide[,-1]),
                              node_data_wide$node, distance = "euclidean")$signif

  # Combine and exit
  res <- list(data = node_data, info = node_info, ANOSIM = paste0("p = ",som_anosim))
  return(res)
}


# Figure data processing --------------------------------------------------

fig_data_func <- function(data_packet){
  # Cast the data wide
  som_data_wide <- data_packet$data %>%
    spread(var, val) %>%
    mutate(mld_anom_cut = cut(mld_anom, breaks = seq(-0.5, 0.5, 0.1)))

  # Reduce wind/ current vectors
  lon_sub <- seq(min(som_data_wide$lon), max(som_data_wide$lon), by = 1)
  lat_sub <- seq(min(som_data_wide$lat), max(som_data_wide$lat), by = 1)

  # currents <- currents[(currents$lon %in% lon_sub & currents$lat %in% lat_sub),]
  som_data_sub <- som_data_wide %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) %>%
    mutate(arrow_size = 0.1)
  # Creating dynamic arrow sizes does not work as ggplot cannot match up the vectorscorrectly

  # MHW season of (peak) occurrence and other meta-data
  OISST_MHW_meta <- OISST_MHW_event %>%
    left_join(data_packet$info, by = c("region", "event_no")) %>%
    filter(region != "ls", node >= 1)

  # Grid of complete node x season matrix
  node_prop_grid <- expand.grid(seq(1:max(OISST_MHW_meta$node, na.rm = T)),
                                c("Summer", "Autumn", "Winter", "Spring"),
                                stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, season_peak = Var2)

  # Proportion of MHWs in each season in each node
  node_prop_info <- OISST_MHW_meta %>%
    dplyr::select(region:event_no, node:season_peak) %>%
    group_by(node, season_peak) %>%
    mutate(node_season_prop = round(n()/count, 2)) %>%
    select(node, count, season_peak:node_season_prop) %>%
    unique() %>%
    na.omit() %>%
    right_join(node_prop_grid, by = c("node", "season_peak")) %>%
    mutate(node_season_prop = ifelse(is.na(node_season_prop), 0, node_season_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()

  # Grid of complete node x season matrix
  region_prop_grid <- expand.grid(seq(1:max(OISST_MHW_meta$node, na.rm = T)),
                                  unique(OISST_MHW_meta$region),
                                  stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, region = Var2)

  # Proportion of MHWs in each season in each region
  region_prop_info <- OISST_MHW_meta %>%
    dplyr::select(region:event_no, node:season_peak) %>%
    group_by(node, region) %>%
    mutate(region_node_prop = round(n()/count, 2)) %>%
    select(region, node, count, region_node_prop) %>%
    unique() %>%
    ungroup() %>%
    right_join(region_prop_grid, by = c("node", "region")) %>%
    mutate(region_node_prop = ifelse(is.na(region_node_prop), 0, region_node_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()

  # Fill in the blanks
  region_prop_grid <- expand.grid(unique(region_prop_info$region),
                                  seq(1:max(OISST_MHW_meta$node, na.rm = T)),
                                  stringsAsFactors = F) %>%
    dplyr::rename(region = Var1, node = Var2) %>%
    left_join(NWA_coords, by = "region") %>%
    left_join(region_prop_info, by = c("region", "node")) %>%
    mutate(count = replace_na(count, 0),
           region_node_prop = replace_na(region_node_prop, 0)) %>%
    filter(region != "ls")

  # Create labels for number of states per region per node
  region_prop_label <- region_prop_grid %>%
    group_by(region, node, count, region_node_prop) %>%
    summarise(lon = mean(lon), lat = mean(lat)) %>%
    mutate(count_region_node = round(count*region_node_prop))

  # Calculate mean and median intensities per node for plotting
  node_h_lines <- OISST_MHW_meta %>%
    group_by(node) %>%
    summarise(mean_int_cum = mean(intensity_cumulative, na.rm = T),
              median_int_cum = median(intensity_cumulative, na.rm = T),
              mean_int_max = mean(intensity_max, na.rm = T),
              median_int_max = median(intensity_max, na.rm = T))

  # Combine and exit
  res <- list(som_data_wide = som_data_wide,
              som_data_sub = som_data_sub,
              OISST_MHW_meta = OISST_MHW_meta,
              node_prop_info = node_prop_info,
              region_prop_grid = region_prop_grid,
              region_prop_label = region_prop_label,
              node_h_lines = node_h_lines)
  return(res)
}


# Figure 2 code -----------------------------------------------------------
# sst + u + v

fig_2_func <- function(fig_data, col_num){

  # The figure
  fig_2 <- frame_base +
    # The ocean temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = sst_anom)) +
    # The bathymetry
    # stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,],
    # aes(x = lon, y = lat, z = depth), alpha = 0.5,
    # colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = fig_data$som_data_sub, aes(xend = lon + u_anom * current_uv_scalar,
                                                   yend = lat + v_anom * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 0.8,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Use diverging gradient
    scale_fill_gradient2(name = "SST\nanom. (°C)", low = "blue", high = "red") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  return(fig_2)
}


# Figure 3 code -----------------------------------------------------------
# Air temp + u + v

fig_3_func <- function(fig_data, col_num){
  fig_3 <- frame_base +
    # The air temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = t2m_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 0.9,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The wind vectors
    geom_segment(data = fig_data$som_data_sub, aes(xend = lon + u10_anom * wind_uv_scalar,
                                          yend = lat + v10_anom * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # Use diverging gradient
    scale_fill_gradient2(name = "Air temp.\nanom. (°C)", low = "blue", high = "red") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  return(fig_3)
}


# Figure 4 code -----------------------------------------------------------
# Net downward heat flux and MLD

fig_4_func <- function(fig_data, col_num){
  fig_4 <- frame_base +
    # The MLD proportion
    geom_raster(data = fig_data$som_data_wide, aes(fill = mld_anom)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 0.8,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # The net downward heat flux contours
    geom_contour(data = fig_data$som_data_wide, binwidth = 50,
                 aes(z = qnet_anom, colour = stat(level)), size = 1) +
    # Colour scale
    scale_fill_gradient2("MLD",low = "blue", high = "red") +
    scale_colour_gradient2("Net downward\nheat flux\nanom. (W/m2)", guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    # The facets
    facet_wrap(~node, ncol = col_num)
  fig_4
  return(fig_4)
}


# Figure 5 code -----------------------------------------------------------
# Events per region and season

fig_5_func <- function(fig_data, col_num){
  fig_5 <- frame_base +
    # The regions
    geom_polygon(data = fig_data$region_prop_grid,
                 aes(group = region, fill = region_node_prop), colour = "black") +
    # The base map
    geom_polygon(data = map_base, aes(group = group), show.legend = F) +
    # Assorted labels
    geom_label(data = fig_data$region_prop_label, aes(label = count_region_node)) +
    geom_label(data = fig_data$region_prop_label,
               aes(x = -68, y = 35, label = paste0("n = ",count))) +
    geom_label(data = filter(fig_data$node_prop_info, season_peak == "Winter"),
               aes(x = -58, y = 37, fill = node_season_prop, label = "Winter"), colour = "black") +
    geom_label(data = filter(fig_data$node_prop_info, season_peak == "Spring"),
               aes(x = -48, y = 37, fill = node_season_prop, label = "Spring"), colour = "black") +
    geom_label(data = filter(fig_data$node_prop_info, season_peak == "Summer"),
               aes(x = -58, y = 35, fill = node_season_prop, label = "Summer"), colour = "black") +
    geom_label(data = filter(fig_data$node_prop_info, season_peak == "Autumn"),
               aes(x = -48, y = 35, fill = node_season_prop, label = "Autumn"), colour = "black") +
    scale_fill_distiller("Proportion\nof events per\nregion/season\nper node",
                         palette = "BuPu", direction = 1) +
    # The facets
    facet_wrap(~node, ncol = col_num)
  return(fig_5)
}


# Figure 6 code -----------------------------------------------------------
# Lollis showing season and cum.int.

fig_6_func <- function(fig_data, col_num){
  fig_6 <- ggplot(data = fig_data$OISST_MHW_meta,
                                aes(x = date_peak, y = intensity_cumulative)) +
    geom_smooth(method = "lm", se = F, aes(colour = season_peak)) +
    geom_lolli() +
    geom_point(aes(colour = season_peak)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
    geom_label(aes(x = mean(range(date_peak)), y = max(intensity_cumulative), label = paste0("n = ", count)),
               size = 3, label.padding = unit(0.5, "lines")) +
    scale_x_date(labels = scales::date_format("%b-%Y"), date_minor_breaks = "1 year") +
    labs(x = "", y = "Cumulative intensity (°C x days)", colour = "Season") +
    theme(legend.position = "bottom") +
    facet_wrap(~node, ncol = col_num)
  return(fig_6)
}


# Figure 7 code -----------------------------------------------------------
# Lollis showing season and cum.int.

fig_7_func <- function(fig_data, col_num){
  fig_7 <- ggplot(data = fig_data$OISST_MHW_meta,
                  aes(x = date_peak, y = intensity_max)) +
    geom_smooth(method = "lm", se = F, aes(colour = region)) +
    geom_lolli() +
    geom_point(aes(colour = region)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_max), linetype = "dashed") +
    geom_label(aes(x = mean(range(date_peak)), y = max(intensity_max), label = paste0("n = ", count)),
               size = 3, label.padding = unit(0.5, "lines")) +
    scale_x_date(labels = scales::date_format("%b-%Y"), date_minor_breaks = "1 year") +
    labs(x = "", y = "Max. intensity (°C)", colour = "Region") +
    theme(legend.position = "bottom") +
    facet_wrap(~node, ncol = col_num)
  return(fig_7)
}


# Create summary figures of all nodes together ----------------------------

# testers...
# som_packet <- readRDS("data/som_nolab.Rda")
# som_packet <- readRDS("data/som_nolab_16.Rda")
# dir_name = "no_ls"
# col_num = 4
# fig_height = 9
# fig_width = 13
som_node_visualise <- function(som_packet,
                               col_num = 4, dir_name = "test",
                               fig_height = 9, fig_width = 13){
  # Check if directory exists and create it if not
  if(!dir.exists(paste0("output/SOM/",dir_name))){
    dir.create(paste0("output/SOM/",dir_name))
  }

  # Base data
  base_data <- fig_data_func(data_packet = som_packet)

  # SST + U + V
  fig_2 <- fig_2_func(base_data, col_num)
  ggsave(fig_2, filename = paste0("output/SOM/",dir_name,"/fig_2.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_2, filename = paste0("output/SOM/",dir_name,"/fig_2.png"), height = fig_height, width = fig_width)

  # Air Temp + U + V
  fig_3 <- fig_3_func(base_data, col_num)
  ggsave(fig_3, filename = paste0("output/SOM/",dir_name,"/fig_3.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_3, filename = paste0("output/SOM/",dir_name,"/fig_3.png"), height = fig_height, width = fig_width)

  # Net downward heat flux and MLD
  fig_4 <- fig_4_func(base_data, col_num)
  ggsave(fig_4, filename = paste0("output/SOM/",dir_name,"/fig_4.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_4, filename = paste0("output/SOM/",dir_name,"/fig_4.png"), height = fig_height, width = fig_width)

  # Events per nregion and season per node
  fig_5 <- fig_5_func(base_data, col_num)
  ggsave(fig_5, filename = paste0("output/SOM/",dir_name,"/fig_5.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_5, filename = paste0("output/SOM/",dir_name,"/fig_5.png"), height = fig_height, width = fig_width)

  # Lollis showing season and cum.int.
  fig_6 <- fig_6_func(base_data, col_num)
  ggsave(fig_6, filename = paste0("output/SOM/",dir_name,"/fig_6.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_6, filename = paste0("output/SOM/",dir_name,"/fig_6.png"), height = fig_height, width = fig_width)

  # Lollis showing season and cum.int.
  fig_7 <- fig_7_func(base_data, col_num)
  ggsave(fig_7, filename = paste0("output/SOM/",dir_name,"/fig_7.pdf"), height = fig_height, width = fig_width)
  ggsave(fig_7, filename = paste0("output/SOM/",dir_name,"/fig_7.png"), height = fig_height, width = fig_width)

  # Create individual node summaries
  plyr::l_ply(1:max(base_data$OISST_MHW_meta$node, na.rm = T), .fun = node_figure, .parallel = T,
              fig_packet = base_data, dir_name = dir_name)
}


# Create a summary figure for a chosen node -------------------------------
## NB: This function contains objects created in "IMBeR_2019_figures.R"

# testers...
# node_number = 8
node_figure <- function(node_number, fig_packet, dir_name){

  # Filter out non-target nodes
  fig_packet$som_data_wide <- filter(fig_packet$som_data_wide, node == node_number)
  fig_packet$som_data_sub <- filter(fig_packet$som_data_sub, node == node_number)
  fig_packet$OISST_MHW_meta <- filter(fig_packet$OISST_MHW_meta, node == node_number)
  fig_packet$node_prop_info <- filter(fig_packet$node_prop_info, node == node_number)
  fig_packet$region_prop_grid <- filter(fig_packet$region_prop_grid, node == node_number)
  fig_packet$region_prop_label <- filter(fig_packet$region_prop_label, node == node_number)
  fig_packet$node_h_lines <- filter(fig_packet$node_h_lines, node == node_number)

  # SST + U + V
  fig_2_sub <- fig_2_func(fig_packet, 1)

  # Air Temp + U + V
  fig_3_sub <- fig_3_func(fig_packet, 1)

  # Net downward heat flux and MLD
  fig_4_sub <- fig_4_func(fig_packet, 1)

  # Events per region and season per node
  fig_5_sub <- fig_5_func(fig_packet, 1)

  # Lollis showing season and cum.int.
  fig_6_sub <- fig_6_func(fig_packet, 1)

  # Lllis showing max.int and region
  fig_7_sub <- fig_7_func(fig_packet, 1)

  # Create title
  title <- cowplot::ggdraw() + cowplot::draw_label(paste0("Node: ",node_number), fontface = 'bold')

  # Stick them together
  fig_all <- cowplot::plot_grid(fig_2_sub, fig_3_sub, fig_4_sub,
                                fig_5_sub, fig_6_sub, fig_7_sub,
                                labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                                nrow = 2, rel_heights = c(1, 1))#, align = "v")#+
    # cowplot::draw_figure_label(label = paste0("Node: ",node_number), size = 20)
  fig_all_title <- cowplot::plot_grid(title, fig_all, ncol = 1, rel_heights = c(0.05, 1))
  # fig_all_title
  # ggsave(fig_all_title, filename = paste0("output/node_",node_number,"_panels.png"), height = 12, width = 16)
  ggsave(fig_all_title, filename = paste0("output/SOM/",dir_name,"/node_",node_number,"_panels.pdf"), height = 9, width = 16)
  ggsave(fig_all_title, filename = paste0("output/SOM/",dir_name,"/node_",node_number,"_panels.png"), height = 9, width = 16)
}
