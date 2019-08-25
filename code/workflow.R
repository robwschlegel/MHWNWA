# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
# workflowr::wflow_publish(files = c("analysis/index.Rmd", "analysis/polygon-prep.Rmd",
#                                    "analysis/sst-prep.Rmd", "analysis/var-prep.Rmd",
#                                    "analysis/som.Rmd",
#                                    # "analysis/figures.Rmd",
#                                    "analysis/node-summary.Rmd"),
#                          message = "Re-publish entire site.")
# ) # 70 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Study area and regions" vignette


# MHW detection -----------------------------------------------------------

# See the "MHW detection" vignette


# Create net heat flux (qnet) variable ------------------------------------
# This requires combining four full variables from the ERA 5 data

# nc_info <- ncdump::NetCDF(ERA5_lhf_files[1])$variable$name

## Long wave radiation
# "msnlwrf"
# print(paste0("Began loading msnlwrf at ", Sys.time()))
# ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
# ERA5_lwr <- load_all_ERA5(ERA5_lwr_files) %>%
#   mutate(msnlwrf = round(msnlwrf, 6))
# saveRDS(ERA5_lwr, "data/ERA5_lwr.Rda")

## Short wave radiation
# "msnswrf"
# print(paste0("Began loading msnswrf at ", Sys.time()))
# ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
# ERA5_swr <- load_all_ERA5(ERA5_swr_files) %>%
#   mutate(msnswrf = round(msnswrf, 6))
# saveRDS(ERA5_swr, "data/ERA5_swr.Rda")

## Latent heat flux
# "mslhf"
# print(paste0("Began loading mslhf at ", Sys.time()))
# ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
# ERA5_lhf <- load_all_ERA5(ERA5_lhf_files) %>%
#   mutate(mslhf = round(mslhf, 6))
# saveRDS(ERA5_lhf, "data/ERA5_lhf.Rda")

## Sensible heat flux
# "msshf"
# print(paste0("Began loading msshf at ", Sys.time()))
# ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
# ERA5_shf <- load_all_ERA5(ERA5_shf_files) %>%
#   mutate(msshf = round(msshf, 6))
# saveRDS(ERA5_shf, "data/ERA5_shf.Rda")

## Net heat flux (not including the advection term, which is accounted for on its own)
# print(paste0("Began creating qnet at ", Sys.time()))
# ERA5_qnet <- left_join(ERA5_lwr, ERA5_swr, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_lhf, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_shf, by = c("lon", "lat", "t")) %>%
#   mutate(qnet = msnlwrf+msnswrf+mslhf+msshf) %>%
#   select(lon, lat, t, qnet) %>%
#   mutate(qnet = round(qnet, 6))
# saveRDS(ERA5_qnet, "data/ERA5_qnet.Rda")
# print(paste0("Began qnet clims at ", Sys.time()))
# ERA5_qnet_clim <- ts2clm_one(ERA5_qnet)
# saveRDS(ERA5_qnet_clim, "data/ERA5_qnet_clim.Rda")
# print(paste0("Began qnet anomalies at ", Sys.time()))
# ERA5_qnet_anom <- anom_one(ERA5_qnet, ERA5_qnet_clim, 6)
# saveRDS(ERA5_qnet_anom, "data/ERA5_qnet_anom.Rda")


# Variable loading, clims, and anomalies ----------------------------------

# nc_info <- ncdump::NetCDF(ERA5_u_files[1])$variable$name

# Set number of cores
  # NB: Can't hold all files in memmory at once
# doMC::registerDoMC(cores = 10)

### OISST data processing
## Sea surface temperature
# OISST_files <- OISST_files <- dir("../../data/OISST", full.names = T)
# OISST_sst <- load_all_OISST(OISST_files)
# saveRDS(OISST_sst, "data/OISST_sst.Rda")
# OISST_sst_clim <- ts2clm_one(OISST_sst)
# saveRDS(OISST_sst_clim, "data/OISST_sst_clim.Rda")
# OISST_sst_anom <- anom_one(OISST_sst, OISST_sst_clim, 2)
# saveRDS(OISST_sst_anom, "data/OISST_sst_anom.Rda")

### ERA 5 data processing
## Air temperature at 2 metres
# Grab file location
# print(paste0("Began loading t2m at ", Sys.time()))
# ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")[15:40]
# ERA5_t2m <- load_all_ERA5(ERA5_t2m_files) %>%
#   mutate(t2m = round(t2m, 2)-272.15)
# saveRDS(ERA5_t2m, "data/ERA5_t2m.Rda")
# print(paste0("Began creating t2m clims at ", Sys.time()))
# ERA5_t2m_clim <- ts2clm_one(ERA5_t2m)
# saveRDS(ERA5_t2m_clim, "data/ERA5_t2m_clim.Rda")
# print(paste0("Began t2m anomalies at ", Sys.time()))
# ERA5_t2m_anom <- anom_one(ERA5_t2m, ERA5_t2m_clim, 2)
# saveRDS(ERA5_t2m_anom, "data/ERA5_t2m_anom.Rda")

## Surface winds U component
# "u10"
# print(paste0("Began loading u10 at ", Sys.time()))
# ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")[15:40]
# ERA5_u <- load_all_ERA5(ERA5_u_files) %>%
  # mutate(u10 = round(u10, 6))
# saveRDS(ERA5_u, "data/ERA5_u.Rda")
# print(paste0("Began u10 clims at ", Sys.time()))
# ERA5_u_clim <- ts2clm_one(ERA5_u)
# saveRDS(ERA5_u_clim, "data/ERA5_u_clim.Rda")
# print(paste0("Began u10 anomalies at ", Sys.time()))
# ERA5_u_anom <- anom_one(ERA5_u, ERA5_u_clim, 6)
# saveRDS(ERA5_u_anom, "data/ERA5_u_anom.Rda")

## Surface winds V component
# "v10"
# print(paste0("Began loading v10 at ", Sys.time()))
# ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")[15:40]
# ERA5_v <- load_all_ERA5(ERA5_v_files) %>%
  # mutate(v10 = round(v10, 6))
# saveRDS(ERA5_v, "data/ERA5_v.Rda")
# print(paste0("Began v10 clims at ", Sys.time()))
# ERA5_v_clim <- ts2clm_one(ERA5_v)
# saveRDS(ERA5_v_clim, "data/ERA5_v_clim.Rda")
# print(paste0("Began v10 anomalies at ", Sys.time()))
# ERA5_v_anom <- anom_one(ERA5_v, ERA5_v_clim, 6)
# saveRDS(ERA5_v_anom, "data/ERA5_v_anom.Rda")

## Mean sea level pressure
# "v10"
# print(paste0("Began loading MSLP at ", Sys.time()))
# ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")[15:40]
# ERA5_mslp <- load_all_ERA5(ERA5_mslp_files)
# saveRDS(ERA5_mslp, "data/ERA5_mslp.Rda")
# print(paste0("Began MSLP clims at ", Sys.time()))
# ERA5_mslp_clim <- ts2clm_one(ERA5_mslp)
# saveRDS(ERA5_mslp_clim, "data/ERA5_mslp_clim.Rda")
# print(paste0("Began MSLP anomalies at ", Sys.time()))
# ERA5_mslp_anom <- anom_one(ERA5_mslp, ERA5_mslp_clim, 2)
# saveRDS(ERA5_mslp_anom, "data/ERA5_mslp_anom.Rda")

### GLORYS data processing
## Load high-res GLORYS data
  # NB: This automatically constrains the data to the same 1/4 degree grid as the low-res data
# GLORYS_high_res_files <- dir("../data/GLORYS", full.names = T, pattern = "twelfth")
# system.time(
# GLORYS_all <- load_all_GLORYS_hires(GLORYS_high_res_files)
# ) # 83 seconds
## test visuals
# ggplot(filter(GLORYS_all, t == "1998-01-01"), aes(x = lon, y = lat, fill = mld)) +
#   geom_raster() +
#   scale_fill_gradient2()
### Separate into three variables
## Surface currents U
# GLORYS_u <- select(GLORYS_all, lon, lat, t, u)
# saveRDS(GLORYS_u, "data/GLORYS_u.Rda")
## Surface currents V
# GLORYS_v <- select(GLORYS_all, lon, lat, t, v)
# saveRDS(GLORYS_v, "data/GLORYS_v.Rda")
## Mixed layer depth
# GLORYS_mld <- select(GLORYS_all, lon, lat, t, mld)
# saveRDS(GLORYS_mld, "data/GLORYS_mld.Rda")
## Calculate climatologies
# print(paste0("Began u clims at ", Sys.time()))
# GLORYS_u <- readRDS("data/GLORYS_u.Rda")
# GLORYS_u_clim <- ts2clm_one(GLORYS_u, GLORYS = T)
# saveRDS(GLORYS_u_clim, "data/GLORYS_u_clim.Rda")
# print(paste0("Began v clims at ", Sys.time()))
# GLORYS_v <- readRDS("data/GLORYS_v.Rda")
# GLORYS_v_clim <- ts2clm_one(GLORYS_v, GLORYS = T)
# saveRDS(GLORYS_v_clim, "data/GLORYS_v_clim.Rda")
# print(paste0("Began mld clims at ", Sys.time()))
# GLORYS_mld <- readRDS("data/GLORYS_mld.Rda")
# GLORYS_mld_clim <- ts2clm_one(GLORYS_mld, GLORYS = T)
# saveRDS(GLORYS_mld_clim, "data/GLORYS_mld_clim.Rda")
## Calculate anomalies
# print(paste0("Began GLORYS anoms at ", Sys.time()))
# GLORYS_u_anom <- anom_one(GLORYS_u, GLORYS_u_clim, 6)
# saveRDS(GLORYS_u_anom, "data/GLORYS_u_anom.Rda")
# GLORYS_v_anom <- anom_one(GLORYS_v, GLORYS_v_clim, 6)
# saveRDS(GLORYS_v_anom, "data/GLORYS_v_anom.Rda")
# GLORYS_mld_anom <- anom_one(GLORYS_mld, GLORYS_mld_clim, 6)
# saveRDS(GLORYS_mld_anom, "data/GLORYS_mld_anom.Rda")


# Combine all clims into one data.frame -----------------------------------

# Load all climatology files
  # NB: There are three slightly different coordinate schemes at play
# print(paste0("Began loading all clims at ", Sys.time()))
# OISST_sst_clim <- readRDS("data/OISST_sst_clim.Rda") %>%
#   mutate(lon = lon-0.125, lat = lat+0.125) %>%
#   dplyr::rename(sst_clim = seas)
# GLORYS_mld_clim <- readRDS("data/GLORYS_mld_clim.Rda") %>%
#   dplyr::rename(mld_clim = seas)
# GLORYS_u_clim <- readRDS("data/GLORYS_u_clim.Rda") %>%
#   dplyr::rename(u_clim = seas)
# GLORYS_v_clim <- readRDS("data/GLORYS_v_clim.Rda") %>%
#   dplyr::rename(v_clim = seas)
# ERA5_qnet_clim <- readRDS("data/ERA5_qnet_clim.Rda") %>%
#   mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::rename(qnet_clim = seas)
# ERA5_t2m_clim <- readRDS("data/ERA5_t2m_clim.Rda") %>%
#   mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::rename(t2m_clim = seas)
# ERA5_u_clim <- readRDS("data/ERA5_u_clim.Rda") %>%
#   mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::rename(u10_clim = seas)
# ERA5_v_clim <- readRDS("data/ERA5_v_clim.Rda") %>%
#   mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::rename(v10_clim = seas)

# Combine into one object
# print(paste0("Began combining all clims at ", Sys.time()))
# system.time(
# ALL_clim <- purrr::reduce(list(ERA5_qnet_clim, ERA5_t2m_clim,
#                                ERA5_v_clim, ERA5_u_clim,
#                                GLORYS_mld_clim, GLORYS_v_clim,
#                                GLORYS_u_clim, OISST_sst_clim), left_join, by = c("lon", "lat", "doy"))
# ) # 64 seconds

# Save
# saveRDS(ALL_clim, "data/ALL_clim.Rda")


# Combine anomalies into one data.frame -----------------------------------
# Loading all of the anomaly data.frames at once doesn't use up too much RAM,
# but the combining of them hangs really badly
# For that reason we are going to load and combine them one at a time,
# purging the memory as we go

# print(paste0("Began combining all anoms at ", Sys.time()))

# ERA 5
# NB: We start with ERA 5 as it has the most pixels due to it being atmospheric
# system.time(ERA5_u_anom <- load_anom("data/ERA5_u_anom.Rda")) # 67 seconds
# system.time(ERA5_v_anom <- load_anom("data/ERA5_v_anom.Rda")) # 69 seconds
# system.time(ALL_anom <- merge(ERA5_u_anom, ERA5_v_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(ERA5_u_anom, ERA5_v_anom); gc()
# system.time(ERA5_t2m_anom <- load_anom("data/ERA5_t2m_anom.Rda")) # 61 seconds
# system.time(ALL_anom <- merge(ALL_anom, ERA5_t2m_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(ERA5_t2m_anom); gc()
# system.time(ERA5_qnet_anom <- load_anom("data/ERA5_qnet_anom.Rda")) # 68 seconds
# system.time( ALL_anom <- merge(ALL_anom, ERA5_qnet_anom,
#                                by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(ERA5_qnet_anom); gc()

# GLORYS
# system.time(GLORYS_u_anom <- load_anom("data/GLORYS_u_anom.Rda")) # 59 seconds
# system.time(ALL_anom <- merge(ALL_anom, GLORYS_u_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(GLORYS_u_anom); gc()
# system.time(GLORYS_v_anom <- load_anom("data/GLORYS_v_anom.Rda")) # 58 seconds
# system.time(ALL_anom <- merge(ALL_anom, GLORYS_v_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(GLORYS_v_anom); gc()
# system.time(GLORYS_mld_anom <- readRDS("data/GLORYS_mld_anom.Rda")) # 16 seconds
# system.time(ALL_anom <- merge(ALL_anom, GLORYS_mld_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
# rm(GLORYS_mld_anom); gc()

# OISST
# system.time(OISST_sst_anom <- load_anom("data/OISST_sst_anom.Rda", OISST = T)) # 35 seconds
# system.time(ALL_anom <- merge(ALL_anom, OISST_sst_anom,
#                               by = c("lon", "lat", "t"), all.x = T)) # 36 seconds
# rm(OISST_sst_anom); gc()

# Save
# NB: This causes RStudio server to hang, but it still works
# print(paste0("Began saving all anoms at ", Sys.time()))
# system.time(
# saveRDS(ALL_anom, "data/anom/ALL_anom.Rda")
# ) # 514 seconds

# Load
# system.time(
# ALL_anom <- readRDS("data/anom/ALL_anom.Rda")
# ) # 78 seconds

## Test visuals
# ALL_anom %>%
#   filter(t == "2000-01-01") %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_raster(aes(fill = sst_anom))


# Combine all other data into one data.frame ------------------------------
# These data are not used in the SOM calculation,
# But are used to create summary figures

# Load and merge GLORYS U and V data
# system.time(GLORYS_u_sub <- load_anom("data/base/GLORYS_u.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 42 seconds
# system.time(GLORYS_v_sub <- load_anom("data/base/GLORYS_v.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 42 seconds
# system.time(GLORYS_uv_sub <- left_join(GLORYS_u_sub, GLORYS_v_sub,
#                                        by = c("lon", "lat", "t"))) # 5 seconds

# Load ERA 5 U and V data
# system.time(ERA5_u_sub <- load_anom("data/base/ERA5_u.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 60 seconds
# system.time(ERA5_v_sub <- load_anom("data/base/ERA5_v.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 60 seconds
# system.time(ERA5_uv_sub <- left_join(ERA5_u_sub, ERA5_v_sub,
#                                      by = c("lon", "lat", "t"))) # 7 seconds

# Combine all
# system.time(ALL_uv_sub <- left_join(ERA5_uv_sub, GLORYS_uv_sub,
#                                     by = c("lon", "lat", "t"))) # 5 seconds

# Load air temp data
# system.time(ERA5_t2m <- load_anom("data/base/ERA5_t2m.Rda")) # 55 seconds

# Load MSLP anomaly data
# system.time(ERA5_mslp_anom <- load_anom("data/anom/ERA5_mslp_anom.Rda")) # 52 seconds

# Load SST data
# system.time(OISST_sst <- load_anom("data/base/OISST_sst.Rda", OISST = T)) # 33 seconds

# Merge everything
# system.time(ALL_other <- left_join(ERA5_t2m, ERA5_mslp_anom,
#                                    by = c("lon", "lat", "t")) %>%
#               left_join(OISST_sst, by = c("lon", "lat", "t")) %>%
#               left_join(ALL_uv_sub, by = c("lon", "lat", "t"))) # 260 seconds

# Save
# NB: This causes RStudio server to hang, but it still works
# print(paste0("Began saving all other data at ", Sys.time()))
# system.time(saveRDS(ALL_other, "data/anom/ALL_other.Rda")) # 318 seconds

# Load
# system.time(ALL_other <- readRDS("data/anom/ALL_other.Rda")) # 29 seconds

## Test visuals
# ALL_other %>%
#   filter(t == "2000-01-01") %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_raster(aes(fill = sst)) +
#   geom_segment(aes(xend = lon + u10 * wind_uv_scalar,
#                    yend = lat + v10 * wind_uv_scalar),
#                arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
#                linejoin = "mitre", size = 0.4, alpha = 0.4)


# Data packets ------------------------------------------------------------

# Set number of cores
  # NB: 50 cores can be too much for the RAM
# doMC::registerDoMC(cores = 25)

## Create one big anomaly packet
# print(paste0("Began creating data packets at ", Sys.time()))
# system.time(synoptic_states <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
                                           # data_packet_func, .parallel = T)) # 50 seconds
# Save
# saveRDS(synoptic_states, "data/SOM/synoptic_states.Rda")

## Create other synoptic states per MHW per variable
# doMC::registerDoMC(cores = 10) # NB: Be careful here...
# system.time(synoptic_states_other <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
                                                 # data_packet_func, .parallel = T, df = ALL_other)) # 71 seconds
# Save
# saveRDS(synoptic_states_other, "data/SOM/synoptic_states_other.Rda")


# SOM analysis ------------------------------------------------------------

# packet <- readRDS("data/SOM/packet.Rda")
# system.time(som <- som_model_PCI(packet)) # 78 seconds
# saveRDS(som, file = "data/SOM/som.Rda")


# Visuals -----------------------------------------------------------------

# som <- readRDS("data/SOM/som.Rda")
# som_node_visualise(som)

