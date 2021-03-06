# code/workflow.R
# This script may be run with source() in order to run the entire analysis
# This is not strongly encouraged and should rather be done in bite-sized chunks


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
system.time(
workflowr::wflow_publish(files = c("analysis/index.Rmd",
                                   # "analysis/polygon-prep.Rmd",
                                   # "analysis/sst-prep.Rmd",
                                   # "analysis/var-prep.Rmd",
                                   # "analysis/som.Rmd",
                                   # "analysis/figures.Rmd",
                                   "analysis/node-summary.Rmd"),
                         message = "Re-publish entire site.")
) # 70 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Study area and regions" vignette


# MHW detection -----------------------------------------------------------

# See the "MHW detection" vignette (analysis/sst-prep.Rmd)


# OISST processing --------------------------------------------------------

# Set number of cores
# NB: Can't hold all files in memmory at once
doParallel::registerDoParallel(cores = 10)

### OISST data processing
## SST
OISST_files <- OISST_files <- dir("../../data/OISST", full.names = T)
OISST_sst <- load_all_OISST(OISST_files)
saveRDS(OISST_sst, "data/OISST_sst.Rda")

## OISST clims
OISST_sst_clim <- ts2clm_one(OISST_sst)
saveRDS(OISST_sst_clim, "data/OISST_sst_clim.Rda")

## OISST anoms
OISST_sst_anom <- anom_one(OISST_sst, OISST_sst_clim, 2)
saveRDS(OISST_sst_anom, "data/OISST_sst_anom.Rda")


# GLORYS processing -------------------------------------------------------

## Load high-res GLORYS data
# NB: This automatically constrains the data to the same 1/4 degree grid as the low-res data
GLORYS_high_res_files <- dir("../data/GLORYS", full.names = T, pattern = "twelfth")
system.time(
GLORYS_all <- load_all_GLORYS_hires(GLORYS_high_res_files)
) # 83 seconds

## test visuals
ggplot(filter(GLORYS_all, t == "1998-01-01"), aes(x = lon, y = lat, fill = mld)) +
  geom_raster() +
  scale_fill_gradient2()

## Surface currents U
GLORYS_u <- select(GLORYS_all, lon, lat, t, u)
saveRDS(GLORYS_u, "data/GLORYS_u.Rda")
# U clims
GLORYS_u_clim <- ts2clm_one(GLORYS_u, GLORYS = T)
saveRDS(GLORYS_u_clim, "data/GLORYS_u_clim.Rda")
# U anoms
GLORYS_u_anom <- anom_one(GLORYS_u, GLORYS_u_clim, 6)
saveRDS(GLORYS_u_anom, "data/GLORYS_u_anom.Rda")

## Surface currents V
GLORYS_v <- select(GLORYS_all, lon, lat, t, v)
saveRDS(GLORYS_v, "data/GLORYS_v.Rda")
# V clims
GLORYS_v_clim <- ts2clm_one(GLORYS_v, GLORYS = T)
saveRDS(GLORYS_v_clim, "data/GLORYS_v_clim.Rda")
# V anoms
GLORYS_v_anom <- anom_one(GLORYS_v, GLORYS_v_clim, 6)
saveRDS(GLORYS_v_anom, "data/GLORYS_v_anom.Rda")

## Mixed layer depth
GLORYS_mld <- select(GLORYS_all, lon, lat, t, mld)
saveRDS(GLORYS_mld, "data/GLORYS_mld.Rda")
# MLD clims
GLORYS_mld_clim <- ts2clm_one(GLORYS_mld, GLORYS = T)
saveRDS(GLORYS_mld_clim, "data/GLORYS_mld_clim.Rda")
# MLD anoms
GLORYS_mld_anom <- anom_one(GLORYS_mld, GLORYS_mld_clim, 6)
saveRDS(GLORYS_mld_anom, "data/GLORYS_mld_anom.Rda")


# ERA 5 processing --------------------------------------------------------

# nc_info <- ncdump::NetCDF(ERA5_lhf_files[1])$variable$name

## Long wave radiation
# "msnlwrf"
# print(paste0("Began loading msnlwrf at ", Sys.time()))
ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
ERA5_lwr <- load_all_ERA5(ERA5_lwr_files) %>%
  mutate(msnlwrf = round(msnlwrf, 6))
saveRDS(ERA5_lwr, "data/ERA5_lwr.Rda")

## Short wave radiation
# "msnswrf"
# print(paste0("Began loading msnswrf at ", Sys.time()))
ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
ERA5_swr <- load_all_ERA5(ERA5_swr_files) %>%
  mutate(msnswrf = round(msnswrf, 6))
saveRDS(ERA5_swr, "data/ERA5_swr.Rda")

## Latent heat flux
# "mslhf"
# print(paste0("Began loading mslhf at ", Sys.time()))
ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
ERA5_lhf <- load_all_ERA5(ERA5_lhf_files) %>%
  mutate(mslhf = round(mslhf, 6))
saveRDS(ERA5_lhf, "data/ERA5_lhf.Rda")

## Sensible heat flux
# "msshf"
# print(paste0("Began loading msshf at ", Sys.time()))
ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
ERA5_shf <- load_all_ERA5(ERA5_shf_files) %>%
  mutate(msshf = round(msshf, 6))
saveRDS(ERA5_shf, "data/ERA5_shf.Rda")

## Net heat flux (not including the advection term, which is accounted for on its own)
# print(paste0("Began creating qnet at ", Sys.time()))
ERA5_qnet <- left_join(ERA5_lwr, ERA5_swr, by = c("lon", "lat", "t")) %>%
  left_join(ERA5_lhf, by = c("lon", "lat", "t")) %>%
  left_join(ERA5_shf, by = c("lon", "lat", "t")) %>%
  mutate(qnet = msnlwrf+msnswrf+mslhf+msshf) %>%
  select(lon, lat, t, qnet) %>%
  mutate(qnet = round(qnet, 6))
saveRDS(ERA5_qnet, "data/ERA5_qnet.Rda")
# Qnet Clims
# print(paste0("Began qnet clims at ", Sys.time()))
ERA5_qnet_clim <- ts2clm_one(ERA5_qnet)
saveRDS(ERA5_qnet_clim, "data/ERA5_qnet_clim.Rda")
# Qnet anoms
# print(paste0("Began qnet anomalies at ", Sys.time()))
ERA5_qnet_anom <- anom_one(ERA5_qnet, ERA5_qnet_clim, 6)
saveRDS(ERA5_qnet_anom, "data/ERA5_qnet_anom.Rda")

## Air temperature at 2 metres
# print(paste0("Began loading t2m at ", Sys.time()))
ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")[15:40]
ERA5_t2m <- load_all_ERA5(ERA5_t2m_files) %>%
  mutate(t2m = round(t2m, 2)-272.15)
saveRDS(ERA5_t2m, "data/ERA5_t2m.Rda")
# Air clims
# print(paste0("Began creating t2m clims at ", Sys.time()))
ERA5_t2m_clim <- ts2clm_one(ERA5_t2m)
saveRDS(ERA5_t2m_clim, "data/ERA5_t2m_clim.Rda")
# Air anoms
# print(paste0("Began t2m anomalies at ", Sys.time()))
ERA5_t2m_anom <- anom_one(ERA5_t2m, ERA5_t2m_clim, 2)
saveRDS(ERA5_t2m_anom, "data/ERA5_t2m_anom.Rda")

## Surface winds U component
# "u10"
# print(paste0("Began loading u10 at ", Sys.time()))
ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")[15:40]
ERA5_u <- load_all_ERA5(ERA5_u_files) %>%
  mutate(u10 = round(u10, 6))
saveRDS(ERA5_u, "data/ERA5_u.Rda")
# U10clims
# print(paste0("Began u10 clims at ", Sys.time()))
ERA5_u_clim <- ts2clm_one(ERA5_u)
saveRDS(ERA5_u_clim, "data/ERA5_u_clim.Rda")
# U10 anoms
# print(paste0("Began u10 anomalies at ", Sys.time()))
ERA5_u_anom <- anom_one(ERA5_u, ERA5_u_clim, 6)
saveRDS(ERA5_u_anom, "data/ERA5_u_anom.Rda")

## Surface winds V component
# "v10"
# print(paste0("Began loading v10 at ", Sys.time()))
ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")[15:40]
ERA5_v <- load_all_ERA5(ERA5_v_files) %>%
  mutate(v10 = round(v10, 6))
saveRDS(ERA5_v, "data/ERA5_v.Rda")
# V10 clims
# print(paste0("Began v10 clims at ", Sys.time()))
ERA5_v_clim <- ts2clm_one(ERA5_v)
saveRDS(ERA5_v_clim, "data/ERA5_v_clim.Rda")
# V10 anoms
# print(paste0("Began v10 anomalies at ", Sys.time()))
ERA5_v_anom <- anom_one(ERA5_v, ERA5_v_clim, 6)
saveRDS(ERA5_v_anom, "data/ERA5_v_anom.Rda")

## Mean sea level pressure
# "mslp"
# print(paste0("Began loading MSLP at ", Sys.time()))
ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")[15:40]
ERA5_mslp <- load_all_ERA5(ERA5_mslp_files)
saveRDS(ERA5_mslp, "data/ERA5_mslp.Rda")
# MSLP clims
# print(paste0("Began MSLP clims at ", Sys.time()))
ERA5_mslp_clim <- ts2clm_one(ERA5_mslp)
saveRDS(ERA5_mslp_clim, "data/ERA5_mslp_clim.Rda")
# MSLP anoms
# print(paste0("Began MSLP anomalies at ", Sys.time()))
ERA5_mslp_anom <- anom_one(ERA5_mslp, ERA5_mslp_clim, 2)
saveRDS(ERA5_mslp_anom, "data/ERA5_mslp_anom.Rda")


# Climatology dataframe ---------------------------------------------------

## Load all climatology files
  # NB: There are three slightly different coordinate schemes at play
# print(paste0("Began loading all clims at ", Sys.time()))
OISST_sst_clim <- readRDS("data/OISST_sst_clim.Rda") %>%
  mutate(lon = lon-0.125, lat = lat+0.125) %>%
  dplyr::rename(sst_clim = seas)
GLORYS_mld_clim <- readRDS("data/GLORYS_mld_clim.Rda") %>%
  dplyr::rename(mld_clim = seas)
GLORYS_u_clim <- readRDS("data/GLORYS_u_clim.Rda") %>%
  dplyr::rename(u_clim = seas)
GLORYS_v_clim <- readRDS("data/GLORYS_v_clim.Rda") %>%
  dplyr::rename(v_clim = seas)
ERA5_qnet_clim <- readRDS("data/ERA5_qnet_clim.Rda") %>%
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
  dplyr::rename(qnet_clim = seas)
ERA5_t2m_clim <- readRDS("data/ERA5_t2m_clim.Rda") %>%
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
  dplyr::rename(t2m_clim = seas)
ERA5_u_clim <- readRDS("data/ERA5_u_clim.Rda") %>%
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
  dplyr::rename(u10_clim = seas)
ERA5_v_clim <- readRDS("data/ERA5_v_clim.Rda") %>%
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
  dplyr::rename(v10_clim = seas)

## Combine into one object
# print(paste0("Began combining all clims at ", Sys.time()))
system.time(
ALL_clim <- purrr::reduce(list(ERA5_qnet_clim, ERA5_t2m_clim,
                               ERA5_v_clim, ERA5_u_clim,
                               GLORYS_mld_clim, GLORYS_v_clim,
                               GLORYS_u_clim, OISST_sst_clim),
                          left_join, by = c("lon", "lat", "doy"))
) # 64 seconds
# Save
saveRDS(ALL_clim, "data/ALL_clim.Rda")


# Anomaly dataframe -------------------------------------------------------

# Loading all of the anomaly dataframes at once doesn't use up too much RAM,
# but the combining of them hangs really badly
# For that reason we are going to load and combine them one at a time,
# purging the memory as we go

# print(paste0("Began combining all anoms at ", Sys.time()))

## ERA 5
  # NB: We start with ERA 5 as it has the most pixels due to it being atmospheric
system.time(ERA5_u_anom <- load_anom("data/ERA5_u_anom.Rda")) # 67 seconds
system.time(ERA5_v_anom <- load_anom("data/ERA5_v_anom.Rda")) # 69 seconds
system.time(ALL_anom <- merge(ERA5_u_anom, ERA5_v_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(ERA5_u_anom, ERA5_v_anom); gc()
system.time(ERA5_t2m_anom <- load_anom("data/ERA5_t2m_anom.Rda")) # 61 seconds
system.time(ALL_anom <- merge(ALL_anom, ERA5_t2m_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(ERA5_t2m_anom); gc()
system.time(ERA5_qnet_anom <- load_anom("data/ERA5_qnet_anom.Rda")) # 68 seconds
system.time( ALL_anom <- merge(ALL_anom, ERA5_qnet_anom,
                               by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(ERA5_qnet_anom); gc()

## GLORYS
system.time(GLORYS_u_anom <- load_anom("data/GLORYS_u_anom.Rda")) # 59 seconds
system.time(ALL_anom <- merge(ALL_anom, GLORYS_u_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(GLORYS_u_anom); gc()
system.time(GLORYS_v_anom <- load_anom("data/GLORYS_v_anom.Rda")) # 58 seconds
system.time(ALL_anom <- merge(ALL_anom, GLORYS_v_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(GLORYS_v_anom); gc()
system.time(GLORYS_mld_anom <- readRDS("data/GLORYS_mld_anom.Rda")) # 16 seconds
system.time(ALL_anom <- merge(ALL_anom, GLORYS_mld_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 33 seconds
rm(GLORYS_mld_anom); gc()

## OISST
system.time(OISST_sst_anom <- load_anom("data/OISST_sst_anom.Rda", OISST = T)) # 35 seconds
system.time(ALL_anom <- merge(ALL_anom, OISST_sst_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # 36 seconds
rm(OISST_sst_anom); gc()

## Save
  # NB: This causes RStudio server to hang, but it still works
print(paste0("Began saving all anoms at ", Sys.time()))
system.time(saveRDS(ALL_anom, "data/anom/ALL_anom.Rda")) # 514 seconds

## Test visuals
# Load
system.time(ALL_anom <- readRDS("data/anom/ALL_anom.Rda")) # 78 seconds
# Plot
ALL_anom %>%
  filter(t == "2000-01-01") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst_anom))


# Other dataframe ---------------------------------------------------------

## These data are not used in the SOM calculation,
## But are used to create summary figures

## Load and merge GLORYS U and V data
system.time(GLORYS_u_sub <- load_anom("data/base/GLORYS_u.Rda") %>%
              filter(lon %in% lon_sub,
                     lat %in% lat_sub)) # 42 seconds
system.time(GLORYS_v_sub <- load_anom("data/base/GLORYS_v.Rda") %>%
              filter(lon %in% lon_sub,
                     lat %in% lat_sub)) # 42 seconds
system.time(GLORYS_uv_sub <- left_join(GLORYS_u_sub, GLORYS_v_sub,
                                       by = c("lon", "lat", "t"))) # 5 seconds

## Load ERA 5 U and V data
system.time(ERA5_u_sub <- load_anom("data/base/ERA5_u.Rda") %>%
              filter(lon %in% lon_sub,
                     lat %in% lat_sub)) # 60 seconds
system.time(ERA5_v_sub <- load_anom("data/base/ERA5_v.Rda") %>%
              filter(lon %in% lon_sub,
                     lat %in% lat_sub)) # 60 seconds
system.time(ERA5_uv_sub <- left_join(ERA5_u_sub, ERA5_v_sub,
                                     by = c("lon", "lat", "t"))) # 7 seconds

## Combine all
system.time(ALL_uv_sub <- left_join(ERA5_uv_sub, GLORYS_uv_sub,
                                    by = c("lon", "lat", "t"))) # 5 seconds

## Load air temp data
system.time(ERA5_t2m <- load_anom("data/base/ERA5_t2m.Rda")) # 55 seconds

# Load MSLP real and anomaly data and combine
system.time(ERA5_mslp <- load_anom("data/base/ERA5_mslp.Rda")) # 52 seconds
system.time(ERA5_mslp_anom <- load_anom("data/anom/ERA5_mslp_anom.Rda")) # 52 seconds
system.time(ERA5_mslp_all <- left_join(ERA5_mslp, ERA5_mslp_anom,
                                       by = c("lon", "lat", "t"))) # 5 seconds

## Load SST data
system.time(OISST_sst <- load_anom("data/base/OISST_sst.Rda", OISST = T)) # 33 seconds

## Merge everything
system.time(ALL_other <- left_join(ERA5_t2m, ERA5_mslp_all,
                                   by = c("lon", "lat", "t")) %>%
              left_join(OISST_sst, by = c("lon", "lat", "t")) %>%
              left_join(ALL_uv_sub, by = c("lon", "lat", "t"))) # 260 seconds
# Save
  # NB: This causes RStudio server to hang, but it still works
# print(paste0("Began saving all other data at ", Sys.time()))
system.time(saveRDS(ALL_other, "data/anom/ALL_other.Rda")) # 318 seconds

## Test visuals
# Load
system.time(ALL_other <- readRDS("data/anom/ALL_other.Rda")) # 29 seconds
# Plot
ALL_other %>%
  filter(t == "2000-01-01") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = sst)) +
  geom_segment(aes(xend = lon + u10 * wind_uv_scalar,
                   yend = lat + v10 * wind_uv_scalar),
               arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
               linejoin = "mitre", size = 0.4, alpha = 0.4) +
  geom_contour(aes(z = msl, colour = stat(level)), size = 1)


# Data packets ------------------------------------------------------------

# Set number of cores
  # NB: 50 cores requires too much RAM
doParallel::registerDoParallel(cores = 25)

## Create one big anomaly packet from OISST data
# print(paste0("Began creating data packets at ", Sys.time()))
system.time(synoptic_states <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
                                           data_packet_func, .parallel = T)) # 204 seconds
# Save
saveRDS(synoptic_states, "data/SOM/synoptic_states.Rda")

## Create other synoptic states per MHW per variable
doParallel::registerDoParallel(cores = 10) # NB: Be careful here...
system.time(synoptic_states_other <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
                                                 data_packet_func, .parallel = T, df = ALL_other)) # 122 seconds
# Save
saveRDS(synoptic_states_other, "data/SOM/synoptic_states_other.Rda")

## Create wide data packet that is fed to SOM
system.time(packet <- readRDS("data/SOM/synoptic_states.Rda") %>%
              select(region, event_no, synoptic) %>%
              unnest() %>%
              wide_packet_func()) # 122 seconds
# Save
saveRDS(packet, "data/SOM/packet.Rda")

## Create one big anomaly packet from GLORYS data
# Load the GLORYS MHW data
GLORYS_region_MHW <- readRDS("../MHWflux/data/GLORYS_region_MHW.Rda")
GLORYS_MHW_event <- GLORYS_region_MHW %>%
  select(-cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 0) %>%
  unnest(events)

# print(paste0("Began creating data packets at ", Sys.time()))
doParallel::registerDoParallel(cores = 10)
system.time(synoptic_states_GLORYS <- plyr::ddply(GLORYS_MHW_event, c("region", "event_no"),
                                                  data_packet_func, .parallel = T)) # 204 seconds
# Save
saveRDS(synoptic_states_GLORYS, "data/SOM/synoptic_states_GLORYS.Rda")

## Create other synoptic states per MHW per variable
doParallel::registerDoParallel(cores = 10) # NB: Be careful here...
system.time(synoptic_states_other_GLORYS <- plyr::ddply(GLORYS_MHW_event, c("region", "event_no"),
                                                        data_packet_func, .parallel = T, df = ALL_other)) # 122 seconds
# Save
saveRDS(synoptic_states_other_GLORYS, "data/SOM/synoptic_states_other_GLORYS.Rda")

## Create wide data packet that is fed to SOM
system.time(packet_GLORYS <- readRDS("data/SOM/synoptic_states_GLORYS.Rda") %>%
              select(region, event_no, synoptic) %>%
              unnest() %>%
              wide_packet_func()) # 122 seconds
# Save
saveRDS(packet_GLORYS, "data/SOM/packet_GLORYS.Rda")


# Visualise data packets --------------------------------------------------

# Look at event sizes etc.
# OISST_MHW_event

# NB: Just pick a region and event number to save a synoptic summary of it

# The 2012 lobster MHW
fig_data_packet("gm",	14)
fig_data_packet("gsl",	39)

# Smol MHWs
fig_data_packet("cbs",	14)
fig_data_packet("mab", 20)

# Hecking big MHWs
fig_data_packet("mab",	23)


# SOM analysis ------------------------------------------------------------

# OISST SOM analysis
packet <- readRDS("data/SOM/packet.Rda")
synoptic_states_other <- readRDS("data/SOM/synoptic_states_other.Rda")
system.time(som <- som_model_PCI(packet, synoptic_states_other)) # 83 seconds
saveRDS(som, file = "data/SOM/som.Rda")

# GLORYS SOM analysis
packet_GLORYS <- readRDS("data/SOM/packet_GLORYS.Rda")
synoptic_states_other_GLORYS <- readRDS("data/SOM/synoptic_states_other_GLORYS.Rda")
system.time(som_GLORYS <- som_model_PCI(packet_GLORYS, synoptic_states_other_GLORYS)) # 158 seconds
saveRDS(som_GLORYS, file = "data/SOM/som_GLORYS.Rda")


# Visuals -----------------------------------------------------------------

# OISST visuals
som <- readRDS("data/SOM/som.Rda")
fig_all_som(som)

# GLORYS visuals
som_GLORYS <- readRDS("data/SOM/som_GLORYS.Rda")
GLORYS_region_MHW <- readRDS("../MHWflux/data/GLORYS_region_MHW.Rda")
fig_all_som(som_GLORYS, GLORYS_region_MHW, "GLORYS")

