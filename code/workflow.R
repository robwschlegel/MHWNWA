# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
# workflowr::wflow_publish(files = c("analysis/index.Rmd", "analysis/polygon-prep.Rmd",
#                                    "analysis/sst-prep.Rmd", "analysis/var-prep.Rmd"),#,
#                                    #"analysis/som.Rmd", "analysis/figures.Rmd", "analysis/node-summary.Rmd"),
#                          message = "Re-publish entire site.")
# ) # 223 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Polygon preparation" vignette


# MHW calculations --------------------------------------------------------

# See the "SST preparation" vignette


# Data prep ---------------------------------------------------------------

# Create net heat flux (qnet) variable
  # This requires combining four full variables from the ERA 5 data

# nc_info <- ncdump::NetCDF(ERA5_shf_files[1])$variable$name

## Long wave radiation
# "msnlwrf"
# ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
# ERA5_lwr <- load_all_ERA5(ERA5_lwr_files) %>%
#   mutate(msnlwrf = round(msnlwrf, 6))
# saveRDS(ERA5_lwr, "data/ERA5_lwr.Rda")

## Short wave radiation
# "msnswrf"
# ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
# ERA5_swr <- load_all_ERA5(ERA5_swr_files) %>%
#   mutate(msnswrf = round(msnswrf, 6))
# saveRDS(ERA5_swr, "data/ERA5_swr.Rda")

## Latent heat flux
# "mslhf"
# ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
# ERA5_lhf <- load_all_ERA5(ERA5_lhf_files) %>%
#   mutate(mslhf = round(mslhf, 6))
# saveRDS(ERA5_lhf, "data/ERA5_lhf.Rda")

## Sensible heat flux
# "msshf"
# ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
# ERA5_shf <- load_all_ERA5(ERA5_shf_files) %>%
#   mutate(msshf = round(msshf, 6))
# saveRDS(ERA5_shf, "data/ERA5_shf.Rda")

## Net heat flux (not including the advection term, which is accounted for on its own)
# ERA5_qnet <- left_join(ERA5_lwr, ERA5_swr, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_lhf, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_shf, by = c("lon", "lat", "t")) %>%
#   mutate(qnet = msnlwrf+msnswrf+mslhf+msshf) %>%
#   select(lon, lat, t, qnet) %>%
#   mutate(qnet = round(qnet, 6))
# saveRDS(ERA5_qnet, "data/ERA5_qnet.Rda")
# ERA5_qnet_clim <- ts2clm_one(ERA5_qnet)
# saveRDS(ERA5_qnet_clim, "data/ERA5_qnet_clim.Rda")
# ERA5_qnet_anom <- anom_one(ERA5_qnet, ERA5_qnet_clim, 6)
# saveRDS(ERA5_qnet_anom, "data/ERA5_qnet_anom.Rda")


# Variable climatologies --------------------------------------------------

# nc_info <- ncdump::NetCDF(ERA5_u_files[1])$variable$name

## Surface winds U component
# "u10"
# ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")
# ERA5_u <- load_all_ERA5(ERA5_u_files)
# saveRDS(ERA5_u, "data/ERA5_u.Rda")
# ERA5_u_clim <- ts2clm_one(ERA5_u)
# saveRDS(ERA5_u_clim, "data/ERA5_u_clim.Rda")
# ERA5_u_anom <- anom_one(ERA5_u, ERA5_u_clim, 6)
# saveRDS(ERA5_u_anom, "data/ERA5_u_anom.Rda")

## Surface winds U component
# "v10"
# ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")
# ERA5_v <- load_all_ERA5(ERA5_v_files)
# saveRDS(ERA5_v, "data/ERA5_v.Rda")
# ERA5_v_clim <- ts2clm_one(ERA5_v)
# saveRDS(ERA5_v_clim, "data/ERA5_v_clim.Rda")
# ERA5_v_anom <- anom_one(ERA5_v, ERA5_v_clim, 6)
# saveRDS(ERA5_v_anom, "data/ERA5_v_anom.Rda")

### GLORYS data processing
## Load low-res GLORYS data
GLORYS_low_res_files <- dir("../data/GLORYS", full.names = T, pattern = "quarter")
GLORYS_low_res <- load_all_GLORYS(GLORYS_low_res_files)
## Load high-res GLORYS data
  # NB: This is aitomatically constrained to the same 1/4 degree grid as the low-res data
GLORYS_high_res_files <- dir("../data/GLORYS", full.names = T, pattern = "twelfth")
GLORYS_high_res <- load_all_GLORYS_hires(GLORYS_high_res_files)
## Combine GLORYS low and high-res
  # NB: Some of the newer high-res pixels don't continue on from the low-res pixels
GLORYS_all <- rbind(GLORYS_low_res, GLORYS_high_res) %>%
  group_by(lon, lat) %>%
  # This line of code removes 67184 rows of data
  filter(max(t, na.rm = T) == "2018-12-25",
         # This line of code removes xxx rows of data
         min(t, na.rm = T) == "1993-01-01") %>%
  ungroup()
# test visuals
# ggplot(filter(GLORYS_all, t == "2013-06-01"), aes(x = lon, y = lat, fill = mld)) +
#   geom_raster() +
#   scale_fill_gradient2()
## Separate into three variables
# Surface currents U
GLORYS_u <- select(GLORYS_all, lon, lat, t, u)
saveRDS(GLORYS_u, "data/GLORYS_u.Rda")
# Surface currents V
GLORYS_v <- select(GLORYS_all, lon, lat, t, v)
saveRDS(GLORYS_v, "data/GLORYS_v.Rda")
# Mixed layer depth
GLORYS_mld <- select(GLORYS_all, lon, lat, t, mld)
saveRDS(GLORYS_mld, "data/GLORYS_mld.Rda")
## Calculate climatologies
# GLORYS_u <- readRDS("data/GLORYS_u.Rda")
GLORYS_u_clim <- ts2clm_one(GLORYS_u, GLORYS = T)
saveRDS(GLORYS_u_clim, "data/GLORYS_u_clim.Rda")
# GLORYS_v <- readRDS("data/GLORYS_v.Rda")
GLORYS_v_clim <- ts2clm_one(GLORYS_v, GLORYS = T)
saveRDS(GLORYS_v_clim, "data/GLORYS_v_clim.Rda")
# GLORYS_mld <- readRDS("data/GLORYS_mld.Rda")
GLORYS_mld_clim <- ts2clm_one(GLORYS_mld, GLORYS = T)
saveRDS(GLORYS_mld_clim, "data/GLORYS_mld_clim.Rda")
## Calculate anomalies
GLORYS_u_anom <- anom_one(GLORYS_u, GLORYS_u_clim, 6)
saveRDS(GLORYS_u_anom, "data/GLORYS_u_anom.Rda")
GLORYS_v_anom <- anom_one(GLORYS_v, GLORYS_v_clim, 6)
saveRDS(GLORYS_v_anom, "data/GLORYS_v_anom.Rda")
GLORYS_mld_anom <- anom_one(GLORYS_mld, GLORYS_mld_clim, 6)
saveRDS(GLORYS_mld_anom, "data/GLORYS_mld_anom.Rda")


# Variable data packets ---------------------------------------------------

# Set number of cores
# doMC::registerDoMC(cores = 50)

# Create one big packet
# system.time(
# synoptic_states <- plyr::ddply(OISST_MHW_event, c("region", "event_no"), data_packet, .parallel = T)
# ) # 82 seconds for first event, 2,6125 seconds (102 minutes) for all events

# Save
# saveRDS(synoptic_states, "data/synoptic_states.Rda")


# SOM analysis ------------------------------------------------------------

# all_anom <- readRDS("data/packet_all_anom.Rda")
# system.time(som_all_anom <- som_model_PCI(all_anom)) # 122 seconds
# saveRDS(som_all_anom, file = "data/som_all_anom.Rda")


# Visuals -----------------------------------------------------------------

# Create a four panel summary figure for each node result
# plyr::l_ply(1:12, .fun = node_figure, .parallel = T)
