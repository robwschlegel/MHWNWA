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
# ERA5_lwr <- load_all_ERA5(ERA5_lwr_files)
# saveRDS(ERA5_lwr, "data/ERA5_lwr.Rda")
# ERA5_lwr_clim <- ts2clm_one(ERA5_lwr)
# saveRDS(ERA5_lwr_clim, "data/ERA5_lwr_clim.Rda")
# ERA5_lwr_anom <- anom_one(ERA5_lwr, ERA5_lwr_clim, 10)
# saveRDS(ERA5_lwr_anom, "data/ERA5_lwr_anom.Rda")

## Short wave radiation
# "msnswrf"
# ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
# ERA5_swr <- load_all_ERA5(ERA5_swr_files)
# saveRDS(ERA5_swr, "data/ERA5_swr.Rda")
# ERA5_swr_clim <- ts2clm_one(ERA5_swr)
# saveRDS(ERA5_swr_clim, "data/ERA5_swr_clim.Rda")
# ERA5_swr_anom <- anom_one(ERA5_swr, ERA5_swr_clim, 10)
# saveRDS(ERA5_swr_anom, "data/ERA5_swr_anom.Rda")

## Latent heat flux
# "mslhf"
# ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
# ERA5_lhf <- load_all_ERA5(ERA5_lhf_files)
# saveRDS(ERA5_lhf, "data/ERA5_lhf.Rda")
# ERA5_lhf_clim <- ts2clm_one(ERA5_lhf)
# saveRDS(ERA5_lhf_clim, "data/ERA5_lhf_clim.Rda")
# ERA5_lhf_anom <- anom_one(ERA5_lhf, ERA5_lhf_clim, 10)
# saveRDS(ERA5_lhf_anom, "data/ERA5_lhf_anom.Rda")

## Sensible heat flux
# "msshf"
# ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
# ERA5_shf <- load_all_ERA5(ERA5_shf_files)
# saveRDS(ERA5_shf, "data/ERA5_shf.Rda")
# ERA5_shf_clim <- ts2clm_one(ERA5_shf)
# saveRDS(ERA5_shf_clim, "data/ERA5_shf_clim.Rda")
# ERA5_shf_anom <- anom_one(ERA5_shf, ERA5_shf_clim, 10)
# saveRDS(ERA5_shf_anom, "data/ERA5_shf_anom.Rda")

## Net heat flux
# ERA5_qnet <- left_join(ERA5_lwr, ERA5_swr, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_lhf, by = c("lon", "lat", "t")) %>%
#   left_join(ERA5_shf, by = c("lon", "lat", "t")) %>%
#   mutate(qnet = msnlwrf+msnswrf+mslhf+msshf) %>%
#   select(lon, lat, t, qnet)
# saveRDS(ERA5_qnet, "data/ERA5_qnet.Rda")
# ERA5_qnet_clim <- ts2clm_one(ERA5_qnet)
# saveRDS(ERA5_qnet_clim, "data/ERA5_qnet_clim.Rda")
# ERA5_qnet_anom <- anom_one(ERA5_qnet, ERA5_qnet_clim, 10)
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


# Variable data packets ---------------------------------------------------

# Set number of cores
  # NB: Was set to 25 as someone else was using the server at the time
# doMC::registerDoMC(cores = 25)

# Create one big packet
# system.time(
# synoptic_states <- plyr::ddply(NAPA_MHW_event, c("region", "sub_region", "event_no"), data_packet, .parallel = T)
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
