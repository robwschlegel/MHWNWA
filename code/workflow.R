# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------



# MHW calculations --------------------------------------------------------



# Variable climatologies --------------------------------------------------

# NB: The creation of a clim for one variable is too large to run via ldply
# Rather they must be run one at a time via a for loop and the memmory dumped after each
# for(i in 1:nrow(NAPA_vars)){
#   clim_one_var(NAPA_vars$name[i])
#   gc()
# }


# Vector climatologies ----------------------------------------------------

# clim_one_vec(NAPA_U_files)
# gc()

# clim_one_vec(NAPA_V_files)
# gc()

# clim_one_vec(NAPA_W_files)
# gc()


# Data packets ------------------------------------------------------------


# Vector data packets -----------------------------------------------------

# Set number of cores
# NB: Was set to 25 as someone else was using the server at the time
# doMC::registerDoMC(cores = 25)

# Create one big packet
# system.time(
#   synoptic_vec_states <- plyr::ddply(NAPA_MHW_event, c("region", "sub_region", "event_no"), data_vec_packet, .parallel = T)
# ) # 28 seconds for first 2, 6125 seconds (102 minutes) for all

# Save
# saveRDS(synoptic_vec_states, "data/synoptic_vec_states.Rda")


# SOM analysis ------------------------------------------------------------

# all_anom <- readRDS("data/packet_all_anom.Rda")
# system.time(som_all_anom <- som_model_PCI(all_anom)) # 122 seconds
# saveRDS(som_all_anom, file = "data/som_all_anom.Rda")


# Visuals -----------------------------------------------------------------


