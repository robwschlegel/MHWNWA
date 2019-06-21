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
#                                    "analysis/vec-prep.Rmd", "analysis/som.Rmd",
#                                    "analysis/figures.Rmd"),
#                          message = "Re-publish entire site.")
# ) # 193 seconds

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


# Vector data packets -----------------------------------------------------

# Set number of cores
# NB: Was set to 25 as someone else was using the server at the time
doMC::registerDoMC(cores = 50)

# Create one big packet
system.time(
synoptic_vec_states <- plyr::ddply(NAPA_MHW_event, c("region", "sub_region", "event_no"), data_vec_packet, .parallel = T)
) # xxx seconds

# Save
saveRDS(synoptic_vec_states, "data/synoptic_vec_states.Rda")


# SOM analysis ------------------------------------------------------------

# all_anom <- readRDS("data/packet_all_anom.Rda")
# system.time(som_all_anom <- som_model_PCI(all_anom)) # 122 seconds
# saveRDS(som_all_anom, file = "data/som_all_anom.Rda")


# Visuals -----------------------------------------------------------------


