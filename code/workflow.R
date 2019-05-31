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
# for(i in 1:length(NAPA_vars)){
#   clim_one_var(NAPA_vars$name[i])
#   gc()
# }


# Data packets ------------------------------------------------------------



# SOM analysis ------------------------------------------------------------



