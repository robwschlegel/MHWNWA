# Data

This folder contains the data used in and created by this project. All of the data files found within this folder serve general/meta-data purposes for the project. More specific data used at particular steps in the project are to be found in the other folders. Note that most of the data in these sub-folders are not uploaded to GitHub as they are too large(~200 - 6000 MB) The following is a break down of what the folders within this folder contain:

- NAPA: A collection of files from the NAPA/3-Oceans model. This was the data used for the first run of this project but was later abandoned in favour of obs/reanalysis products in the following folders.
- base: These are the base data products as downloaded (and then manipulated into a project-wide standard) from their hosting bodies. The individual variables are saved in separate files and follow the naming convention of `*product*_*variable*.Rda`.
- clim: The files in this folder are the daily climatologies/pixel of the files found in the `base/` folder. They follow the naming convention `*product*_*variable*_clim.Rda`. The climatologies are determined with the same Hobday algorithm used to determine MHWs.
- anom: These files are all of the anomaly data, which effectively are just the files from `clim/` subtracted from the files in `base/`. The naming convention is `*product*_*variable*_anom.Rda`. The exception to this being the file `ALL_other.Rda`, which is a mix of base and anomaly data that are needed for figure creation, but aren't used in the SOM calculations themselves. They are placed in this folder as there is no other better place for them to be.
- comparisons: This folder contains the SOM analysis files used in the intermediate step in this process when multiple different SOM configurations were used before deciding on the current 4x3 node version that includes all regions except the Labrador Shelf.
- SOM: This folder contains the data used to run the SOM calculations, as well as to create visuals of those results afterwards.



