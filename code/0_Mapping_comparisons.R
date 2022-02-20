######### Mapping characterisation #########
# The two boundaries datasets are compared in this script.

######### Libraries and functions ##########
library(devtools)
library(ggplot2)
# R wrapper for the ONS Open Geography Portal API:
# install_github('Chrisjb/rgeoportal') 
library(rgeoportal)
# DNO areas are downloaded directly from the National Grid ESO's portal

######### Data call ##########

##### ONS regions
# Ultra generalised (500m) are appropriate for the purposes of this code
regions_boundaries <- get_boundary(boundary_type = "administrative", 
                              boundary_name = 'Regions_December_2020_EN_BUC_V2')

##### DNO files
dno_boundaries <- st_read("data/DNO_License_Areas_20200506.shp")


# A simple visualisation
ggplot() +
  geom_sf(data = regions_boundaries, color = 'red', alpha = 0.5) +
  geom_sf(data = dno_boundaries, alpha = 0.5)
# Some DNO areas approximate the regions quite well, some less so.