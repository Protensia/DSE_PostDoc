

install.packages("gWidgetsRGtk2", dependencies = T)
library(gWidgetsRGtk2)

install.packages("MODIStsp", dependencies = T)

library(MODIStsp)
MODIStsp()

install.packages("exactextractr")



######## Note on the data downloaded #########
# File name: Surf_temp_Monthly_005dg (Daytime variables)
# Layer: M*D11C3
# Satellite: Terra
# 
# I download the surface monthly temperature data, which is 
# in kelvins. For each pixel, it represents the average monthly
# temperature in a 0.05 x 0.05 degrees area (roughly 5.1 x 5.1 km ).
# 
# Username: pabloj99
# pw: Pj03640840
#
# 
##############################################


library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(rgeos)
library(dplyr)
library(ggplot2)
library(sf)
library(exactextractr)




############### Processing temperature data ##############

# Read-in the polygon shapefile

setwd("C:\\Users\\hadunka2\\Desktop\\r_modis_extract")

output_folder <- "."  # Same as wd

zmbgrid <- st_read('.', 'zambia_grid_02deg_intersect')

#poly <- readOGR("C:\\Users\\hadunka2\\Desktop\\r_modis_example\\zambia_grid_02deg_intersect.shp")

#poly



###########################################
####Day Land Surface Temperature###########
###########################################

##### Data from 2001 to 2009 #####

#Import Land Surface Temperature Grids
grids <- list.files('.', pattern = "MOD11C3_LST_Day_CMG_2018.")
#grids2 <- list.files('.', pattern = "MOD11C3_LST_Day_CMG_2015.")
length<-length(grids)
length
s <- stack(grids)

#crs(zmbgrid)
#newcrs<-crs(s)        # They're different

# The command exacttextract transforms the polygons to the same CRS as the rasters


#zmbgrid_df <- data.frame(zmbgrid$FID_zambia)

modislt18 <- exact_extract(s, zmbgrid, fun = 'mean') 



#write.csv(modislt78, file = "modislt_78.csv")


modis_grid <- cbind(zmbgrid_df, modislt1_2, modislt3_4, modislt5_6,
                    modislt7_8, modislt9_10, modislt11_12, modislt13,
                    modislt14, modislt15, modislt16, modislt17, modislt18, 
                    modislt19)


write.csv(modis_grid, file = "modis_grid_1_19.csv")


## Save in BOX ##
write.csv(modis_grid, file = "C:\\Users\\hadunka2\\Box\\UIUC\\Papers\\Defor_FAW\\Data\\5_modis_temp\\modis_grid_10_19.csv")

write.csv(modis_grid, file = "C:\\Users\\hadunka2\\Box\\UIUC\\Papers\\Defor_FAW\\Data\\5_modis_temp\\modis_grid_1_9.csv")




