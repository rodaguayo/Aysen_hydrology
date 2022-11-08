rm(list=ls())
cat("\014")  

setwd("/home/rooda/Dropbox/Projects/Aysen_Hydrology/")
library("Evapotranspiration")
library("exactextractr")
library("terra")
library("sf")

# Data of polygons (and streamflow data) already merged.
period        <- c(as.Date("1990-01-01"), as.Date("2020-12-31"))
basin_shp     <- st_read("GIS/basins_aysen.shp")
basin_shp     <- st_transform(basin_shp, crs = 4326)
basin_data    <- st_drop_geometry(basin_shp)
period_total  <- seq(period[1], period[2]-1, by = "day")

data("constants")
constants$lat_rad <- -45*3.14/180

# Area
basin_data$Area  <- round(expanse(vect(basin_shp), unit="ha"), 2)

# Topographic attributes
dem   <- rast("/home/rooda/Dropbox/Patagonia/GIS South/dem_patagonia1.tif")
dem   <- crop(dem, vect(basin_shp), snap = "out")

slope <- terrain(dem, v='aspect', unit='degrees') 
basin_data$max_elevation   <- round(exact_extract(dem, basin_shp, "max"), 1)
basin_data$min_elevation   <- round(exact_extract(dem, basin_shp, "min"), 1)
basin_data$mean_elevation   <- round(exact_extract(dem, basin_shp, "mean"), 1)
basin_data$median_elevation <- round(exact_extract(dem, basin_shp, "median"), 1)
basin_data$slope            <- round(exact_extract(slope, basin_shp, "median"), 1)
basin_data$range            <- basin_data$max_elevation - basin_data$min_elevation

# Climate attributes
PP_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1980_2020m.nc")
PP_PMET <- subset(PP_PMET, which(time(PP_PMET) >= period[1] & time(PP_PMET) <= period[2]))
PP_PMET <- mean(tapp(PP_PMET, strftime(time(PP_PMET),format="%Y"), fun = sum, na.rm = TRUE))
PP_PMET <- crop(PP_PMET, vect(basin_shp), snap = "out")
PP_PMET <- resample(PP_PMET, dem, method = "bilinear")
basin_data$PP_annual <- round(exact_extract(PP_PMET, basin_shp, "mean"), 0)

PP_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1980_2020m.nc")
PP_PMET <- subset(PP_PMET, which(time(PP_PMET) >= period[1] & time(PP_PMET) <= period[2]))
PP_PMET <- tapp(PP_PMET, strftime(time(PP_PMET),format="%m"), fun = mean, na.rm = TRUE)
PP_PMET <- crop(PP_PMET, vect(basin_shp), snap = "out")
PP_PMET <- resample(PP_PMET, dem, method = "bilinear")
PP_PMET <- round(exact_extract(PP_PMET, basin_shp, "mean"), 0)
basin_data$PP_season <- NA 

for (i in 1:12) {
  basin_data$PP_season[i] <- round(sum(abs(as.numeric(PP_PMET[i,])-sum(as.numeric(PP_PMET[i,]))/12))/sum(as.numeric(PP_PMET[i,])),2)
}

T2M_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Temperature/Tavg_PMET_1980_2020m.nc")
T2M_PMET <- subset(T2M_PMET, which(time(T2M_PMET) >= period[1] & time(T2M_PMET) <= period[2]))
T2M_PMET <- mean(tapp(T2M_PMET, strftime(time(T2M_PMET),format="%Y"), fun = mean, na.rm = TRUE))
T2M_PMET <- crop(T2M_PMET, vect(basin_shp), snap = "out")
dem_hr <- rast("/home/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
dem_hr <- crop(dem_hr, T2M_PMET, snap = "out")
dem_lr <- resample(dem_hr,    T2M_PMET,  method="bilinear")
dem_lr <- resample(dem_lr,    dem_hr,    method="bilinear")
factor <- round((dem_lr-dem_hr)*0.0065,2)
T2M_PMET <- resample(T2M_PMET, factor, method = "near")
T2M_PMET <- T2M_PMET + factor
basin_data$T2M_annual <- round(exact_extract(T2M_PMET, basin_shp, "mean"), 2)

T2M_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Temperature/Tavg_PMET_1980_2020d.nc")
T2M_PMET <- subset(T2M_PMET, which(time(T2M_PMET) >= period[1] & time(T2M_PMET) <= period[2]))
T2M_PMET <- crop(T2M_PMET, vect(basin_shp), snap = "out")

PP_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1980_2020d.nc")
PP_PMET <- subset(PP_PMET, which(time(PP_PMET) >= period[1] & time(PP_PMET) <= period[2]))
PP_PMET <- crop(PP_PMET, vect(basin_shp), snap = "out")

basin_data$PP_snow <- NA
for (i in  1:12) {
  factor_i   <- crop(factor, vect(basin_shp)[i], snap = "out")
  T2M_PMET_i <- resample(T2M_PMET, factor_i, method = "bilinear")
  T2M_PMET_i <- T2M_PMET_i + factor_i
  
  PP_PMET_i  <- crop(PP_PMET, vect(basin_shp)[i], snap = "out")
  PP_PMET_i  <- resample(PP_PMET_i, factor_i, method = "bilinear")
  PP_PMET_i[T2M_PMET_i > 2] <- 0
  
  PP_PMET_i <- mean(tapp(PP_PMET_i, strftime(time(PP_PMET_i),format="%Y"), fun = sum, na.rm = TRUE))
  basin_data$PP_snow[i] <- round(exact_extract(PP_PMET_i, basin_shp[i,], "mean"), 0)
  print(i)
}

TMAX_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Temperature/Tmax_PMET_1980_2020d.nc")
TMIN_PMET <- rast("/home/rooda/Dropbox/Patagonia/Data/Temperature/Tmin_PMET_1980_2020d.nc")
TMAX_PMET <- subset(TMAX_PMET, which(time(TMAX_PMET) > period[1] & time(TMAX_PMET) <= period[2]))
TMIN_PMET <- subset(TMIN_PMET, which(time(TMIN_PMET) > period[1] & time(TMIN_PMET) <= period[2]))
TMAX_PMET <- crop(TMAX_PMET, vect(basin_shp), snap = "out")
TMIN_PMET <- crop(TMIN_PMET, vect(basin_shp), snap = "out")

basin_data$PET_annual <-NA
for (i in  1:12) {
  factor_i   <- crop(factor, vect(basin_shp)[i], snap = "out")
  TMAX_PMET_i <- crop(TMAX_PMET, vect(basin_shp)[i], snap = "out")
  TMIN_PMET_i <- crop(TMIN_PMET, vect(basin_shp)[i], snap = "out")
  TMAX_PMET_i <- resample(TMAX_PMET_i, factor_i, method = "bilinear")
  TMIN_PMET_i <- resample(TMIN_PMET_i, factor_i, method = "bilinear")
  TMAX_PMET_i <- TMAX_PMET_i + factor_i
  TMIN_PMET_i <- TMIN_PMET_i + factor_i
  
  TMAX_PMET_i <- as.numeric(round(exact_extract(TMAX_PMET_i, basin_shp[i,], "mean"), 2))
  TMIN_PMET_i <- as.numeric(round(exact_extract(TMIN_PMET_i, basin_shp[i,], "mean"), 2))
  
  climatedata <- data.frame("Tmax"  = TMAX_PMET_i, 
                          "Tmin"  = TMIN_PMET_i, 
                          "Month" = as.numeric(format(period_total, "%m")),
                          "Day"   = as.numeric(format(period_total, "%d")),
                          "Year"  = as.numeric(format(period_total, "%Y")))
  
  constants$Elev    <- basin_data$mean_elevation[i]
  climatedata <- ReadInputs(varnames= c("Tmax", "Tmin"), climatedata, constants, stopmissing =c(10,10,3), timestep = "daily", message = "no")
  pet_i <- ET.HargreavesSamani(climatedata, constants, ts="daily", message="no", AdditionalStats="no", save.csv="no")[["ET.Annual"]]
  basin_data$PET_annual[i] <- round(mean(pet_i),0)
  print(i)
}

basin_data$AI <- round(basin_data$PP_annual/basin_data$PET_annual, 2)
write.csv(basin_data, "/home/rooda/Dropbox/Projects/Aysen_Hydrology/Attributes.csv", row.names = FALSE)

