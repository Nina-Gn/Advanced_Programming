setwd("/home/nina/Dokumente/_Advanced_Methods/Advanced_MET/PerformanceRecord/Task_1")

## Interests
# https://www.erik.vansebille.com/science/
# http://www.plasticadrift.org/
# http://oceanparcels.org/    

### Course Task 1: Test of packages OceanView % Oceanmap, to enhance visualisation capabilities of oceanographic data
# https://cran.r-project.org/web/packages/OceanView/vignettes/Northsea.pdf
# https://cran.r-project.org/web/packages/oceanmap/oceanmap.pdf 

# This is my first work containing the NetCDF file format, aim is to get a better understanding of these files.

# necessary libraries
libs <- sapply(c("RNetCDF", "OceanView", "plot3D", "plot3Drgl", "rgl", "rasterVis", "ggplot2"), function(x){
  tryCatch(library(x, character.only = T), error = function(e){
    install.packages(x, dependencies = T)
    library(x, character.only = T)
  })
})

# basic function structure is:
# myfunction <- function(arg1, arg2, ... ){
#  statements
#  return(object)
# }

dir <- "/home/nina/Dokumente/_Advanced_Methods/Advanced_MET/PerformanceRecord/Task_1/"
ocean_nc <- open.nc(paste0(dir,"global-analysis-forecast-phy-001-024-monthly_1561328218562.nc")) # get data
print.nc(ocean_nc) # to see all available attributes, time and depth are 0 here

### 1st function/ visualization: Plot Velocity of Sea Water in N,E,S and W-ward direction
# Desired information are the short vo (= Northward velocity of sea water) and short uo (= Eastward velocity of sea water) 
# Until now, there has always been the challenge that Northward-Velocity and the Eastward-Velocity are seperated datasets if 
# they are looked at in QGIS. R has capabilities to join them. Importing data:
lat <- var.get.nc(ocean_nc, "latitude")
long <- var.get.nc(ocean_nc, "longitude")
Nvel <- var.get.nc(ocean_nc, "vo") # Northward Velocity
Evel <- var.get.nc(ocean_nc, "uo") # Eastward Velocity

# The ranges of the velocities
base::range(Nvel, na.rm=TRUE) # na.rm indicates that NA's should be omitted; Unit: Meter per second; result: -3341  3202
base::range(Evel, na.rm = TRUE) # result: -3108  3482
hist(Nvel) # also interesting to see
hist(Evel)

NC_Velocity <- function(Nvel,Evel){
  Ev <- apply(Evel, FUN = mean, MARGIN = 1:2) # MARGIN = 1:2 indicates rows and columns --> for image2D command, a matrix is needed containing lat and long
  Nv <- apply(Nvel, FUN = mean, MARGIN = 1:2)
  meanV <- sqrt(Ev^2 + Nv^2)
  return(meanV)
}

graphics::par(mfrow = c(1, 1)) # has influence on size
col <- jet2.col(100) # jet2.col is a suitable color scheme, not too dark
image2D(z = NC_Velocity(Nvel = Nvel, Evel = Evel), x = long, y = lat, 
        col = col, NAcol = grey(0.4),  # col = col
        main = "Mean velocity", clab = c("","","m/s"))

### -----------------------------------------------------------------------------------------------

  ### 2nd function/ visualization - Concentration of particles in Sea Water over the year 2017
ocean_nc_2 <- open.nc(paste0(dir,"dataset-ibi-reanalysis-bio-005-003-monthly_1561321147282.nc")) # get data
print.nc(ocean_nc_2) # to see all available attributes
  
  # Below are concentrations of suspended matter 
  # The functions below can be executed with all types of suspended matter.
lat <- var.get.nc(ocean_nc_2, "latitude")
long <- var.get.nc(ocean_nc_2, "longitude")
time <- var.get.nc(ocean_nc_2, "time") 
  #
Chl <- var.get.nc(ocean_nc_2, "chl") # Chlorophll # unit: mg.m-3
Oxy <- var.get.nc(ocean_nc_2, "o2") # Oxygen # unit: mmol.m-3
Nitr <- var.get.nc(ocean_nc_2, "no3") # Nitrate # unit: mmol.m-3
Phosp <- var.get.nc(ocean_nc_2, "po4") # Phosphate # unit: mmol.m-3
Phytop <- var.get.nc(ocean_nc_2, "phyc") # Phytoplancton # unit: mmol.m-3
Silic <- var.get.nc(ocean_nc_2, "si") # Silicate # unit: mmol.m-3
Iron <- var.get.nc(ocean_nc_2, "fe") # Iron # unit: mmol.m-3
Ammonium <- var.get.nc(ocean_nc_2, "nh4") # Ammonium # unit: mmol.m-3
NPPV <- var.get.nc(ocean_nc_2, "nppv") # Net primary productivity of carbon # unit: mg.m-3.day-1
  
        # calculate time, since the data is provided in hours after year 1950 (unit is: hours after 01.01.1950)
dtm <- strptime(c("1.1.1950 00:00"), format = "%d.%m.%Y %H:%M", tz = "CET")
hrs <- function(u) {
    x <- u * 3600
    return(x)
  }
u <- time # hours are too hard to understand!
for (x in u){  # loop returns the days + hours after year 1950
     print(paste("Dates are:", dtm + hrs(x)))
  }
  
dates <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "Oktober", "November", "December")

suspendedMatter <-function(x){
    ConcPerDate <- apply(x, FUN = mean, MARGIN = 1:3) # MARGIN 1:3, for image2D a matrix is needed containing lat, long and time
    return(ConcPerDate)
  }

suspendedMatter_Figure <- function(y, name, unit){
    image2D(z = suspendedMatter(y), x = long, y = lat, 
          col = col, NAcol = grey(0.4),
          main = sprintf("2017's Concentration of Suspended Matter %s - %s", name, dates), clab = c("","", sprintf("%s", unit)))
  }
  
suspendedMatter_Figure(Chl, name = "\n Chlorophyll", unit = "mg m⁻³")
suspendedMatter_Figure(Nitr, name = "\n Nitrat", unit = "mmol m⁻³")
suspendedMatter_Figure(Oxy, name = "\n Oxygen", unit = "mmol m⁻³")
suspendedMatter_Figure(Phosp, name = "\n Phosphate", unit = "mmol m⁻³")
suspendedMatter_Figure(Phytop, name = "\n Phytoplankton", unit = "mmol m⁻³")
suspendedMatter_Figure(Silic, name = "\n Silicate", unit = "mmol m⁻³")
suspendedMatter_Figure(Iron, name = "\n Iron", unit = "mmol m⁻³")
suspendedMatter_Figure(Ammonium, name = "\n Ammonium", unit = "mmol m⁻³")
suspendedMatter_Figure(NPPV, name = "\n Net primary productivity of carbon", unit = "mg m⁻³ day⁻¹")

## Possible Improvements:
# get it to insert the names of the suspended matter + units  ---  check
# ... and get the name of month on each image  ---  check
### -----------------------------------------------------------------------------------------------

### 3rd function - depth
ocean_nc_3 <- open.nc(paste0(dir,"MetO-GLO-PHYS-dm-SSH_1561479714370.nc"))
print.nc(ocean_nc_3) # to see all available attributes, time is 1, depth not stated

lat <- var.get.nc(ocean_nc_3, "lat")
long <- var.get.nc(ocean_nc_3, "lon")
zos <- var.get.nc(ocean_nc_3, "zos")
base::range(zos, na.rm= TRUE)

sea_surface_height <- image2D(z = zos, x = long, y = lat,  # zos = Sea surface height above geoid in m
        col = col, NAcol = grey(0.4),
        main = "Sea surface height above geoid", clab = c("","","m"))

persp3D(z = zos, x = long, y = lat, main = "Sea Surface Height above Geoid in meter" , texture = system.file("./VIS3_sea_surface_height_above_geoid.png"))
## Improvements:
# make it 3D   ---   check