# Barry 'Spacedman' Rowlingson CheatSheet:
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html
#
# GIS in R: overview of many fancy tools
# http://pakillo.github.io/R-GIS-tutorial/
#
# Natural Earth: depository of free geographic maps (physical + human geography) as shapefiles
# http://www.naturalearthdata.com/
#
# GADM: spatial data for precise human geography (all administrative levels)
# http://www.gadm.org/
#
# GPlates: software for paleogeographic reconstructions
# http://www.gplates.org/
#


################### Shapefiles ###########################
library(rgdal)

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_coastline.zip","coastline.zip")
unzip("coastline.zip")
coast <- readOGR(dsn=".",layer="ne_110m_coastline")
#coast <- readShapeSpatial("ne_110m_coastline.shp")
plot(coast)
coast@data
coast@bbox
coast@proj4string
plot(coast, col=1:10)
plot(coast[50,],lwd=4,add=TRUE, col="red")

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip","land.zip")
unzip("land.zip")
land <- readOGR(dsn=".",layer="ne_110m_land")
plot(land, col=1:10)

# Creating a simple shapefile
home <- data.frame(Longitude=1.1447, Latitude=44.8411, Value="My Hometown", Another_Value=24)
home <- SpatialPointsDataFrame(home[,1:2], data=home)
plot(coast)
plot(home, add=TRUE, pch=19, col="red")

writeOGR(home, dsn=".", layer="home", driver="ESRI Shapefile")

proj4string(home) <- CRS("+proj=longlat")
writeOGR(home, dsn="home.kml", layer="Me", driver="KML")
readOGR(dsn="home.kml",layer="Me")

# Checking which formats are available
ogrDrivers()
gdalDrivers()

# Creating a more complex shapefile
lon <- seq(-180,180,by=1)
lat <- rep(-30,length=length(lon))
ab <- cbind(lon,lat)
AB <- Line(ab)
lAB <- Lines(list(AB), ID="AB")
lAB <- SpatialLines(list(lAB), proj4string=CRS("+proj=longlat"))
plot(coast)
plot(lAB, add=TRUE, col="red", lwd=2)

plot(coast)
lines(ab, col="red", lwd=2)

lAB_laea <- spTransform(lAB,CRS("+proj=laea +lat_0=-90 +lon_0=0"))
coast_laea <- spTransform(coast,CRS("+proj=laea +lat_0=-90 +lon_0=0"))
plot(coast_laea)
plot(lAB_laea,add=TRUE,col="red",lwd=2)

quartz(height=4, width=4)
par(mar=c(0,0,0,0))
plot(lAB_laea, col="red", lwd=2,xaxs="i",yaxs="i")
plot(coast_laea,add=TRUE)

################### Rasters ################################
library(raster)
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/NE1_50M_SR.zip","raster.zip")
unzip("raster.zip")
world <- raster("NE1_50M_SR/NE1_50M_SR.tif")
plot(world)
writeRaster(world, "world.grd")

################### NetCDF ################################
library(ncdf4)

yest <- Sys.Date()-1
url <- "ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/L4/GLOB/UKMO/OSTIA/2016/062/20160302-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2"
download.file(url,destfile="sst.bz2")
#system(sprintf("bunzip2 %s/sst.bz2", getwd()))

sst <- nc_open("sst")
an_sst <- ncvar_get(sst, "analysed_sst")
lon <- ncvar_get(sst, "lon")
lat <- ncvar_get(sst, "lat")
nc_close(sst)

lower_res <- an_sst[seq(1,nrow(an_sst),by=10),seq(1,ncol(an_sst),by=10)]
lon <- lon[seq(1,nrow(an_sst),by=10)]
lat <- lat[seq(1,ncol(an_sst),by=10)]
celsius <- lower_res - 273.15

par(mar=c(0,0,0,0))
breaks <- c(-100, seq(-20,40,by=2), 100)
image(x=lon, y=lat, z=celsius, col=rev(heat.colors(length(breaks)-1)), breaks=breaks, xlim=c(-180,180), ylim=c(-90,90))

sst_list <- list(x=lon, y=lat, z=celsius) #As a list
sst_grd <- image2Grid(sst_list) #As a SpatialGridDataFrame
writeGDAL(sst_grd, "sst.tif", drivername="GTiff")
sst_shp <- as(sst_grd,"SpatialPolygonsDataFrame") #As a SpatialPolygonsDataFrame
writeOGR(sst_shp, dsn=".", layer="sst", driver="ESRI Shapefile")