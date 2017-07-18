shapepath <- "/Volumes/dongmeic/beetle/data/vector/na10km_v2/"
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)
par(mfrow=c(1,1),xpd=TRUE,mar=c(.5,.5,.5,.5))

coast_shapefile <- paste(shapepath, "na10km_coast.shp", sep="")
layer <- ogrListLayers(coast_shapefile)
ogrInfo(coast_shapefile, layer=layer)
coast_shp <- readShapeLines(coast_shapefile, proj4string=na10km_crs) 
summary(coast_shp)
plot(coast_shp)

land_shapefile <- paste(shapepath, "na10km_land.shp", sep="")
layer <- ogrListLayers(land_shapefile)
ogrInfo(land_shapefile, layer=layer)
land_shp <- readShapePoly(land_shapefile, proj4string=na10km_crs)
summary(land_shp) 
plot(land_shp)

nalrglakes_shapefile <- paste(shapepath, "na10km_lrglakes.shp", sep="")
layer <- ogrListLayers(nalrglakes_shapefile)
ogrInfo(nalrglakes_shapefile, layer=layer)
lrglakes_shp <- readShapePoly(nalrglakes_shapefile, proj4string=na10km_crs) 
summary(lrglakes_shp)
plot(lrglakes_shp, bor="lightblue", add=TRUE)

us_state_shapefile <- paste(shapepath, "na10km_us_state.shp", sep="")
layer <- ogrListLayers(us_state_shapefile)
ogrInfo(us_state_shapefile, layer=layer)
us_state_shp <- readShapeLines(us_state_shapefile, proj4string=na10km_crs)
summary(us_state_shp)
plot(us_state_shp, col="grey", add=TRUE)

us_shapefile <- paste(shapepath, "na10km_us.shp", sep="")
layer <- ogrListLayers(us_shapefile)
ogrInfo(us_shapefile, layer=layer)
us_shp <- readShapeLines(us_shapefile, proj4string=na10km_crs)
summary(us_shp)
plot(us_shp, col="red", add=TRUE)

coast_shp_proj <- spTransform(coast_shp, na10km_crs)
summary(coast_shp_proj)
plot(coast_shp_proj)

land_shp_proj <- spTransform(land_shp, na10km_crs)
summary(land_shp_proj)
plot(land_shp_proj, bor="green", add=TRUE)

us_state_shp_proj <- spTransform(us_state_shp, na10km_crs)
summary(us_state_shp_proj)
plot(us_state_shp_proj, col="lightblue", add=TRUE)

us_shp_proj <- spTransform(us_shp, na10km_crs)
summary(us_shp_proj)
plot(us_shp_proj, col="blue", add=TRUE)

lrglakes_shp_proj <- spTransform(lrglakes_shp, na10km_crs)
summary(lrglakes_shp_proj)
plot(lrglakes_shp_proj, bor="purple", add=TRUE)

library(raster)
wus10km_bb <- as(extent(-2100000,680000,-2720000,180000), "SpatialPolygons")
proj4string(wus10km_bb) <- CRS(proj4string(coast_shp_proj))
wus10km_coast_shp_proj <- gIntersection(coast_shp_proj, wus10km_bb)

proj4string(land_shp_proj) <- proj4string(coast_shp_proj)
wus10km_land_shp_proj <- gIntersection(land_shp_proj, byid=TRUE, wus10km_bb)
polygon1 <- gBuffer(land_shp_proj, width=0)
polygon2 <- gBuffer(wus10km_bb, byid=TRUE, width=0)
wus10km_land_shp_proj <- gIntersection(polygon1, polygon2, byid=TRUE)

wus10km_us_state_shp_proj <- gIntersection(us_state_shp_proj, byid=TRUE, wus10km_bb)
wus10km_us_shp_proj <- gIntersection(us_shp_proj, byid=TRUE, wus10km_bb)
wus10km_lrglakes_shp_proj <- gIntersection(lrglakes_shp_proj, byid=TRUE, wus10km_bb)

#wus10km_bb <- as(wus10km_bb, "SpatialLines")
wus10km_us_state_shp_proj <- as(wus10km_us_state_shp_proj, "SpatialLines")
wus10km_us_shp_proj <- as(wus10km_us_shp_proj, "SpatialLines")
wus10km_lrglakes_shp_proj <- as(wus10km_lrglakes_shp_proj, "SpatialPolygons")

trellis.device("pdf", file = "wus_10km_01.pdf")
plot(wus10km_bb, col="gray90")
plot(wus10km_land_shp_proj, col="gray", bor="purple", add=TRUE)
plot(wus10km_us_state_shp_proj, lwd=0.2, col="red", add=TRUE)
plot(wus10km_us_shp_proj, lwd=0.2, col="black", add=TRUE)
plot(wus10km_lrglakes_shp_proj, lwd=0.2, col="blue", add=TRUE)
plot(wus10km_coast_shp_proj, lwd=0.3, add=TRUE)
text(-2100000, 220000, pos=c(4), offset=0.0, cex=1.25, "wus10km -- 10km Outlines")
dev.off()
```

```{r write output, eval=FALSE, include=TRUE}
outpath <- "/Volumes/dongmeic/beetle/wus10km_v1/shapefiles/"
outshape <- wus10km_bb
outfile <- "wus10km_bb"
outshapefile <- paste(outpath,outfile,sep="")
spdf <- data.frame(as.numeric(row.names(outshape)))
row.names(spdf) <- row.names(outshape)
outshape <- SpatialPolygonsDataFrame(outshape, spdf)
writePolyShape(outshape, outshapefile, factor2char=TRUE)

test <- readShapePoly(outshapefile)
plot(test, col="gray90")

outshape <- wus10km_coast_shp_proj
outfile <- "wus10km_coast"
outshapefile <- paste(outpath,outfile,sep="")
sldf <- data.frame(as.numeric(row.names(outshape)))
row.names(sldf) <- row.names(outshape)
outshape <- SpatialLinesDataFrame(outshape, sldf, match.ID = TRUE)
writeLinesShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapeLines(outshapefile)
plot(test, col="gray90")

outshape <- wus10km_land_shp_proj
outfile <- "wus10km_land"
outshapefile <- paste(outpath,outfile,sep="")
sapply(slot(outshape, "polygons"), function(x) slot(x, "ID"))
spdf <- data.frame(1)
row.names(spdf) <- "buffer 1"
outshape <- SpatialPolygonsDataFrame(outshape, spdf, match.ID = TRUE)
writePolyShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapePoly(outshapefile)
plot(test, col="gray90", bor="black")

outshape <- wus10km_us_state_shp_proj
outfile <- "wus10km_us_state"
outshapefile <- paste(outpath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- row.names(outshape)
outshape <- SpatialLinesDataFrame(outshape, sldf, match.ID = TRUE)
writeLinesShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapeLines(outshapefile)
plot(test, col="green", add=TRUE)

outshape <- wus10km_us_shp_proj
outfile <- "wus10km_us"
outshapefile <- paste(outpath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- row.names(outshape)
outshape <- SpatialLinesDataFrame(outshape, sldf, match.ID = TRUE)
writeLinesShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapeLines(outshapefile)
plot(test, col="green", add=TRUE)

outshape <- wus10km_lrglakes_shp_proj
outfile <- "wus10km_lrglakes"
outshapefile <- paste(outpath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- row.names(outshape)
outshape <- SpatialPolygonsDataFrame(outshape, sldf, match.ID = TRUE)
writePolyShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapePoly(outshapefile)
plot(test, col="lightblue", bor="blue", add=TRUE)

datapath <- "/Volumes/dongmeic/beetle/data/text/wus10km_v1/"
shapepath <- "/Volumes/dongmeic/beetle/data/vector/wus10km_v1/"

shapefile = "wus10km_bb.shp"
bb_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
plot(bb_shp, col="gray90")

shapefile = "wus10km_us.shp"
us_shp <- readShapeLines(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
plot(us_shp, col="gray", add=TRUE)

shapefile = "wus10km_coast.shp"
coast_shp <- readShapeLines(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
plot(coast_shp, add=TRUE)

shapefile = "wus10km_land.shp"
land_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
plot(land_shp, col="gray", bor="black")

shapefile = "wus10km_lrglakes.shp"
lrglakes_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
plot(lrglakes_shp, col="lightblue", bor="blue", add=TRUE)

# generate a grid, and the four corner points
xmin = -2100*1000; xmax = 680*1000; ymin = -2720*1000; ymax = 180*1000
grid_spacing <- 10*1000*1
x <- seq(xmin,xmax,by=grid_spacing)
nx <- length(x); nx
head(x); tail(x)
y <- seq(ymin,ymax,by=grid_spacing)
ny <- length(y); ny
head(y); tail(y)
grid_xy <- expand.grid(x,y)
grid_xy_pts <- SpatialPoints(grid_xy, proj4string=na10km_crs)
# trim
grid_land_xy_pts <- gIntersection(land_shp, grid_xy_pts, byid=FALSE)

pdf(file = "wus10km_v1_pts.pdf")
plot(bb_shp, col="gray95")
plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
plot(lrglakes_shp, lwd=0.2, col="lightblue", bor="blue", add=TRUE)
plot(coast_shp, lwd=0.3, add=TRUE)
plot(grid_land_xy_pts, pch=16, cex=0.1, col="green", add=TRUE)
text(-2100000, 220000, pos=c(4), offset=0.0, cex=1.0, "wus10km -- Points")
plot(bb_shp, add=TRUE)
dev.off()

# knock out large lake points
grid_lrglakes_xy_pts <- gIntersection(lrglakes_shp, grid_land_xy_pts, byid=FALSE)

pdf(file = "wus10km_v1_pts_lrglakes.pdf")
plot(bb_shp, col="gray95")
plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
plot(lrglakes_shp, lwd=0.2, col="lightblue", bor="blue", add=TRUE)
plot(coast_shp, lwd=0.3, add=TRUE)
plot(grid_lrglakes_xy_pts, pch=16, cex=0.1, col="purple", add=TRUE)
text(-2100000, 220000, pos=c(4), offset=0.0, cex=1.0, "wus10km -- Large Lakes")
plot(bb_shp, add=TRUE)
dev.off()

# add row names
row.names(grid_land_xy_pts) <- as.character(seq(1:length(row.names(grid_land_xy_pts))))
row.names(grid_lrglakes_xy_pts) <- as.character(seq(1:length(row.names(grid_lrglakes_xy_pts))))
head(cbind(grid_land_xy_pts$x,grid_land_xy_pts$y))
tail(cbind(grid_land_xy_pts$x,grid_land_xy_pts$y))

outshape <- grid_land_xy_pts
outfile <- "wus10km_land_pts"
outshapefile <- paste(shapepath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- as.character(seq(1:length(row.names(outshape))))
outshape <- SpatialPointsDataFrame(outshape, sldf, match.ID = TRUE)
writePointsShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapePoints(outshapefile)
pdf(file = "grid_land_xy_pts.pdf")
plot(land_shp, col="gray", bor="black", lwd=0.1)
plot(test, col="purple", pch=16, cex=0.1, add=TRUE)
dev.off()

outshape <- grid_lrglakes_xy_pts
outfile <- "wus10km_lrglakes_pts"
outshapefile <- paste(shapepath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- as.character(seq(1:length(row.names(outshape))))
outshape <- SpatialPointsDataFrame(outshape, sldf, match.ID = TRUE)
writePointsShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapePoints(outshapefile)
pdf(file = "grid_lrglakes_xy_pts.pdf")
plot(land_shp, col="gray", bor="black", lwd=0.1)
plot(test, col="purple", pch=16, cex=0.1, add=TRUE)
dev.off()

# unproject points
longlat_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
grid_lonlat_pts <- spTransform(grid_xy_pts, longlat_crs)
grid_land_lonlat_pts <- spTransform(grid_land_xy_pts, longlat_crs)
head(cbind(grid_land_lonlat_pts$x,grid_land_lonlat_pts$y))
tail(cbind(grid_land_lonlat_pts$x,grid_land_lonlat_pts$y))
min(grid_land_lonlat_pts$x); max(grid_land_lonlat_pts$x)
min(grid_land_lonlat_pts$y); max(grid_land_lonlat_pts$y)

pdf(file = "grid_land_lonlat_pts.pdf")
plot(grid_lonlat_pts, pch=16, cex=0.1, col="lightblue")
plot(grid_land_lonlat_pts, pch=16, cex=0.1, add=TRUE)
dev.off()

# create a data frame
npts <- nx*ny
wus10km_v1 <- as.data.frame(cbind(grid_xy_pts@coords,grid_lonlat_pts@coords, rep(0,npts), rep(0,npts), rep(0,npts)))
row.names(wus10km_v1) <- seq(1:npts) 
names(wus10km_v1) <- c("x", "y", "lon", "lat", "mask", "etopo1", "srtm30")
head(wus10km_v1)

# set mask using grid_land_xy_pts and grid_lrglakes_xy_pts
j <- match(grid_land_xy_pts$x, x)
k <- match(grid_land_xy_pts$y, y)
wus10km_v1$mask[((k-1)*nx)+j] <- 1
j <- match(grid_lrglakes_xy_pts$x, x)
k <- match(grid_lrglakes_xy_pts$y, y)
wus10km_v1$mask[((k-1)*nx)+j] <- 0

#mask shapefile
mask <- wus10km_v1[wus10km_v1$mask == 1, 1:4]
str(mask)
grid_mask_xy_pts <- SpatialPoints(coords=cbind(mask[,1:2]), proj4string=CRS(na10km_projstr))
summary(grid_mask_xy_pts)
outshape <- grid_mask_xy_pts
outfile <- "wus10km_mask_pts"
outshapefile <- paste(shapepath,outfile,sep="")
sldf <- data.frame(seq(1:length(row.names(outshape))))
row.names(sldf) <- as.character(seq(1:length(row.names(outshape))))
outshape <- SpatialPointsDataFrame(outshape, sldf, match.ID = FALSE)
writePointsShape(outshape, outshapefile, factor2char=TRUE )

test <- readShapePoints(outshapefile)
pdf(file = "grid_mask_xy_pts.pdf")
plot(land_shp, col="gray", bor="black", lwd=0.1)
plot(test, col="purple", pch=16, cex=0.1, add=TRUE)
dev.off()

# read elevations
library(ncdf4)
etopo1file <- "/Volumes/BeetleProject/Data/ETOPO1/etopo1_ig.nc"
etopo1 <- raster(etopo1file)
etopo1
library(rasterVis)
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "BrBG")))
cutpts <- c(-10000, -4000, -3000, -2000, -1000, 0, 1000, 2000, 3000, 4000, 5000)
library(lattice)
levelplot(etopo1, margin = FALSE, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme)

srtm30file <- "/Volumes/BeetleProject/Data/SRTM30Plus/topo1.grd.nc"
srtm30 <- raster(srtm30file)
srtm30
srtm30_rotate <- rotate(srtm30)
srtm30_rotate
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "BrBG")))
cutpts <- c(-10000, -4000, -3000, -2000, -1000, 0, 1000, 2000, 3000, 4000, 5000)
levelplot(srtm30_rotate, margin = FALSE, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme)

# extract etopo1 values
ptm <- proc.time() 
etopo1_elev <- extract(etopo1,cbind(wus10km_v1$lon,wus10km_v1$lat))
proc.time() - ptm
etopo1_array <- array(etopo1_elev, dim=c(nx,ny))
levelplot(etopo1_array, margin = FALSE, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme)
etopo1_land <- extract(etopo1,cbind(mask$lon,mask$lat))

# extract srtm30 values
ptm <- proc.time() 
srtm30_elev <- extract(srtm30_rotate,cbind(wus10km_v1$lon,wus10km_v1$lat))
proc.time() - ptm
srtm30_array <- array(srtm30_elev, dim=c(nx,ny))
levelplot(srtm30_array, margin = FALSE, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme)
srtm30_land <- extract(srtm30_rotate,cbind(mask$lon,mask$lat))

wus10km_v1$etopo1 <- etopo1_elev
wus10km_v1$srtm30 <- srtm30_elev
head(wus10km_v1); tail(wus10km_v1)

pdf(file = "wus10km_v1_pts.pdf")
plot(bb_shp, col="gray95")
plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
plot(lrglakes_shp, lwd=0.2, col="lightblue", bor="blue", add=TRUE)
plot(coast_shp, lwd=0.3, add=TRUE)
plot(grid_mask_xy_pts, pch=16, cex=0.1, col="red", add=TRUE)
text(-2100000, 220000, pos=c(4), offset=0.0, cex=1.0, "wus10km_v1 -- Points")
plot(bb_shp, add=TRUE)
dev.off()

# write out x, y, lon, lat and elevation for land-only points
wus10km_v1_land <- wus10km_v1[wus10km_v1$mask == 1, ]
head(wus10km_v1_land); tail(wus10km_v1_land)
outfile <- paste(datapath, "wus10km_v1.csv", sep="")
write.table(wus10km_v1_land, outfile, sep=",", row.names=FALSE)

# netCDF file of mask values
# create and write the netCDF file -- ncdf4 version

ncfname <- "/Volumes/dongmeic/beetle/data/raster/wus10km_v1/wus10km_v1.nc"

lon <- array(wus10km_v1$lon, dim=c(nx,ny)); lat <- array(wus10km_v1$lat, dim=c(nx,ny))
mask <- array(wus10km_v1$mask, dim=c(nx,ny))

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))

# define variables
fillvalue <- 1e32
ifill <- 9
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")

dlname <- "landmask"
mask_def <- ncvar_def("landmask","1",list(xdim,ydim),ifill,dlname,prec="integer")
dlname <- "etopo1"
etopo1_def <- ncvar_def("etopo1","1",list(xdim,ydim),ifill,dlname,prec="integer")
dlname <- "srtm30"
srtm30_def <- ncvar_def("srtm30","1",list(xdim,ydim),ifill,dlname,prec="integer")

projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# create netCDF file and put data
ncout <- nc_create(ncfname,list(lon_def,lat_def,mask_def,etopo1_def,srtm30_def,proj_def),force_v4=T)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"x","axis","X")
ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
ncatt_put(ncout,"x","grid_spacing","10000 m")
ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
ncatt_put(ncout,"y","axis","Y")
ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
ncatt_put(ncout,"y","grid_spacing","10000 m")
ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")

ncatt_put(ncout,"landmask","missing_value",ifill)
ncatt_put(ncout,"landmask","long_name","Landmask from NaturalEarth 10m Data")
ncatt_put(ncout,"etopo1","missing_value",ifill)
ncatt_put(ncout,"etopo1","data_source","/Data/ETOPO1/nc_files/etopo1_ig.nc")
ncatt_put(ncout,"etopo1","long_name","ETOPO1 DEM")
ncatt_put(ncout,"etopo1","source_resolution","1-min")
ncatt_put(ncout,"srtm30","missing_value",ifill)
ncatt_put(ncout,"srtm30","data_source","/Data/srtm30plus/topo1.grd.nc")
ncatt_put(ncout,"srtm30","long_name","SRTM30 Plus DEM")
ncatt_put(ncout,"srtm30","source_resolution","1-min")

ncatt_put(ncout,projname,"name",projname)
ncatt_put(ncout,projname,"long_name",projname)
ncatt_put(ncout,projname,"grid_mapping_name",projname)
ncatt_put(ncout,projname,"longitude_of_projection_origin",-100.0)
ncatt_put(ncout,projname,"latitude_of_projection_origin",50.0)
ncatt_put(ncout,projname,"earth_shape","6370997 m")
ncatt_put(ncout,projname,"_CoordinateTransformType","Projection")
ncatt_put(ncout,projname,"_CoordinateAxisTypes","GeoX GeoY")
ncatt_put(ncout,projname,"CRS.PROJ.4",na10km_projstr)

# put variables
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)
ncvar_put(ncout,mask_def,mask)
ncvar_put(ncout,etopo1_def,etopo1_array)
ncvar_put(ncout,srtm30_def,srtm30_array)

# add global attributes
ncatt_put(ncout,0,"title","wus10km_v1 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by wus10km_v1_01.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)