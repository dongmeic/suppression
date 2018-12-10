library(rgdal)
library(raster)
library(RColorBrewer)

# region of interest
mpb10km.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km"
mpb10km <- readOGR(dsn = mpb10km.path, layer = "mpb10km")
mpb10km.pts <- readOGR(dsn = mpb10km.path, layer = "mpb10km_us_gridpts")
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km.pts, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)
mpb10km.pts.r <- raster("/gpfs/projects/gavingrp/dongmeic/beetle/raster/mpb10km_grid.nc", varname = "etopo1")

crs <- proj4string(mpb10km)

get.raster <- function(shp, var, fun){
	cell.size=10000
	xmin <- -1006739; xmax <- 1050000.0; ymin <- -1722656; ymax <- 539131.6
	ncols <- (xmax - xmin)/cell.size; nrows <- (ymax - ymin)/cell.size
	r <- raster(nrows=nrows, ncols=ncols, ext=extent(mpb10km),crs = crs)
	rasterize(shp, r, var, fun=fun, na.rm=TRUE) 
}

rasterized <- function(shp, var, fun){
	r <- rasterize(shp, mpb10km.pts.r, var, fun=fun, na.rm=TRUE) 
	projection(r) <- crs
	r
}

fire.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/firedata/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
fwfod <- readOGR(dsn=paste0(fire.path, "wf_all_1980_2016"), layer="wf_all_1980_2016", 
								 stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
fwfod <- spTransform(fwfod, crs)
fpafod <- readOGR(dsn=paste0(fire.path, "RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb"), 
               layer="Fires", stringsAsFactors = FALSE, dropNULLGeometries = FALSE) 
fpafod <- spTransform(fpafod, crs)
# select naturally caused fires
fwfod.n <- fwfod[!is.na(fwfod$CAUSE) & fwfod$CAUSE == "Natural",]
# select suppressed fires
fwfod.n.s <- fwfod.n[!is.na(fwfod.n$FIRETYPE) & fwfod.n$FIRETYPE == "1",]

png(paste0(out,"fpafod_fwfod.png"), width = 10, height = 8, units = "in", res=300)
par(mfrow=c(1,2),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, border="black", main="FW-FOD fire records in the western US")
points(fwfod, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)
plot(mpb10km, border="black", main="FPA-FOD fire records in the western US")
points(fpafod, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)
dev.off()
png(paste0(out,"fwfod.png"), width = 10, height = 8, units = "in", res=300)
par(mfrow=c(1,2),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, border="black", main="FW-FOD natural fire records in the western US")
points(fwfod.n, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)
plot(mpb10km, border="black", main="FW-FOD suppressed natural fire records in the western US")
points(fwfod.n.s, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)
dev.off()

fwfod.n.s$FIRETYPE <- as.numeric(fwfod.n.s$FIRETYPE)
fwfod.n$Natural <- rep(1, length(fwfod.n$CAUSE))
fire.sprs <- rasterized(fwfod.n.s, "FIRETYPE", sum)
fire.natr <- rasterized(fwfod.n, "Natural", sum)
pct.sprs <- fire.sprs/fire.natr

mapping <- function(outnm, r, title, cols){
	png(paste0(out, outnm, ".png"), width = 8, height = 8, units = "in", res=300)
	par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,5))
	plot(mpb10km,main=title,bord="white")
	ncls <- 5
	v <-r@data@values
	if(min(na.omit(v))%%1==0){
		d <- 0
	}else{
	  d <- 2
	}
	clIn <- classIntervals(v, n = ncls, style = "kmeans")
	plot(r, breaks=round(clIn$brks, digits=d), col = brewer.pal(ncls,cols), axes=FALSE, box=FALSE, add=T)
	plot(mpb10km, bord=alpha("black", alpha = 0.6), lwd=0.5, add=T)
	dev.off()
}

mapping("pct_sprs", pct.sprs, "Percent of naturally-caused fires suppressed", "Reds")
mapping("fire_sprs", fire.sprs, "Number of naturally-caused fires suppressed", "Reds")
