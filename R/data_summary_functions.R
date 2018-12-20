# functions
library(raster)
library(classInt)
library(RColorBrewer)
library(Scale)

gini <- function(x, na.rm=TRUE){
	l <- length(x)
	a <- rep(1, l)/l
	b <- sort(x/sum(x))
	q <- c(rep(0,l,each=1)) 
  p <- c(rep(0,l,each=1))
  z <- c(rep(0,l,each=1))
  p[1] <- a[1] # number
  q[1] <- b[1] # size
  z[1] <- q[1]*p[1]
	if(l >= 2){		
		for(k in 2:l){
			p[k] <- a[k]+p[k-1]
			q[k] <- b[k]+q[k-1]
			z[k]=(q[k]+q[k-1])*(p[k]-p[k-1]) 				
		}
		1-sum(z)
	}else{
		NA
	}
}

mode <- function(x) {
  return(names(sort(-table(na.omit(x))))[1])
}

median.fire.size <- function(x, na.rm=TRUE){
	x[which(x>24710)] <- 24710
  median(x)
}

count <- function(x, na.rm=TRUE){
	log(length(x))
}

sum.log <- function(x, na.rm=TRUE){
	log(sum(x)+1)
}

# region of interest
library(rgdal)
mpb10km.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km"
mpb10km <- readOGR(dsn = mpb10km.path, layer = "mpb10km")
crs <- proj4string(mpb10km)
lonlat <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
mapping <- function(outnm, r, title, d, cols, sty){
	png(paste0(out, outnm, ".png"), width = 8, height = 8, units = "in", res=300)
	par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,6))
	plot(mpb10km,main=title,bord="white")
	ncls <- 5
	v <-getValues(r)
	clIn <- classIntervals(v, n = ncls, style = sty)
	plot(r, breaks=round(clIn$brks, digits=d), col = brewer.pal(ncls,cols), legend=FALSE, axes=FALSE, box=FALSE, add=T)
	plot(r, breaks=round(clIn$brks, digits=d), legend.only=TRUE, col=brewer.pal(ncls,cols), legend.width=1, legend.shrink=0.75,
    smallplot=c(0.83,.85,.1,.9))
	plot(mpb10km, bord=alpha("black", alpha = 0.6), lwd=0.5, add=T)
	dev.off()
}

df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  xy.n <- spTransform(xy, crs)
  spdf <- SpatialPointsDataFrame(coords = xy.n, data = df, proj4string = crs)
  return(spdf)
}

get.raster <- function(shp, var, fun){
	cell.size=100000
	xmin <- -1006739; xmax <- 1050000.0; ymin <- -1722656; ymax <- 539131.6
	ncols <- (xmax - xmin)/cell.size; nrows <- (ymax - ymin)/cell.size
	r <- raster(nrows=nrows, ncols=ncols, ext=extent(mpb10km),crs = crs)
	rasterize(shp, r, var, fun=fun, na.rm=TRUE) 
}

mpb10km.pts.r <- raster("/gpfs/projects/gavingrp/dongmeic/beetle/raster/mpb10km_grid.nc", varname = "etopo1")
projection(mpb10km.pts.r) <- crs
rasterized <- function(shp, var, fun){
	rasterize(shp, mpb10km.pts.r, var, fun=fun, na.rm=TRUE) 
}
