library(ncdf4)
library(rasterVis)
library(maptools)
library(sp)
library(grid)
library(lattice)
library(grid)
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)
ncfolder <- "/Volumes/dongmeic/beetle/data/raster/wus10km_v1/"
ncfile <- "wus10km_v1_short_01.nc"
outpath <- "/Volumes/dongmeic/beetle/output/maps/"
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)
btlfir_prs_r <- brick(paste(ncfolder, ncfile, sep=""), varname="btlfir_prs", crs = na10km_crs)
wus_state_lines <- readShapeLines("/Volumes/dongmeic/beetle/data/vector/wus10km_v1/wus10km_us_state.shp", proj4string = na10km_crs)

myColors <- c('white', 'blue', 'green', 'red')
myKey <- list(text=list(lab=c("fire-beetle free", "beetle only", "cooccurrence", "fire only"), cex=c(1.5,1.5,1.5,1.5)), 
              rectangles=list(col = myColors), space="top", width = 0.5, columns=4)
years <- 1997:2013
for (i in 1:length(years)){
  #wus_uspr_pts <- uspr.west.spdf[uspr.west.spdf$FiscalYear == years[i],]
  #wus_spr_pts <- rds.sit.west.spdf.proj[rds.sit.west.spdf.proj$FIRE_YEAR == years[i],]
  myPath <- paste(outpath,"fire_beetle_", toString(years[i]),".png", sep="")
  png(myPath, width = 10, height = 8, units = "in", res=300)
  par(mfrow=c(1,1),xpd=TRUE,mar=c(1,0,1,0))
  # sp <- spplot(subset(btlfir_prs_r, i), scales = list(draw = FALSE), margin=F,
  #                   main=list(label=paste("Fire-beetle cooccurrence in ", toString(years[i]), sep=""), cex=2),
  #                   col.regions=myColors, colorkey = FALSE, key=myKey)
  # args <- sp$legend$left$args$key
  # legendArgs <- list(fun = draw.colorkey, args = list(key = args), corner = c(0.05,0.05))
  print(levelplot(subset(btlfir_prs_r, i), par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                  col.regions=myColors, main=list(label=paste("Fire-beetle cooccurrence in ", toString(years[i]), sep=""), cex=2), 
                  colorkey = FALSE, key=myKey)+layer(sp.lines(wus_state_lines, lwd=0.8, col='dimgray'))) #+layer(sp.points(wus_uspr_pts, col="blue", pch=16, cex=0.2))) #+layer(sp.points(wus_spr_pts, col="red", pch=16, cex=0.08)))
  dev.off()
  print(paste(toString(years[i]), "done!"))
}
library(animation)
im.convert(paste(outpath,"fire_beetle_*.png", sep=""), output=paste(outpath,"fire_beetle_yearly.gif", sep=""))