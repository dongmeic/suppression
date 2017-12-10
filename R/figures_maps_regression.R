# load libraries
library(maptools)
library(classInt)
library(GISTools)
library(ncdf4)
library(rasterVis)
library(sp)
library(grid)
library(lattice)
library(grid)
library(abind)
library(raster)
library(rgdal)
library(RColorBrewer)
library(latticeExtra)
library(gridExtra)
library(BAMMtools)
library(gvlma)
library(corrplot)
library(PerformanceAnalytics)

outpath <- "/Volumes/dongmeic/beetle/output/fire_suppression/"
# fire, suppression, MPB (Figure 1)
fire.mpb <- read.csv("/Volumes/dongmeic/beetle/data/text/fire_mpb.csv")
fire.mpb <- subset(fire.mpb, Year >= 1997)
png(paste(outpath, "fire_cost_beetle.png", sep=""), width=12, height=9, units="in", res=300)
par(mfrow=c(4,1),xpd=FALSE,mar=c(3,3,3,0))
barplot(fire.mpb$Acres/fire.mpb$Fires,main = "Mean fire size (Acres)", axes = F, cex.main=2)
axis(2, cex.axis=1.5)
barplot(fire.mpb$Acres/1000000,main = "Area burned (Millions of Acres)", axes = F, cex.main=2)
axis(2, cex.axis=1.5)
barplot(fire.mpb$Costs/1000000000,main = "Fire suppression cost (Billions of Dollars)",  axes = F, cex.main=2)
axis(2, cex.axis=1.5)
barplot(fire.mpb$MPB/1000000, names.arg = fire.mpb$Year, main = "Mountain pine beetle affected area (Millions of Acres)", cex.names = 1.6, cex.lab=1.6, cex.axis=1.6, cex.main=2.2)
dev.off()

####################################### fire-insect co-occurrence ##################################################################################################################

# fire-insect co-occurrence (Figure 2)
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)
ncfolder <- "/Volumes/dongmeic/beetle/data/raster/wus10km_v1/"
ncfile <- "wus10km_v1_short_01.nc"
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)
btlfir_prs_r <- brick(paste(ncfolder, ncfile, sep=""), varname="btlfir_prs", crs = na10km_crs, lvar=4)
wus_state_lines <- readShapeLines("/Volumes/dongmeic/beetle/data/vector/wus10km_v1/wus10km_us_state.shp", proj4string = na10km_crs)

## test: multiple maps
# myColors <- c('white', 'blue', 'green', 'red')
# myKey <- list(text=list(lab=c("fire-beetle free", "beetle only", "co-occurrence", "fire only"), cex=c(1.5,1.5,1.5,1.5)),
#               rectangles=list(col = myColors), space="top", width = 0.5, columns=4)
# levelplot(btlfir_prs_r, par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F,
#                 col.regions=myColors,colorkey = FALSE, key=myKey)+layer(sp.lines(wus_state_lines, lwd=0.8, col='dimgray'))

add.northarrow <- function(){
  #GISTools::north.arrow(xb=-680000, yb=-2260000, len=40000)
  GISTools::north.arrow(xb=-1380000, yb=-2300000, len=40000)
}
add.scale <- function(){
  #GISTools::map.scale(-680000, -2460000, 400000,"100km",2,2,sfcol='brown')
  GISTools::map.scale(-880000, -2260000, 400000,"100km",2,2,sfcol='brown')
} 
years <- 1997:2013
#outpath <- "/Users/dongmeichen/Documents/beetle/"
myPath <- paste(outpath,"fire_beetle_overlay.png", sep="")
arg <- list(at=c(0,1,2,3), labels=c("Fire-beetle free", "Beetle only", "Co-occurrence", "Fire only"))
png(myPath, width = 9, height = 5, units = "in", res=300)
par(mfrow=c(1,2),xpd=TRUE,mar=c(0,0,2,0))
plot(subset(btlfir_prs_r, 1), col=c("white","blue","green","red"), main="Beetle-fire co-occurrence (1997)", xaxt = "n", yaxt = "n", bty="n", box=FALSE, legend=FALSE)
plot(subset(btlfir_prs_r, 1), col=c("white","blue","green","red"), axis.args=arg, legend.only=TRUE, legend.width=0.7, legend.shrink=0.8, legend.args=list(text="", side=3, font=2, line=2,cex=1), smallplot=c(.2,.25,.08,.2)); par(mar = par("mar"))
plot(wus_state_lines, lwd=0.8, col='dimgray', add=TRUE)
plot(subset(btlfir_prs_r, 13), col=c("white","blue","green","red"), main="Beetle-fire co-occurrence (2009)", xaxt = "n", yaxt = "n", bty="n", box=FALSE, legend=FALSE)
plot(wus_state_lines, lwd=0.8, col='dimgray', add=TRUE)
# par(mfrow=c(1,1),xpd=TRUE,mar=c(0,0,0,0))
# plot(subset(btlfir_prs_r, 1), col=c("white", "blue","green","red"), xaxt = "n", yaxt = "n", bty="n", box=FALSE, legend=FALSE)
# alp <- seq(.98, 0.28, by=-0.045)
# for (i in 2:length(years)){
#   par(mfrow=c(1,1),xpd=TRUE,mar=c(0,0,0,0), new=TRUE)
#   blue <- rgb(0,0,1, alpha=alp[i-1]); green <- rgb(0,1,0, alpha=alp[i-1]); red <- rgb(1,0,0, alpha=alp[i-1])
#   plot(subset(btlfir_prs_r, i), col=c("white", blue, green, red), xaxt = "n", yaxt = "n", bty="n", box=FALSE, legend=FALSE)
# }
# plot(subset(btlfir_prs_r, i), col=c("white","blue","green","red"), axis.args=arg, legend.only=TRUE, legend.width=0.7, legend.shrink=0.8, legend.args=list(text='Beetle-fire overlay\n(1997-2013)', side=3, font=2, line=2,cex=1), smallplot=c(.2,.25,.08,.2)); par(mar = par("mar"))
# plot(wus_state_lines, lwd=0.8, col='dimgray', add=TRUE)
add.northarrow()
add.scale()
dev.off()

####################################### maps of jurisdictional boundaries ##################################################################################################################
lonlat <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
west.usa.shp <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/western_us.shp")
proj4string(west.usa.shp) <- crs

parks <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/jurisditional/AdministrativeForest.shp")
proj4string(parks) <- lonlat 
parks <- spTransform(parks, crs)
ecoregions <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/ecoregions/ecosections.shp")
proj4string(ecoregions) <- crs
subecoregions <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/ecoregions/subsections.shp")
proj4string(subecoregions) <- crs
landown <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/jurisditional/land_ownerships_wus.shp")
proj4string(landown) <- lonlat 
landown <- spTransform(landown, crs)

png(paste0(outpath,"jurisd_bound.png"), width = 10, height = 12, units = "in", res = 300)
par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(ecoregions, bord="red")
plot(west.usa.shp, add=TRUE, bord="grey")
title(line=0, main="Ecoregions", cex.main=2)
plot(landown, bord="red")
plot(west.usa.shp, add=TRUE, bord="grey")
title(line=0, main="Land ownerships", cex.main=2)
plot(parks, bord="red")
plot(west.usa.shp, add=TRUE, bord="grey")
title(line=0, main="National forests", cex.main=2)
plot(subecoregions, bord="red")
plot(west.usa.shp, add=TRUE, bord="grey")
title(line=0, main="Sub-ecoregions", cex.main=2)
dev.off()

######################################## maps of fire suppression and beetle outbreaks on jurisdictional levels ########################################################################################################################

# the spatial pattern of fire suppression and beetle outbreaks (Figure 4)
northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
  # checking arguments
  if(missing(loc)) stop("loc is missing")
  if(missing(size)) stop("size is missing")
  # default colors are white and black
  if(missing(cols)) cols <- rep(c("white","black"),8)
  # calculating coordinates of polygons
  radii <- rep(size/c(1,4,2,4),4)
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  # drawing polygons
  for (i in 1:15) {
    x1 <- c(x[i],x[i+1],loc[1])
    y1 <- c(y[i],y[i+1],loc[2])
    polygon(x1,y1,col=cols[i])
  }
  # drawing the last polygon
  polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
  # drawing letters
  b <- c("E","N","W","S")
  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
                      (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
                      cex=2)
}

add.northarrow <- function(){
  northarrow(c(-1800000,700000),120000)
}

add.scale <- function(){
  lines(c(-1100000,-1100000,-900000,-900000),c(550000,585000,585000,550000), lwd=3)
  text(-1000000,462000, "200 km", cex = 2)
}

quick.map <- function(spdf,var,legend.title,main.title,color,outname) {
  plotvar <- spdf@data[,var]
  nclr <- 5
  plotclr <- brewer.pal(nclr,color)
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  png(paste0(outpath,outname,".png"), width=12, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0,0,2,2))
  plot(west.usa.shp, bord="grey")
  plot(spdf, col=colcode, add=T)
  title(main.title,cex.main=2)
  legend(180000,2800000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), title=legend.title, title.adj =0.2,bty="n")
  add.northarrow()
  add.scale()
  dev.off()
}

# fire density
sprs.fire <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/fh_all/", layer = "fire_history_west_us_firetype1")
proj4string(sprs.fire) <- crs
sprs.fire$no_fire <- rep(1,1,length(sprs.fire$FIREI))
# beetle data
mpb.path <- "/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/mpb/"
mpb.file <- "us_mpb_points.shp"
mpb.pts <- readShapeSpatial(paste(mpb.path, mpb.file, sep=""))
proj4string(mpb.pts) <- crs
# fire data with distance to WUI
fires <- readShapePoints("/Volumes/dongmeic/beetle/data/vector/spatial_join/fpa_sit_wui.shp")
proj4string(fires) <- crs

# ecoregions
ecoregions <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/ecoregions/ecosections.shp")
proj4string(ecoregions) <- crs
fires$ecoreg <- over(fires, ecoregions)$MAP_UNIT_S
burnedArea.in.ecoreg <- aggregate(Acres ~ ecoreg, data=fires, sum, na.rm=TRUE)
fireDuration.in.ecoreg <- aggregate(Duration ~ ecoreg, data = fires, mean, na.rm=TRUE)
costEst.in.ecoreg <- aggregate(Costs ~ ecoreg, data = fires, sum, na.rm=TRUE)
fireSize.in.ecoreg <- aggregate(FIRE_SIZE ~ ecoreg, data = fires, mean, na.rm=TRUE)
dist.in.ecoreg <- aggregate(Distance ~ ecoreg, data=fires, mean, na.rm=TRUE)
sprs.fire$ecoreg <- over(sprs.fire, ecoregions)$MAP_UNIT_S
fdens.in.ecoreg <- aggregate(no_fire ~ ecoreg, data=sprs.fire, sum)
mpb.pts$ecoreg <- over(mpb.pts, ecoregions)$MAP_UNIT_S
mpb.in.ecoreg <- aggregate(ACRES ~ ecoreg, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.ecoreg)[which(names(mpb.in.ecoreg) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.ecoreg, fireDuration.in.ecoreg, by= "ecoreg", all=TRUE)
df.2 <- merge(costEst.in.ecoreg, fdens.in.ecoreg, by= "ecoreg", all=TRUE)
df.3 <- merge(fireSize.in.ecoreg, mpb.in.ecoreg, by= "ecoreg", all=TRUE)
df <- merge(df.1, df.2, by= "ecoreg", all=TRUE)
df <- merge(df, df.3, by= "ecoreg", all=TRUE)
df <- merge(df, dist.in.ecoreg, by= "ecoreg", all=TRUE)
colnames(df)[which(names(df) == "ecoreg")] <- "MAP_UNIT_S"
ecoreg.m <- merge(ecoregions, df, by="MAP_UNIT_S", all=TRUE)

quick.map(ecoreg.m,"Costs","Containment costs (Dollars)", "Fire suppression costs by ecoregions","Reds", "costs_eco")
quick.map(ecoreg.m,"Acres","Area burned (Acres)", "Area burned by ecoregions","Reds", "burnedArea_eco")
quick.map(ecoreg.m,"MPB_ACRES","MPB affected area (Acres)", "Beetle affected area by ecoregions","Reds", "MPBoutbreak_eco")
quick.map(ecoreg.m,"Duration","Containment duration (Days)", "Fire duration by ecoregions","Reds", "duration_eco")
quick.map(ecoreg.m,"no_fire","Suppression density (Points)", "Fire density by ecoregions","Reds", "density_eco")
quick.map(ecoreg.m,"FIRE_SIZE","Fire size (Acres)", "Fire size by ecoregions","Reds", "size_eco")

png(paste0(outpath,"ecoregions.png"), width=24, height=18, units="in", res=300)
par(xpd=FALSE,mfrow=c(2,3),mar=c(0,0,4,2))
plotvar <- ecoreg.m@data[,"MPB_ACRES"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("MPB affected area",cex.main=5)
legend(-2300000,1200000, legend=c("Low", " ", " ", " ","High"), cex=3,
       fill=attr(colcode, "palette"), bty="n")
# add.northarrow()
# add.scale()

plotvar <- ecoreg.m@data[,"Costs"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("Containment costs",cex.main=5)
# legend(180000,2800000, legend=names(attr(colcode, "table")),
#        fill=attr(colcode, "palette"), title="Fire suppression costs ($)", title.adj =0.2,bty="n")
# add.northarrow()
# add.scale()

plotvar <- ecoreg.m@data[,"Duration"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("Containment duration",cex.main=5)
# legend(180000,2800000, legend=names(attr(colcode, "table")),
#        fill=attr(colcode, "palette"), title="Containment duration (day)", title.adj =0.2,bty="n")
# add.northarrow()
# add.scale()

plotvar <- ecoreg.m@data[,"Acres"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("Area burned",cex.main=5)
# legend(180000,2800000, legend=names(attr(colcode, "table")),
#        fill=attr(colcode, "palette"), title="Area burned (acre)", title.adj =0.2,bty="n")
# add.northarrow()
#add.scale()

plotvar <- ecoreg.m@data[,"FIRE_SIZE"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("Mean fire size",cex.main=5)
# legend(180000,2800000, legend=names(attr(colcode, "table")),
#        fill=attr(colcode, "palette"), title="Fire size", title.adj =0.2,bty="n")
# add.northarrow()
# add.scale()

plotvar <- ecoreg.m@data[,"no_fire"]
nclr <- 5
plotclr <- brewer.pal(nclr,"Reds")
class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
colcode <- findColours(class, plotclr)
plot(ecoreg.m, col=colcode, bord='dimgray')
title("Suppression density",cex.main=5)
add.northarrow()
add.scale()
dev.off()

########################################### maps of fire suppression and beetle outbreaks in raster #########################################################################################################################################################

# Maps in a raster plot (Figure 4) # WGS84
host.shp <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/vegetation", layer="MPB_host_proj_westUS_clip_Disall")
pts <- over(fires, as(west.usa.shp,"SpatialPolygons"))
#pts <- over(fires, as(host.shp,"SpatialPolygons"))
fpa.sit.wus.spdf <- fires[!is.na(pts),]

mpb.pts$LogAcre <- log(mpb.pts$ACRES)

names <- c("MPB affected area (Millions of Acres)", "Containment costs (Billions of Dollars)", "Containment duration (Days)", "Area burned (Millions of Acres)", "Mean fire size (Thousands of Acres)", "Suppression density (Thousands of Points)")
cell.size <- c(10000,25000,50000,100000,200000)
cellsize.lab <- c("10km", "25km", "50km", "100km", "200km")
i <- 4
# # roi == "host"
# xmin <- -2354936; xmax <- 237189.6; ymin <- 684244; ymax <- 3165384
# ncols <- (xmax - xmin)/cell.size[i]; nrows <- (ymax - ymin)/cell.size[i]
# r <- raster(nrows=nrows, ncols=ncols, ext=extent(host.shp),crs = crs)

# roi == "westUS"
xmin <- -2347426; xmax <- -246127.2; ymin <- 311822.4; ymax <- 3165592
ncols <- (xmax - xmin)/cell.size[i]; nrows <- (ymax - ymin)/cell.size[i]
r <- raster(nrows=nrows, ncols=ncols, ext=extent(west.usa.shp), crs = crs)

fire.dens <- rasterize(sprs.fire, r, "FIREI", fun='count', na.rm=TRUE)
fire.acre <- rasterize(fpa.sit.wus.spdf, r, "Acres", fun=sum, na.rm=TRUE)
fire.size <- rasterize(fpa.sit.wus.spdf, r, "FIRE_SIZE", fun=mean, na.rm=TRUE)
cost <- rasterize(fpa.sit.wus.spdf, r, "Costs", fun=sum, na.rm=TRUE)
duration <- rasterize(fpa.sit.wus.spdf, r, "Duration", fun=mean, na.rm=TRUE)
btl.acre <- rasterize(mpb.pts, r, "ACRES", fun=sum, na.rm=TRUE)
# plot rasters
require(sp)
color <- "Reds"
mapTheme <- rasterTheme(region=brewer.pal(5,color), cex=3)
# roi == "westUS"
# at=getJenksBreaks(na.omit(getValues(cost/1000000000)), 6),
p2 <- levelplot(cost/1000000000, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[2])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
# at=getJenksBreaks(na.omit(getValues(fire.size/1000)), 6),
p5 <- levelplot(fire.size/1000, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[5])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
# at=getJenksBreaks(na.omit(getValues(fire.acre/1000000)), 6),
p4 <- levelplot(fire.acre/1000000, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[4])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
# at=getJenksBreaks(na.omit(getValues(duration)), 6),
p3 <- levelplot(duration, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[3])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
# at=getJenksBreaks(na.omit(getValues(fire.dens/1000)), 6),
p6 <- levelplot(fire.dens/1000, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[6])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
# at=getJenksBreaks(na.omit(getValues(btl.acre/1000000)), 6),
p1 <- levelplot(btl.acre/1000000, scales = list(draw = FALSE), margin=F, par.settings=mapTheme, main=names[1])+latticeExtra::layer(sp.polygons(west.usa.shp, lwd=0.8, col='dimgray'))#+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='lightgreen'))
png(paste0(outpath, "raster_map_", cellsize.lab[i], ".png", sep=""), width=16, height=10, units="in", res=300)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
dev.off()
# #roi == "host"
# p1 <- levelplot(cost, margin=F, par.settings=mapTheme, main=names[1])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# p2 <- levelplot(fire.size, margin=F, par.settings=mapTheme, main=names[2])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# p3 <- levelplot(fire.acre, margin=F, par.settings=mapTheme, main=names[3])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# p4 <- levelplot(duration, margin=F, par.settings=mapTheme, main=names[4])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# p5 <- levelplot(fire.dens, margin=F, par.settings=mapTheme, main=names[5])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# p6 <- levelplot(btl.acre, margin=F, par.settings=mapTheme, main=names[6])+latticeExtra::layer(sp.polygons(host.shp, lwd=0.5, col='green'))
# grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)
# dev.off()

############################################## Regression ######################################################################################################################################################

## regression
# national forests
parks <- readShapePoly("/Volumes/dongmeic /beetle/data/vector/jurisditional/AdministrativeForest.shp")
proj4string(parks) <- lonlat
parks <- spTransform(parks, crs)
fires$park <- over(fires, parks)$FORESTORGC
burnedArea.in.parks <- aggregate(Acres ~ park, data=fires, sum, na.rm=TRUE)
fireDuration.in.parks <- aggregate(Duration ~ park, data = fires, mean, na.rm=TRUE)
costEst.in.parks <- aggregate(Costs ~ park, data = fires, sum, na.rm=TRUE)
fireSize.in.parks <- aggregate(FIRE_SIZE ~ park, data = fires, mean, na.rm=TRUE)
sprs.fire$park <- over(sprs.fire, parks)$FORESTORGC
sprs.fire$no_fire <- rep(1,1,length(sprs.fire$FIREI))
fdens.in.park <- aggregate(no_fire ~ park, data=sprs.fire, sum)
mpb.pts$park <- over(mpb.pts, parks)$FORESTORGC
mpb.in.park <- aggregate(ACRES ~ park, data=mpb.pts, sum, na.rm=TRUE)
#mpb.in.park <- aggregate(ORIG_FID ~ park, data=mpb.pts, function(x) length(x))
dist.in.parks <- aggregate(Distance ~ park, data=fires, mean, na.rm=TRUE)

df.1 <- merge(burnedArea.in.parks, fireDuration.in.parks, by= "park", all=TRUE)
df.2 <- merge(costEst.in.parks, fdens.in.park, by= "park", all=TRUE)
df.3 <- merge(fireSize.in.parks, mpb.in.park, by= "park", all=TRUE)
df <- merge(df.1, df.2, by= "park", all=TRUE)
df <- merge(df, df.3, by= "park", all=TRUE)
df <- merge(df, dist.in.parks, by= "park", all=TRUE)
colnames(df)[which(names(df) == "park")] <- "FORESTORGC"
#colnames(df)[which(names(df) == "ORIG_FID")] <- "MPBpts"
parks.m <- merge(parks, df, by="FORESTORGC", all=TRUE)

quick.map(parks.m,"Costs","Fire suppression costs ($)", "Fire suppression costs by national forests","Reds", "costs_np")
quick.map(parks.m,"Acres","Burned area (acre)", "Burned area by national forests","Reds", "burnedArea_np")
quick.map(parks.m,"ACRES","MPB affected area (acre)", "Beetle affected area by national forests","Reds", "MPBoutbreak_np")
quick.map(parks.m,"Duration","Fire duration (day)", "Fire duration by national forests","Reds", "duration_np")
quick.map(parks.m,"no_fire","Fire density", "Fire density by national forests","Reds", "density_np")
quick.map(parks.m,"FIRE_SIZE","Fire size", "Fire size by national forests","Reds", "size_np")

# regression
parks.m.df <- as.data.frame(parks.m)
varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","ACRES")
#varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPBpts")
parks.df <- parks.m.df[varlist]
parks.df$Acres <- log(parks.df$Acres)
parks.df$Costs <- log(parks.df$Costs)
parks.df$ACRES <- log(parks.df$ACRES)
#parks.df$MPBpts <- log(parks.df$MPBpts)
parks.df$FIRE_SIZE <- log(parks.df$FIRE_SIZE)
parks.df$no_fire <- log(parks.df$no_fire)
parks.df$Distance <- log(parks.df$Distance)
parks.df <- parks.df[parks.df$Acres != -Inf & 
                       parks.df$Costs != -Inf & 
                       parks.df$Distance != -Inf,]
parks.df <- na.omit(parks.df)
mlr <- lm(ACRES ~ ., data=parks.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(ACRES ~ Costs, data=parks.df)
summary(lr)
cor.test(parks.df$ACRES, parks.df$no_fire)

# ecoregion
ecoreg.m.df <- as.data.frame(ecoreg.m)
varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPB_ACRES")
#varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPBpts")
ecoreg.m.df <- ecoreg.m.df[varlist]
ecoreg.m.df$Acres <- log(ecoreg.m.df$Acres)
ecoreg.m.df$Costs <- log(ecoreg.m.df$Costs)
ecoreg.m.df$MPB_ACRES <- log(ecoreg.m.df$MPB_ACRES)
#ecoreg.m.df$MPBpts <- log(ecoreg.m.df$MPBpts)
ecoreg.m.df$FIRE_SIZE <- log(ecoreg.m.df$FIRE_SIZE)
ecoreg.m.df$no_fire <- log(ecoreg.m.df$no_fire)
ecoreg.m.df$Distance <- log(ecoreg.m.df$Distance)
ecoreg.m.df <- ecoreg.m.df[ecoreg.m.df$Acres != -Inf & 
                             ecoreg.m.df$Costs != -Inf,]
ecoreg.m.df <- na.omit(ecoreg.m.df)
mlr <- lm(MPB_ACRES ~., data=ecoreg.m.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=ecoreg.m.df)
summary(lr)
cor.test(ecoreg.m.df$MPB_ACRES, ecoreg.m.df$no_fire)

subecoregions <- readShapePoly("/Volumes/dongmeic /beetle/data/vector/ecoregions/subsections.shp")
proj4string(subecoregions) <- crs
fires$subeco <- over(fires, subecoregions)$MAP_UNIT_S
burnedArea.in.subeco <- aggregate(Acres ~ subeco, data=fires, sum, na.rm=TRUE)
fireDuration.in.subeco <- aggregate(Duration ~ subeco, data = fires, mean, na.rm=TRUE)
costEst.in.subeco <- aggregate(Costs ~ subeco, data = fires, sum, na.rm=TRUE)
fireSize.in.subeco <- aggregate(FIRE_SIZE ~ subeco, data = fires, mean, na.rm=TRUE)
sprs.fire$subeco <- over(sprs.fire, subecoregions)$MAP_UNIT_S
fdens.in.subeco <- aggregate(no_fire ~ subeco, data=sprs.fire, sum)
mpb.pts$subeco <- over(mpb.pts, subecoregions)$MAP_UNIT_S
mpb.in.subeco <- aggregate(ACRES ~ subeco, data=mpb.pts, sum, na.rm=TRUE)
#mpb.in.subeco <- aggregate(ORIG_FID ~ subeco, data=mpb.pts, function(x) length(x))
colnames(mpb.in.subeco)[which(names(mpb.in.subeco) == "ACRES")] <- "MPB_ACRES"
#colnames(mpb.in.subeco)[which(names(mpb.in.subeco) == "ORIG_FID")] <- "MPBpts"
dist.in.subeco <- aggregate(Distance ~ subeco, data=fires, mean, na.rm=TRUE)

df.1 <- merge(burnedArea.in.subeco, fireDuration.in.subeco, by= "subeco", all=TRUE)
df.2 <- merge(costEst.in.subeco, fdens.in.subeco, by= "subeco", all=TRUE)
df.3 <- merge(fireSize.in.subeco, mpb.in.subeco, by= "subeco", all=TRUE)
df <- merge(df.1, df.2, by= "subeco", all=TRUE)
df <- merge(df, df.3, by= "subeco", all=TRUE)
df <- merge(df, dist.in.subeco, by= "subeco", all=TRUE)
colnames(df)[which(names(df) == "subeco")] <- "MAP_UNIT_S"
subeco.m <- merge(subecoregions, df, by="MAP_UNIT_S", all=TRUE)

quick.map(subeco.m,"Costs","Containment costs ($)", "Containment costs by sub-ecoregions","Reds", "costs_subeco")
quick.map(subeco.m,"Acres","Burned area (acre)", "Burned area by sub-ecoregions","Reds", "burnedArea_subeco")
quick.map(subeco.m,"MPB_ACRES","MPB affected area (acre)", "Beetle affected area by sub-ecoregions","Reds", "MPBoutbreak_subeco")
quick.map(subeco.m,"Duration","Containment duration (day)", "Containment duration by sub-ecoregions","Reds", "duration_subeco")
quick.map(subeco.m,"no_fire","Fire density", "Fire density by sub-ecoregions","Reds", "density_subeco")
quick.map(subeco.m,"FIRE_SIZE","Fire size", "Fire size by sub-ecoregions","Reds", "size_subeco")

# regression
# subecoregion
subeco.m.df <- as.data.frame(subeco.m)
varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPB_ACRES")
#varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPBpts")
subeco.m.df <- subeco.m.df[varlist]
subeco.m.df$Acres <- log(subeco.m.df$Acres)
subeco.m.df$Costs <- log(subeco.m.df$Costs)
subeco.m.df$MPB_ACRES <- log(subeco.m.df$MPB_ACRES)
#subeco.m.df$MPBpts <- log(subeco.m.df$MPBpts)
subeco.m.df$FIRE_SIZE <- log(subeco.m.df$FIRE_SIZE)
subeco.m.df$no_fire <- log(subeco.m.df$no_fire)
subeco.m.df$Distance <- log(subeco.m.df$Distance)
subeco.m.df <- subeco.m.df[subeco.m.df$Acres != -Inf & 
                             subeco.m.df$Costs != -Inf & 
                             subeco.m.df$Distance != -Inf,]
subeco.m.df <- na.omit(subeco.m.df)
mlr <- lm(MPB_ACRES ~., data=subeco.m.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=subeco.m.df)
summary(lr)
cor.test(subeco.m.df$MPB_ACRES, subeco.m.df$no_fire)

landown <- readShapePoly("/Volumes/dongmeic /beetle/data/vector/jurisditional/land_ownerships_wus.shp")
proj4string(landown) <- lonlat
landown <- spTransform(landown, crs)
landown$OBJECTID <- c(1:length(landown$OBJECTID))
fires$landown <- over(fires, landown)$OBJECTID
burnedArea.in.landown <- aggregate(Acres ~ landown, data=fires, sum, na.rm=TRUE)
fireDuration.in.landown <- aggregate(Duration ~ landown, data = fires, mean, na.rm=TRUE)
costEst.in.landown <- aggregate(Costs ~ landown, data = fires, sum, na.rm=TRUE)
fireSize.in.landown <- aggregate(FIRE_SIZE ~ landown, data = fires, mean, na.rm=TRUE)
sprs.fire$landown <- over(sprs.fire, landown)$OBJECTID
fdens.in.landown <- aggregate(no_fire ~ landown, data=sprs.fire, sum)
mpb.pts$landown <- over(mpb.pts, landown)$OBJECTID
mpb.in.landown <- aggregate(ACRES ~ landown, data=mpb.pts, sum, na.rm=TRUE)
#mpb.in.landown <- aggregate(ORIG_FID ~ landown, data=mpb.pts, function(x) length(x))
dist.in.landown <- aggregate(Distance ~ landown, data=fires, mean, na.rm=TRUE)

df.1 <- merge(burnedArea.in.landown, fireDuration.in.landown, by= "landown", all=TRUE)
df.2 <- merge(costEst.in.landown, fdens.in.landown, by= "landown", all=TRUE)
df.3 <- merge(fireSize.in.landown, mpb.in.landown, by= "landown", all=TRUE)
df <- merge(df.1, df.2, by= "landown", all=TRUE)
df <- merge(df, df.3, by= "landown", all=TRUE)
df <- merge(df, dist.in.landown, by= "landown", all=TRUE)
colnames(df)[which(names(df) == "landown")] <- "OBJECTID"
#colnames(df)[which(names(df) == "ORIG_FID")] <- "MPBpts"
landown.m <- merge(landown, df, by="OBJECTID", all=TRUE)

quick.map(landown.m,"Costs","Containment costs ($)", "Containment costs by land ownerships","Reds", "costs_land")
quick.map(landown.m,"Acres","Burned area (Acres)", "Burned area by land ownerships","Reds", "burnedArea_land")
quick.map(landown.m,"MPB_acres","MPB affected area (Acres)", "Beetle affected area by land ownerships","Reds", "MPBoutbreak_land")
quick.map(landown.m,"Duration","Containment duration (Days)", "Containment duration by land ownerships","Reds", "duration_land")
quick.map(landown.m,"no_fire","Fire density (Points)", "Fire density by land ownerships","Reds", "density_land")
quick.map(landown.m,"FIRE_SIZE","Fire size (Acres)", "Fire size by land ownerships","Reds", "size_land")

# regression
# land ownerships
landown.m.df <- as.data.frame(landown.m)
varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","ACRES")
#varlist <- c("Acres","Duration","Costs","no_fire","FIRE_SIZE","Distance","MPBpts")
landown.m.df <- na.omit(landown.m.df[varlist])
landown.m.df$Acres <- log(landown.m.df$Acres)
landown.m.df$Costs <- log(landown.m.df$Costs)
landown.m.df$ACRES <- log(landown.m.df$ACRES)
#landown.m.df$MPBpts <- log(landown.m.df$MPBpts)
landown.m.df$FIRE_SIZE <- log(landown.m.df$FIRE_SIZE)
landown.m.df$no_fire <- log(landown.m.df$no_fire)
landown.m.df$Distance <- log(landown.m.df$Distance)
landown.m.df <- landown.m.df[landown.m.df$Acres != -Inf & 
                               landown.m.df$Costs != -Inf & 
                               landown.m.df$Distance != -Inf,]
mlr <- lm(ACRES ~., data=landown.m.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(ACRES ~ Costs, data=landown.m.df)
summary(lr)
cor.test(landown.m.df$ACRES, landown.m.df$no_fire)

############################################ raster scaling regression models ########################################################################################################################################################

ndf <- read.csv("/Volumes/dongmeic /beetle/data/text/raster_scaling_df.csv")
# MPB points
varlist2 <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist")
ndf1 <- ndf[ndf$CellSize == 10000,]
ndf2 <- ndf[ndf$CellSize == 25000,]
ndf3 <- ndf[ndf$CellSize == 50000,]
ndf4 <- ndf[ndf$CellSize == 100000,]
ndf5 <- ndf[ndf$CellSize == 200000,]
ras.df1 <- ndf1[varlist2]
ras.df2 <- ndf2[varlist2]
ras.df3 <- ndf3[varlist2]
ras.df4 <- ndf4[varlist2]
ras.df5 <- ndf5[varlist2]
mlr <- lm(LogMPB ~ ., data=ras.df1)
summary(mlr)

############################################## Variables by forest ownerships ######################################################################################################################################################

# read join_forestowner data
forestownerships <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/spatial_join/forestownerships_fires.shp")
forestownerships.df <- as.data.frame(forestownerships)

forestowner_density <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/spatial_join/forestownerships_density.shp")
forestowner_density.df <- as.data.frame(forestowner_density)
forestowner_mpb <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/spatial_join/forest_ownerships_mpb.shp")
forestowner_mpb.df <- as.data.frame(forestowner_mpb)

table <- read.table("/Volumes/dongmeic /beetle/data/vector/spatial_join/area.txt", header = T, sep=",")
labels <- c("Federal", "State", "Local", "Family", "Corporate", "Other")
png(paste0(outpath,"forestownerships.png"), width=12, height=8, units="in", res=300)
par(mfrow=c(3,2),xpd=FALSE,mar=c(2,3,3,0))
barplot(forestowner_mpb.df$ACRES/table$Area, names.arg = labels, main = "MPB affected area (%)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestownerships.df$Costs/table$Area, names.arg = labels, main = "Containment costs ($/ac)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestownerships.df$Duration, names.arg = labels, main = "Containment duration (Days)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestownerships.df$Acres/table$Area, names.arg = labels, main = "Area burned (%)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestownerships.df$FIRE_SIZE, names.arg = labels, main = "Maximum fire size (Acres)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner_density.df$Join_Count, names.arg = labels, main = "Suppression density (Points)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
dev.off()

# a quicker way to look at the data
forestowner <- read.csv("/Volumes/dongmeic/beetle/data/text/forestowner_df.csv")
forestowner <- forestowner[,2:10]
table <- read.table("/Volumes/dongmeic/beetle/data/vector/spatial_join/area.txt", header = T, sep=",")
labels <- c("Federal", "State", "Local", "Family", "Corporate", "Other")
png(paste0(outpath,"forestownerships.png"), width=12, height=8, units="in", res=300)
par(mfrow=c(3,2),xpd=FALSE,mar=c(2,3,3,0))
barplot(forestowner$beetle_acres/table$Area, names.arg = labels, main = "MPB affected area (%)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner$costs/table$Area, names.arg = labels, main = "Containment costs ($/ac)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner$duration, names.arg = labels, main = "Containment duration (Days)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner$burned_area/table$Area, names.arg = labels, main = "Area burned (%)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner$fire_size, names.arg = labels, main = "Maximum fire size (Acres)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
barplot(forestowner$fire_density, names.arg = labels, main = "Suppression density (Points)", cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
dev.off()

############################################# correlation #######################################################################################################################################################

# correlation
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
subeco.m.df <- subeco.m.df[c("MPB_ACRES", "Costs", "Duration", "Acres", "FIRE_SIZE", "no_fire", "Distance")]
colnames(subeco.m.df) <- c("MPB", "Cost", "Duration", "Area", "Size", "Density", "Distance")
colnames(subeco.m.df)[7] <- "WUI"
M <- cor(subeco.m.df)
res1 <- cor.mtest(subeco.m.df,0.95)
png(paste0(outpath, "corr_plot_subeco.png"), width=6, height=6, units="in", res=300)
par(mfrow=c(1,1),xpd=FALSE,mar=c(2,2,2,2))
corrplot.mixed(M, p.mat = res1[[1]], sig.level=0.05)
title(line=0, main="Correlation by sub-ecoregions")
dev.off()

# correlation
names(parks.df); names(ecoreg.m.df); names(subeco.m.df); names(landown.m.df)
# parks.df, ecoreg.m.df, subeco.m.df, landown.m.df
parks.df <- parks.df[c("ACRES", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
ecoreg.m.df <- ecoreg.m.df[c("MPB_ACRES", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
landown.m.df <- landown.m.df[c("ACRES", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
# parks.df <- parks.df[c("MPBpts", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
# ecoreg.m.df <- ecoreg.m.df[c("MPBpts", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
# subeco.m.df <- subeco.m.df[c("MPBpts", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
# landown.m.df <- landown.m.df[c("MPBpts", "Costs", "FIRE_SIZE", "Acres", "Duration", "no_fire", "Distance")]
varlist <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist")
colnames(parks.df) <- varlist
colnames(ecoreg.m.df) <- varlist
colnames(subeco.m.df) <- varlist
colnames(landown.m.df) <- varlist
# read dataframe instead
inpath <- "/Volumes/dongmeic/beetle/data/text/"
df.jurisd <- read.csv(paste0(inpath, "jurisditional_scaling_df.csv"))
parks.df <- subset(df.jurisd, Jurisd=="National Forests")[varlist]
ecoreg.m.df <- subset(df.jurisd, Jurisd=="Ecoregions")[varlist]
subeco.m.df <- subset(df.jurisd, Jurisd=="Sub-ecoregions")[varlist]
landown.m.df <- subset(df.jurisd, Jurisd=="Land ownerships")[varlist]
M1 <- cor(parks.df)
M2 <- cor(ecoreg.m.df)
M3 <- cor(subeco.m.df)
M4 <- cor(landown.m.df)
res1 <- cor.mtest(parks.df,0.95)
res2 <- cor.mtest(ecoreg.m.df,0.95)
res3 <- cor.mtest(subeco.m.df,0.95)
res4 <- cor.mtest(landown.m.df,0.95)

png(paste0(outpath, "corr_plot.png"), width=12, height=12, units="in", res=300)
par(mfrow=c(2,2),mar=c(2,2,2,2))
corrplot.mixed(M2, p.mat = res2[[1]], sig.level=0.05)
title(line=0, main="Correlation by ecoregions", cex.main=2)
corrplot.mixed(M4, p.mat = res4[[1]], sig.level=0.05)
title(line=0, main="Correlation by land ownerships", cex.main=2)
corrplot.mixed(M1, p.mat = res1[[1]], sig.level=0.05)
title(line=0, main="Correlation by national forests", cex.main=2)
corrplot.mixed(M3, p.mat = res3[[1]], sig.level=0.05)
title(line=0, main="Correlation by sub-ecoregions", cex.main=2)
dev.off()

############################################# fire event scale #######################################################################################################################################################
data <- read.csv("/Volumes/dongmeic/beetle/data/text/fire_event_scaling_df.csv")
head(data); attach(data)
mlr <- lm(LogMPB ~ LogCost + Duration + LogSize, data=subset(data, Year==-1)) 
library(ggplot2)
ggplot(data, aes(LogCost, LogMPB, colour = YearClass)) + geom_point() + geom_smooth(se = FALSE, method = "lm")
