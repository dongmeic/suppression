#############################################  Correlation  ####################################################
# on jurisdictional levels
df <- read.csv("/Volumes/dongmeic/beetle/data/text/jurisditional_scaling_point_df.csv")
df.eco <- df[df$Jurisd == "Ecoregions",]
df.eco <- df.eco[2:8]
df.subeco <- df[df$Jurisd == "Sub-ecoregions",]
df.subeco <- df.subeco[2:8]
df.nafor <- df[df$Jurisd == "National Forests",]
df.nafor<- df.nafor[2:8]
df.lanown <- df[df$Jurisd == "Land ownerships",]
df.lanown <- df.lanown[2:8]
head(df.eco)
plot(df.eco)

library("PerformanceAnalytics")
chart.Correlation(df.eco, histogram=TRUE, pch=19)

M1 <- cor(df.nafor)
M2 <- cor(df.eco)
M3 <- cor(df.subeco)
M4 <- cor(df.lanown)

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

res1 <- cor.mtest(df.nafor,0.95)
res2 <- cor.mtest(df.eco,0.95)
res3 <- cor.mtest(df.subeco,0.95)
res4 <- cor.mtest(df.lanown,0.95)

png(paste0(outpath, "corr_plot.png"), width=12, height=12, units="in", res=300)
par(mfrow=c(2,2),mar=c(2,2,2,2))
corrplot.mixed(M1, p.mat = res1[[1]], sig.level=0.05)
title(line=0, main="Correlation by national forests")
corrplot.mixed(M2, p.mat = res2[[1]], sig.level=0.05)
title(line=0, main="Correlation by ecoregions")
corrplot.mixed(M3, p.mat = res3[[1]], sig.level=0.05)
title(line=0, main="Correlation by sub-ecoregions")
corrplot.mixed(M4, p.mat = res4[[1]], sig.level=0.05)
title(line=0, main="Correlation by landownerships")
dev.off()

################################  Geographically Weighted Regression  #########################################
library(maptools)
crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
west.usa.shp <- readShapePoly("/Volumes/dongmeic/beetle/data/vector/western_us.shp")
proj4string(west.usa.shp) <- crs
# read fire density data
library(rgdal)
sprs.fire <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/fh_all/", layer = "fire_history_west_us_firetype1")
proj4string(sprs.fire) <- crs
sprs.fire$no_fire <- rep(1,1,length(sprs.fire$FIREI))
# read beetle data
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
quick.map <- function(spdf,var,legend.title,main.title,color) {
  plotvar <- spdf@data[,var]
  nclr <- 5
  plotclr <- brewer.pal(nclr,color)
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0,0,2,2))
  plot(west.usa.shp, bord="grey")
  plot(spdf, col=colcode, add=T)
  title(main.title,cex.main=2)
  legend(180000,2800000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), title=legend.title, title.adj =0.2,bty="n")
  add.northarrow()
  add.scale()
}
quick.map.r <- function(spdf,var,legend.title,main.title,color) {
  plotvar <- spdf@data[,var]
  nclr <- 4
  plotclr <- brewer.pal(nclr,color)
  class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks = c(-1, -0.5, 0, 0.5, 1), dataPrecision=1)
  colcode <- findColours(class, plotclr)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0,0,2,2))
  plot(west.usa.shp, bord="grey")
  plot(spdf, col=colcode, add=T)
  title(main.title,cex.main=2)
  legend(180000,2800000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), title=legend.title, title.adj =0.2,bty="n")
  add.northarrow()
  add.scale()
}
library(RColorBrewer)
library(classInt)
quick.map(ecoreg.m,"Costs","Containment costs (Dollars)", "Fire suppression costs by ecoregions","Reds")

library(GWmodel)
ecoreg.m$LogMPB <- log(ecoreg.m$MPB_ACRES)
ecoreg.m$LogCost <- log(ecoreg.m$Costs)
ecoreg.m.mpb <- ecoreg.m[!is.na(ecoreg.m$LogMPB),]
ecoreg.m.mpb <- ecoreg.m.mpb[ecoreg.m.mpb$LogCost != -Inf,]
localstats <- gwss(ecoreg.m.mpb, vars = c("LogMPB", "LogCost"), bw=100000)
head(data.frame(localstats$SDF))
quick.map(localstats$SDF,"LogMPB_LM","LogMPB", "LogMPB by ecoregions","Reds")
quick.map(localstats$SDF,"LogCost_LM","LogCost", "LogCost by ecoregions","Reds")
gwr.res <- gwr.basic(LogMPB~LogCost, data = ecoreg.m.mpb, bw=100000, kernel = 'gaussian')
head(gwr.res$SDF)
quick.map.r(gwr.res$SDF,"LogCost","LogCost Coefficient", "GWR by ecoregions","Reds")
gwr.res <- gwr.basic(LogMPB~Duration, data = ecoreg.m.mpb, bw=100000, kernel = 'gaussian')
head(gwr.res$SDF)
quick.map.r(gwr.res$SDF,"Duration","Duration Coefficient", "GWR by ecoregions","Reds")

################################  Spatial autocorrelation Moran's I  #########################################
require(spdep)
ecoreg.m.mpb.nb <- poly2nb(ecoreg.m.mpb)
ecoreg.m.mpb.nb
plot(ecoreg.m.mpb, border='lightgrey')
plot(ecoreg.m.mpb.nb, coordinates(ecoreg.m.mpb), add=TRUE, col='red')

ecoreg.m.mpb.nb2 <- poly2nb(ecoreg.m.mpb, queen = FALSE)
plot(ecoreg.m.mpb, border='lightgrey')
plot(ecoreg.m.mpb.nb, coordinates(ecoreg.m.mpb), add=TRUE, col='blue', lwd=2)
plot(ecoreg.m.mpb.nb2, coordinates(ecoreg.m.mpb), add=TRUE, col='yellow')

ecoreg.m.mpb.lw <- nb2listw(ecoreg.m.mpb.nb2)
ecoreg.m.mpb.lw
ecoreg.mpb.lagged.means <- lag.listw(ecoreg.m.mpb.lw, ecoreg.m.mpb$MPB_ACRES, NAOK = TRUE)
library(GISTools)
shades <- auto.shading(ecoreg.m.mpb$MPB_ACRES, n=6, cols = brewer.pal(6,'Reds'))
choropleth(ecoreg.m.mpb, ecoreg.mpb.lagged.means, shades)
choro.legend(180000,2800000,shades, title="Lag means of MPB acres", cex=0.8)

par(xpd=FALSE,mfrow=c(1,1),mar=c(2,2,2,2))
plot(ecoreg.m.mpb$MPB_ACRES,ecoreg.mpb.lagged.means,asp=1,xlim=range(ecoreg.m.mpb$MPB_ACRES),ylim=range(ecoreg.m.mpb$MPB_ACRES), main="Lagged mean plot for MPB acres in Oregon", xlab="Mean MPB acres", ylab="Lagged means of MPB acres")
# a, b the intercept and slope, single values.
abline(a=0,b=1)
abline(v=mean(ecoreg.m.mpb$MPB_ACRES),lty=2)
abline(h=mean(ecoreg.mpb.lagged.means),lty=2)

moran.plot(ecoreg.m.mpb$MPB_ACRES,ecoreg.m.mpb.lw, xlab="MPB acres", ylab="spatially lagged income")
moran.test(ecoreg.m.mpb$MPB_ACRES,ecoreg.m.mpb.lw, na.action = na.omit)
mpb.lI <- localmoran(ecoreg.m.mpb$MPB_ACRES,ecoreg.m.mpb.lw,zero.policy=TRUE,na.action = na.exclude)
mpb.shade <- auto.shading(c(mpb.lI[,1],-mpb.lI[,1]),cols=brewer.pal(5,"RdBu"))
choropleth(ecoreg.m.mpb,mpb.lI[,1],shading=mpb.shade)
choro.legend(180000,2800000,mpb.shade,fmt="%6.2f")
################################  Wilderness - WUI proximity analysis  #########################################
wdn.file <- "/Volumes/dongmeic/beetle/data/vector/spatial_join/wilderness_proj.shp"
wdn <- readShapePoly(wdn.file)
proj4string(wdn) <- crs
mpb.pts$wdn <- over(mpb.pts, wdn)$OBJECTID_1
mpb.in.wdn <- aggregate(ACRES ~ wdn, data=mpb.pts, sum, na.rm=TRUE)
names(wdn)[which(names(wdn) == "OBJECTID_1")] <- "wdn"
wdn.m <- merge(wdn, mpb.in.wdn, by="wdn", all=TRUE)
head(wdn.m)
wdn.m.df <- as.data.frame(wdn.m)
wdn.m.df.1 <- wdn.m.df[!is.na(wdn.m.df$ACRES),][c("Distance","YearOfDesi","ACRES")]
wdn.m.df.1$Years <- 2014 - as.numeric(as.character(wdn.m.df.1$YearOfDesi))
cor.test(wdn.m.df.1$Distance, wdn.m.df.1$ACRES)
cor.test(wdn.m.df.1$Years, wdn.m.df.1$ACRES)
plot(wdn.m.df.1$Distance, wdn.m.df.1$ACRES)
################################  Slope  #######################################################################
library(raster)
slope <- raster("/Volumes/dongmeic/beetle/data/raster/west_us_slope_0.tif")
#hist(slope)
#slope.values <- getValues(slope)
# r <- raster(res=res(slope)*10, ext=extent(slope), crs=crs)
# r <- resample(slope, r, method='bilinear')
mpb_slope <- readShapePoints("/Volumes/dongmeic/beetle/data/vector/extract_value_to_points.shp")
mpb_slope <- mpb_slope[mpb_slope$RASTERVALU != -9999,]
range(mpb_slope$RASTERVALU)
mean(mpb_slope$RASTERVALU); median(mpb_slope$RASTERVALU)
cor.test(mpb_slope$RASTERVALU, mpb_slope$ACRES) #-0.0005623664; p-value = 0.5543

