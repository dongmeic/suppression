## Dongmei CHEN
# objectives: raster-based analysis
# reference: 1. /Users/dongmeichen/Documents/scripts/beetle/r/fire_suppression/raster_scaling.R;
#            2. /Users/dongmeichen/GitHub/suppression/figures_maps_regression.R;
#            3. /Users/dongmeichen/GitHub/suppression/regression.R
# input: /Volumes/dongmeic/beetle/data/vector/spatial_join/fpa_sit_dist.shp;

# output: data frames or spatial data frames

# libraries
library(rgdal)
library(raster)
library(corrplot)

# data needed: fpa.sit.join, mpb.pts, fwfod_westus
load("~/Documents/writing/fire suppression/scripts/data_analysis.RData")

cell.size <- c(10000,25000,50000,100000,200000)
cellsize.lab <- c("10km", "25km", "50km", "100km", "200km")
xmin <- -1006739; xmax <- 1050000.0; ymin <- -1722656; ymax <- 539131.6
ndf <- data.frame(MPBAcre = numeric(0), LogMPB = numeric(0), Cost = numeric(0), LogCost = numeric(0), 
                  BurnedArea = numeric(0), logBA = numeric(0), Duration = numeric(0), FireSize = numeric(0),
                  LogSize = numeric(0), FireDens = numeric(0), LogDens = numeric(0), Distance = numeric(0), 
                  LogDist = numeric(0), CellSize = numeric(0))
varlist <- c("MPBAcres", "LogMPB", "Cost", "LogCost", "BurnedArea", "LogBA", "Duration", "FireSize", "LogSize", "FireDens", "LogDens", "Distance", "LogDist")
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist")
for (i in 1:length(cell.size)){
  ncols <- (xmax - xmin)/cell.size[i]; nrows <- (ymax - ymin)/cell.size[i]
  r <- raster(nrows=nrows, ncols=ncols, ext=extent(mpb10km),crs = crs)
  fire.dens <- rasterize(fwfod_westus, r, "FIREID", fun='count', na.rm=TRUE)
  fire.acre <- rasterize(fpa.sit.join, r, "Acres", fun=sum, na.rm=TRUE)
  fire.size <- rasterize(fpa.sit.join, r, "FIRE_SIZE", fun=mean, na.rm=TRUE)
  cost <- rasterize(fpa.sit.join, r, "Costs", fun=sum, na.rm=TRUE)
  duration <- rasterize(fpa.sit.join, r, "Duration", fun=mean, na.rm=TRUE)
  distance <- rasterize(fpa.sit.join, r, "dist", fun=mean, na.rm=TRUE)
  btl.acre <- rasterize(mpb.pts, r, "ORIG_FID", fun='count', na.rm=TRUE)
  df <- as.data.frame(cbind(getValues(cost), getValues(fire.acre), getValues(duration), getValues(fire.size), getValues(fire.dens), getValues(distance), getValues(btl.acre)))
  colnames(df) <- c("Cost", "BurnedArea","Duration", "FireSize", "FireDens", "Distance", "MPBAcres")
  df$LogCost <- log(df$Cost)
  df$LogBA <- log(df$BurnedArea)
  df$LogMPB <- log(df$MPBAcres)
  df$LogSize <- log(df$FireSize)
  df$LogDens <- log(df$FireDens)
  df$LogDist <- log(df$Distance)
  df<- df[varlist]
  df.n <- df[df$LogMPB != -Inf & 
               df$LogCost != -Inf & 
               df$LogBA != -Inf &
               df$LogSize != -Inf &
               df$LogDens != -Inf &
               df$LogDist != -Inf,]
  df.n <- na.omit(df.n)
  df.n <- df.n[varlist.n]
  # png(paste("raster_", cellsize.lab[i], ".png", sep = ""), width=12, height=8, units="in", res=300)
  # par(mfrow=c(1,1),xpd=FALSE,mar=c(4,4,2,2.5))
  # plot(df.n, cex=0.2, main=paste("Spatial resolution:", cellsize.lab[i]))
  # dev.off()
  df.n$CellSize <- cell.size[i]
  ndf <- rbind(ndf, df.n)
  print(paste(cell.size[i], "Done!"))
  # print(paste("Results for spatial resolution:", cellsize.lab[i]))
  # M <- cor(df.n)
  # corrplot(M, order="AOE", cl.pos="b", tl.pos="d", tl.srt=60)
  # res <- cor.mtest(df.n,0.95)
  # png(paste("corr_", cellsize.lab[i], ".png", sep = ""), width=8, height=6, units="in", res=300)
  # par(mfrow=c(1,1),xpd=FALSE,mar=c(1,1,1,1))
  # corrplot.mixed(M, p.mat = res[[1]], sig.level=0.05)
  # dev.off()
  # mlr <- lm(LogMPB ~ ., data=df.n)
  # #mlr <- lm(LogMPB ~ LogCost + Duration + LogSize + LogDens + LogDist, data=df.n)
  # summary(mlr)
  # layout(matrix(c(1,2,3,4),2,2))
  # plot(mlr, cex=0.2)
  # gvmodel <- gvlma(mlr)
  # summary(gvmodel)
  # step <- stepAIC(mlr, direction="both")
  # print(step$anova)
}
write.csv(ndf, "/Users/dongmeichen/Documents/writing/fire suppression/output/v4/df/raster_scaling_point_df.csv", row.names = FALSE)

save.image("~/Documents/writing/fire suppression/scripts/data_analysis.RData")

