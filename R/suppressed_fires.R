library(rgdal)
library(raster)
library(DescTools)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
mpb10km.pts <- readOGR(dsn = mpb10km.path, layer = "mpb10km_us_gridpts")
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km.pts, pch=16, col=alpha("red", alpha = 0.3), cex=0.2)

mpb.pts <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb", layer="MPB_points")
mpb.pts <- spTransform(mpb.pts, crs)

fire.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/firedata/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
fwfod <- readOGR(dsn=paste0(fire.path, "wf_all_1980_2016"), layer="wf_all_1980_2016", 
								 stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
fwfod <- spTransform(fwfod, crs)
fpafod <- readOGR(dsn=paste0(fire.path, "RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb"), 
               layer="Fires", stringsAsFactors = FALSE, dropNULLGeometries = FALSE) 
fpafod <- spTransform(fpafod, crs)

fwfod.c <- fwfod[!is.na(fwfod$CAUSE),]
fwfod.c.s <- fwfod.c[!is.na(fwfod.c$FIRETYPE) & fwfod.c$FIRETYPE == "1",]
# select naturally caused fires
fwfod.n <- fwfod[!is.na(fwfod$CAUSE) & fwfod$CAUSE == "Natural",]
# select suppressed fires
fwfod.n.s <- fwfod.n[!is.na(fwfod.n$FIRETYPE) & fwfod.n$FIRETYPE == "1",]
# sit209 <- readOGR(dsn=fire.path, layer="sit_westus", stringsAsFactors = FALSE)
sit209.df <- read.csv('/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/icsdata_coordinates.csv')
sit209.df$LONGITUDE <- ifelse(is.na(sit209.df$LONGITUDE) & !is.na(sit209.df$Longitude), sit209.df$Longitude, sit209.df$LONGITUDE)
sit209.df$LATITUDE <- ifelse(is.na(sit209.df$LATITUDE) & !is.na(sit209.df$Latitude), sit209.df$Latitude, sit209.df$LATITUDE)
sit209.df <- sit209.df[!is.na(sit209.df$LONGITUDE) & !is.na(sit209.df$LATITUDE),]

sit209.spdf <- df2spdf(10, 11, 'LONGITUDE', 'LATITUDE', sit209.df)
sit209.spdf <- sit209.spdf[!is.na(over(sit209.spdf, as(mpb10km, "SpatialPolygons"))),]

plot(mpb10km.lonlat)
points(sit209.df$LONGITUDE, sit209.df$LATITUDE, pch=16, cex=0.5, col='red')
sit209.df <- sit209.df[sit209.df$LONGITUDE > -125 & sit209.df$LONGITUDE < -66 
                           & sit209.df$LATITUDE > 24 & sit209.df$LATITUDE < 50, ] 

# remove NA values?                           
sit209.df.na <- sit209.df[(is.na(sit209.df$LONGITUDE) | is.na(sit209.df$LATITUDE)),]
#dim(sit209.df.na)
#[1] 6652   13
sit209.df.na.na <- sit209.df.na[(is.na(sit209.df.na$Longitude) | is.na(sit209.df.na$Latitude)),]
#dim(sit209.df.na.na)
#[1] 126  13

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
fwfod.c.s$FIRETYPE <- as.numeric(fwfod.c.s$FIRETYPE)
fwfod.c$Cause <- rep(1, length(fwfod.c$CAUSE))
# suppression of naturally caused fires
fire.sprs <- rasterized(fwfod.n.s, "FIRETYPE", sum)
fire.natr <- rasterized(fwfod.n, "Natural", sum)
# all suppressed fires
fire.sprs.a <- rasterized(fwfod.c.s, "FIRETYPE", sum)
fire.a <- rasterized(fwfod.c, "Cause", sum)
pct.sprs <- fire.sprs/fire.natr
pct.sprs.a <- fire.sprs.a/fire.a
#size.sprs <- rasterized(fwfod.n.s, "SIZECLASSN", mode)
gridacre <- 24710.5
fwfod.n.s$TOTALACRES <- ifelse(fwfod.n.s$TOTALACRES > gridacre, gridacre, fwfod.n.s$TOTALACRES)
acres.sprs <- rasterized(fwfod.n.s, "TOTALACRES", mean)
fwfod.c.s$TOTALACRES <- ifelse(fwfod.c.s$TOTALACRES > gridacre, gridacre, fwfod.c.s$TOTALACRES)
acres.sprs.a <- rasterized(fwfod.c.s, "TOTALACRES", mean)

fwfod.n.s$STARTDATED <- as.Date(fwfod.n.s$STARTDATED, format = "%Y/%m/%d")
fwfod.n.s$CONTRDATED <- as.Date(fwfod.n.s$CONTRDATED, format = "%Y/%m/%d")
fwfod.n.s$OUTDATED <- as.Date(fwfod.n.s$OUTDATED, format = "%Y/%m/%d")
fwfod.n.s$SprsDuration <- abs(as.numeric(difftime(fwfod.n.s$STARTDATED, fwfod.n.s$CONTRDATED, units="days")))
fwfod.n.s$OutDuration <- abs(as.numeric(difftime(fwfod.n.s$STARTDATED, fwfod.n.s$OUTDATED, units="days")))

fwfod.c.s$STARTDATED <- as.Date(fwfod.c.s$STARTDATED, format = "%Y/%m/%d")
fwfod.c.s$CONTRDATED <- as.Date(fwfod.c.s$CONTRDATED, format = "%Y/%m/%d")
fwfod.c.s$OUTDATED <- as.Date(fwfod.c.s$OUTDATED, format = "%Y/%m/%d")
fwfod.c.s$SprsDuration <- abs(as.numeric(difftime(fwfod.c.s$STARTDATED, fwfod.c.s$CONTRDATED, units="days")))
fwfod.c.s$OutDuration <- abs(as.numeric(difftime(fwfod.c.s$STARTDATED, fwfod.c.s$OUTDATED, units="days")))

dates.sprs <- rasterized(fwfod.n.s, "SprsDuration", median)
dates.out <- rasterized(fwfod.n.s, "OutDuration", median) 

# extract values
SprsFires <- data.frame(SprsFires=extract(fire.sprs, mpb10km.pt, method='simple'))
PctSprs <- data.frame(PctSprs=extract(pct.sprs, mpb10km.pt, method='simple'))
#SprsSize <- data.frame(SprsSize=extract(size.sprs, mpb10km.pt, method='simple'))
SprsAcre <- data.frame(SprsAcre=extract(acres.sprs, mpb10km.pt, method='simple'))
SprsDays <- data.frame(SprsDays=extract(dates.sprs, mpb10km.pt, method='simple'))
OutDays <- data.frame(OutDays=extract(dates.out, mpb10km.pt, method='simple'))
df <- cbind(SprsFires, PctSprs, SprsAcre, SprsDays, OutDays)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
write.csv(df, paste0(csvpath, "suppressed_fires.csv"), row.names=FALSE)

fpafod$logFS <- log(fpafod$FIRE_SIZE+1)
firesize <- rasterized(fpafod, "logFS", median)
firefreq <- rasterized(fpafod, "FIRE_SIZE", count)
gini.fs <- rasterized(fpafod, "FIRE_SIZE", gini)
gini.cost <- rasterized(sit209, "Costs", gini)
sit209 <- sit209[!is.na(sit209$Costs) & sit209$Costs >0,]
sit209$Costs <- ifelse(sit209$Acres > gridacre, sit209$Costs/sit209$Acres * gridacre, sit209$Costs)
sit209$Acres <- ifelse(sit209$Acres > gridacre, gridacre, sit209$Acres)
sit209$LogCost <- log(sit209$Costs)
logCost <- rasterized(sit209, "LogCost", median)
#costs <- rasterized(sit209, "Costs", median)
costs <- rasterized(sit209.spdf, "Costs", median)
#acres <- rasterized(sit209, "Acres", median)
acres <- rasterized(sit209.spdf, "Acres", median)
sit209$CostPerAcre <- sit209$Costs / sit209$Acres
sit209.spdf$CostPerAcre <- sit209.spdf$Costs / sit209.spdf$Acres
#CostPerAcre <- costs/acres
CostPerAcre <- rasterized(sit209.spdf, "CostPerAcre", median)
#CostPerAcre <- rasterized(sit209, "CostPerAcre", median)
par(mfrow=c(1,1),xpd=FALSE,mar=c(2,2,2,3))
ncls <- 5
cols <- "Reds"
plot(CostPerAcre, col = brewer.pal(ncls,cols))
mpb.acre <- rasterized(mpb.pts, "ACRES", sum.log)

# extract values
SprsCosts <- data.frame(SprsCosts=extract(costs, mpb10km.pt, method='simple'))
SprsAcres <- data.frame(SprsAcres=extract(acres, mpb10km.pt, method='simple'))
SprsCPA <- data.frame(SprsCPA=extract(CostPerAcre, mpb10km.pt, method='simple'))
df <- cbind(SprsCosts, SprsAcres, SprsCPA)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
write.csv(df, paste0(csvpath, "suppressed_costs.csv"), row.names=FALSE)

mapping("pct_sprs", pct.sprs, "Percent of naturally-caused fires suppressed", d=1, "Reds", "kmeans")
mapping("fire_sprs", fire.sprs, "Number of naturally-caused fires suppressed", d=0,"Reds", "kmeans")
mapping("firesize", firesize, "Median fire size (log)", d=0, "YlOrBr", "pretty")
mapping("firefreq", firefreq, "Number of fires(log)", d=0, "YlOrBr", "pretty")
mapping("elevation", mpb10km.pts.r, "Elevation", d=0, "Greys", "kmeans")
mapping("costs", logCost, "Fire containment cost per fire (log)", d=0,  "YlOrBr", "pretty")
mapping("gini_index", gini.fs, "Gini index of fire size", d=1, "YlOrBr", "quantile")
mapping("gini_index_costs", gini.cost, "Gini index of fire containment costs", d=1, "YlOrBr", "pretty")
mapping("mpb_acre", mpb.acre, "Beetle affected acres (log)", d=0, "YlOrBr", "kmeans")
mapping("dates_sprs", dates.sprs, "Containment duration", d=0, "YlOrBr", "kmeans")
