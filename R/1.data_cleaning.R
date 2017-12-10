## Dongmei CHEN
# objectives: fire suppression data cleanning
# reference: 1. /Users/dongmeichen/Documents/scripts/beetle/r/presentation/fire_beetle.Rmd;
#            2. /Users/dongmeichen/Documents/scripts/beetle/r/presentation/fire_suppression_beetle_20170112.Rmd;
#            3. /Users/dongmeichen/Documents/scripts/beetle/r/fire_suppression/jurisdictional_v2.R;
# input: 1. /Users/dongmeichen/Documents/writing/fire suppression/data/fpafod.csv

# output: data frames or spatial data frames

# libraries
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(data.table)
library(maptools)

# region of interest
# WGS84
lonlat <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# Set the same projection with MPB data
crs <- CRS("+proj=laea +lon_0=-112.5 +lat_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 

# functions
mode <- function(x) {
  return(names(sort(-table(x)))[1])
}

df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  xy.n <- spTransform(xy, crs)
  spdf <- SpatialPointsDataFrame(coords = xy.n, data = df, proj4string = crs)
  return(spdf)
}

# global settings
inpath <- "/Users/dongmeichen/Documents/writing/fire suppression/data/"
outpath <- "/Users/dongmeichen/Documents/writing/fire suppression/output/v4/"

# ROI
mpb10km <- readOGR(dsn = inpath, layer = "mpb10km_us")
proj4string(mpb10km) <- crs
#writeOGR(mpb10km, dsn = paste0(outpath,"spdf"), layer = "mpb10km", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# FPA-FOD
rds <- readOGR(dsn="/Users/dongmeichen/Documents/writing/fire suppression/data/RDS-2013-0009/Data/FPA_FOD_20170508.gdb", 
               layer="Fires", stringsAsFactors = FALSE, dropNULLGeometries = FALSE) # with 1880465 features
fpafod <- as.data.frame(rds)
# write.csv(fpafod, paste(inpath,"fpafod.csv", sep=""), row.names = FALSE)
fpa.spdf <- spTransform(rds, crs)
fpa.westus <- gIntersection(fpa.spdf, mpb10km)
png(paste0(outpath,"figures/fpa_fod_westUS.png"), width = 10, height = 8, units = "in", res=300)
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, main="FPA-FOD fire records in the western US")
points(fpa.westus$x, fpa.westus$y, pch=16, col="red", cex=0.2)
plot(mpb10km, border="dimgray", add=TRUE)
dev.off()
fpa_westus <- fpa.spdf[!is.na(over(fpa.spdf, as(mpb10km, "SpatialPolygons"))),]
writeOGR(fpa_westus, dsn = paste0(outpath,"spdf"), layer = "fpa_westus", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# SIT-209
# Combine yearly data
# Information needed: coordinates, start and containment dates, fire suppression cost, and burned area
merge <- data.frame(ID = character(0), LON = numeric(0), LAT = numeric(0), SDATE=character(0), CDATE=character(0), COST = numeric(0), AREA= numeric(0), YEAR= numeric(0), STATE=character(0))
for (year in 1999:2016){
  ics.file <- paste0(inpath, "SIT-209/", year,"_Incidents.csv")
  df <- read.csv(ics.file, stringsAsFactors = FALSE)
  if (year == 1999 | year == 2000){
    df.1 <- df[df$F_CONTAIN==100,]
    df.1$LAT <- df.1$LATDEG + df.1$LATMIN/60
    df.1$LON <- df.1$LONGDEG + df.1$LONGMIN/60
    df.n <- data.frame(cbind(df.1$EVENT_ID, df.1$LON, df.1$LAT, df.1$STARTDATE, df.1$REPDATE, df.1$ECOSTS, df.1$ACRES, c(rep(year, length(dim(df.1)[1]),1)),df.1$UN_USTATE))
  } else if (year >= 2014){
    df.1 <- df[df$PCT_CONTAINED_COMPLETED==100,]
    df.n <- data.frame(cbind(df.1$INCIDENT_NUMBER, df.1$POO_LONGITUDE, df.1$POO_LATITUDE, df.1$DISCOVERY_DATE, df.1$LAST_MODIFIED_DATE, df.1$EST_IM_COST_TO_DATE, df.1$INCIDENT_AREA, c(rep(year, length(dim(df.1)[1]),1)), df.1$POO_STATE_CODE))
  } else if (year == 2013){
    df.1 <- df[df$P_CONTAIN==100,]
    df.n <- data.frame(cbind(df.1$INCIDENT_NUMBER, df.1$LONGITUDE, df.1$LATITUDE, df.1$START_DATE, df.1$CONTROLLED_DATE, df.1$COSTS_TO_DATE, df.1$AREA, c(rep(year, length(dim(df.1)[1]),1)),df.1$OWNERSHIP_STATE))
  } else{
    df.1 <- df[df$P_CONTAIN==100,]
    df.n <- data.frame(cbind(df.1$INCIDENT_NUMBER, df.1$LONGITUDE, df.1$LATITUDE, df.1$START_DATE, df.1$CONTROLLED_DATE, df.1$COSTS_TO_DATE, df.1$AREA, c(rep(year, length(dim(df.1)[1]),1)),df.1$UN_USTATE))
  }                                                                                                                    
  merge <- rbind(merge, df.n)
  print(year)
}
# rename the table fields
colnames(merge) <- c("SITID","Longitude", "Latitude", "StartDate", "EndDate", "Costs", "Acres", "Year", "State")
# clean up: convert eastern longitude to western longitude and remove the wrong coordinates
merge$Longitude <- as.numeric(as.character(merge$Longitude))
merge$Latitude <- as.numeric(as.character(merge$Latitude))
merge$Costs <- as.numeric(as.character(merge$Costs))
merge$Acres <- as.numeric(as.character(merge$Acres))
merge$Year <- as.numeric(as.character(merge$Year))
# when there is more than one record for each fire, maximum fire suppression cost is retained
print(paste("SIT-209 has", length(merge$SITID), "records during", range(merge$Year)[1], "and",range(merge$Year)[2],"in raw data where percent containment is 100!"))
# "SIT-209 has 167594 records during 1999 and 2016 in raw data where percent containment is 100!"
# write.csv(merge, paste(outpath,"df/ics-209.csv", sep=""), row.names = FALSE)
icsdata.a <- aggregate(cbind(Longitude, Latitude, Year, State) ~ SITID, data = merge, mode)
icsdata.b <- aggregate(cbind(Costs, Acres) ~ SITID, data = merge, max)
icsdata.c <- aggregate(StartDate ~ SITID, data=merge, function(x) min(as.Date(x, format = "%m/%d/%Y")))
icsdata.d <- aggregate(EndDate ~ SITID, data=merge, function(x) max(as.Date(x, format = "%m/%d/%Y")))
icsdata.1 <- merge(icsdata.a, icsdata.b, by = "SITID", all=TRUE)
icsdata.2 <- merge(icsdata.c, icsdata.d, by = "SITID", all=TRUE)
icsdata.3 <- merge(icsdata.1, icsdata.2, by = "SITID", all=TRUE)
print(paste("SIT-209 has ", toString(length(icsdata.3[icsdata.3$Longitude==0,]$Longitude)+length(icsdata.3[icsdata.3$Longitude>0,]$Longitude)), " erroneous location in ", toString(length(icsdata.3$SITID)), " records.", sep=""))
# "SIT-209 has 19883 erroneous location in 23691 records."
icsdata.3$Longitude <- with(icsdata.3, ifelse(Longitude < 0, Longitude, -(as.numeric(Longitude))))
# create a table retain unique records with fire suppression costs
icsdata.n <- icsdata.3
icsdata.n1 <- icsdata.n[!is.na(as.numeric(icsdata.n$Costs)) & icsdata.n$Costs >= 100,]
icsdata.n1$LogCost <- log(icsdata.n1$Costs)
icsdata.n1$LogAcre <- log(icsdata.n1$Acre)
#write.csv(icsdata.n1, paste(outpath,"df/ics-209-costs-aggregate.csv", sep="")) # this table is used to join table in ArcGIS
colnames(icsdata.n)[1] <- "ICS_209_INCIDENT_NUMBER"
# remove zeros in coordinates
icsdata.n.1 <- with(icsdata.n, icsdata.n[!(Longitude == 0), ])
icsdata.n.2 <- with(icsdata.n.1, icsdata.n.1[!(Latitude == 0), ])
icsdata.n.3 <- icsdata.n.2[!is.na(icsdata.n.2$Longitude) & !is.na(icsdata.n.2$Latitude),]
icsdata.n.3$Longitude <- as.numeric(icsdata.n.3$Longitude)
icsdata.n.3$Latitude <- as.numeric(icsdata.n.3$Latitude)
# length(icsdata.n.3$ICS_209_INCIDENT_NUMBER)
# [1] 23112
# save points in contiguous US
icsdata.n.3 <- icsdata.n.3[icsdata.n.3$Longitude > -125 & icsdata.n.3$Longitude < -66 
                           & icsdata.n.3$Latitude > 24 & icsdata.n.3$Latitude < 50, ] 
# length(icsdata.n.3$ICS_209_INCIDENT_NUMBER)
# [1] 22548
xy <- data.frame(icsdata.n.3[,c(2,3)])
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- lonlat
xy.n <- spTransform(xy, crs)
sit.spdf <- SpatialPointsDataFrame(coords = xy.n, data = icsdata.n.3, proj4string = crs)
sit.westus <- gIntersection(sit.spdf, mpb10km)
png(paste0(outpath,"figures/sit_209_westUS.png"), width = 10, height = 8, units = "in", res=300)
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, main="SIT-209 fire records in the western US")
points(sit.westus$x, sit.westus$y, pch=16, col="red", cex=0.2)
dev.off()
sit_westus <- sit.spdf[!is.na(over(sit.spdf, as(mpb10km, "SpatialPolygons"))),]
writeOGR(sit_westus, dsn = paste0(outpath,"spdf"), layer = "sit_westus", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# MTBS
mtbs.shp <- readOGR(dsn=paste0(inpath, "mtbs_perimeter_data"), layer="mtbs_perims_1984-2015_DD_20170815") # this takes some time
mtbs.pts <- readOGR(dsn=paste0(inpath, "mtbs_fod_pts_data"), layer="mtbs_fod_pts_20170501")
mtbs.shp <- spTransform(mtbs.shp, crs)
mtbs.pts <- spTransform(mtbs.pts, crs)
mtbs.df <- as.data.frame(mtbs.pts)
print(names(mtbs.df)); colnames(mtbs.df)[1] <- "MTBS_ID" # for data merge
head(mtbs.df)
mpb10km <- gBuffer(mpb10km, byid=TRUE, width=0)
mtbs.shp <- gSimplify(mtbs.shp, tol = 0.00001)
mtbs.shp <- gBuffer(mtbs.shp, byid=TRUE, width=0)
ptm <- proc.time()
mtbs.westus <- gIntersection(mtbs.shp, byid=TRUE, mpb10km) # this takes a while
proc.time() - ptm # 552.123s
mtbs.pts.westus <- mtbs.pts[!is.na(over(mtbs.pts, as(mpb10km, "SpatialPolygons"))),]
writeOGR(mtbs.pts.westus, dsn = paste0(outpath,"spdf"), layer = "mtbs_pts_westus", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# ptm <- proc.time()
# png(paste0(outpath,"figures/mtbs_westUS.png"), width = 10, height = 8, units = "in", res=300)
# par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
# plot(mpb10km, border = "gray", main="MTBS fire records in the western US")
# plot(mtbs.westus, col='lightblue', border='red', add=TRUE)# it takes much time to plot the polygons
# dev.off()
# proc.time() - ptm # 1793.970s

# ptm <- proc.time()
# png(paste0(outpath,"figures/fires_westUS.png"), width = 12, height = 4, units = "in", res=300)
# par(mfrow=c(1,3),xpd=FALSE,mar=c(0,0,2,0))
# plot(mpb10km, main="FPA FOD fire records")
# points(fpa.westus$x, fpa.westus$y, pch=16, col="red", cex=0.2)
# plot(mpb10km, main="SIT-209 fire records")
# points(sit.westus$x, sit.westus$y, pch=16, col="red", cex=0.5)
# plot(mpb10km, border = "gray", main="MTBS fire records")
# plot(mtbs.westus, col='lightblue', border='red', add=TRUE)# it takes much time to plot the polygons
# dev.off()
# proc.time() - ptm # 3330.363

# Federal Wild Fire Occurrence Data
fwfod <- readOGR(dsn = "/Users/dongmeichen/Documents/writing/fire suppression/data/wf_all_1980_2016", layer = "wf_all_1980_2016", stringsAsFactors = FALSE)
# Warning message:
#   In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation
fwfod <- spTransform(fwfod, crs)
fwfod.1 <- subset(fwfod, FIRETYPE == '1')
fwfod_westus <- fwfod.1[!is.na(over(fwfod.1, as(mpb10km, "SpatialPolygons"))),]
writeOGR(fwfod_westus, dsn = paste0(outpath,"spdf"), layer = "fwfod_westus", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# merge: make sure match one by one
length(fpafod$ICS_209_INCIDENT_NUMBER);length(unique(fpafod$ICS_209_INCIDENT_NUMBER))
# [1] 1880465
# [1] 22738
length(fpa_westus@data$ICS_209_INCIDENT_NUMBER);length(unique(fpa_westus@data$ICS_209_INCIDENT_NUMBER))
# [1] 620658
# [1] 11323
length(fpafod$MTBS_ID);length(unique(fpafod$MTBS_ID))
# [1] 1880465
# [1] 10482
length(fpa_westus@data$MTBS_ID);length(unique(fpa_westus@data$MTBS_ID))
# [1] 620658
# [1] 6848
length(icsdata.n$ICS_209_INCIDENT_NUMBER);length(unique(icsdata.n$ICS_209_INCIDENT_NUMBER))
# [1] 23691
# [1] 23691
length(sit_westus@data$ICS_209_INCIDENT_NUMBER);length(unique(sit_westus@data$ICS_209_INCIDENT_NUMBER))
# [1] 9833
# [1] 9833
length(mtbs.df$MTBS_ID);length(unique(mtbs.df$MTBS_ID))
# [1] 20340
# [1] 20340
length(mtbs.pts@data$FIRE_ID);length(unique(mtbs.pts@data$FIRE_ID))
# [1] 20340
# [1] 20340
length(mtbs.pts.westus@data$FIRE_ID);length(unique(mtbs.pts.westus@data$FIRE_ID))
# [1] 8986
# [1] 8986
6848/8986
# [1] 0.7620743

# merge SIT-209 and fpafod
fpafod.dt <- data.table(fpa_westus@data)
setkey(fpafod.dt, "ICS_209_INCIDENT_NUMBER")
fpafod.1 <- fpafod.dt[, list(MTBS_ID= first(MTBS_ID), FIRE_YEAR = first(FIRE_YEAR), LONGITUDE=first(LONGITUDE), LATITUDE=first(LATITUDE),   
                             DISCOVERY_DATE=first(DISCOVERY_DATE), DISCOVERY_DOY = first(DISCOVERY_DOY), 
                             CONT_DATE=last(CONT_DATE), CONT_DOY=last(CONT_DOY), FIRE_SIZE=mean(FIRE_SIZE)), by=key(fpafod.dt)]
length(fpafod.1$ICS_209_INCIDENT_NUMBER);length(unique(fpafod.1$ICS_209_INCIDENT_NUMBER))
# [1] 11323
# [1] 11323
fpa.sit.df <- merge(fpafod.1, icsdata.n, by= "ICS_209_INCIDENT_NUMBER", all=TRUE)
fpa.sit.df$Duration1 <- as.numeric(difftime(as.Date(fpa.sit.df$CONT_DATE), as.Date(fpa.sit.df$DISCOVERY_DATE),units = "days"))
fpa.sit.df$Duration2 <- fpa.sit.df$CONT_DOY - fpa.sit.df$DISCOVERY_DOY
fpa.sit.df$Duration3 <- as.numeric(difftime(as.Date(fpa.sit.df$EndDate), as.Date(fpa.sit.df$StartDate),units = "days"))
fpa.sit.df$Duration <- ifelse(fpa.sit.df$Duration1 > 200 | fpa.sit.df$Duration1 < 0, abs(fpa.sit.df$Duration2), fpa.sit.df$Duration1)
fpa.sit.df$Duration <- ifelse(fpa.sit.df$Duration1 == 0 & fpa.sit.df$Duration2 == 0 & fpa.sit.df$Duration3 > 0 & fpa.sit.df$Duration3 < 200, fpa.sit.df$Duration3, fpa.sit.df$Duration)

# get a table to join in ArcGIS
fpa.sit.tb <- fpa.sit.df[!is.na(ICS_209_INCIDENT_NUMBER) & !is.na(MTBS_ID),]
setkey(fpa.sit.tb, "MTBS_ID")
length(fpa.sit.tb$MTBS_ID);length(unique(fpa.sit.tb$MTBS_ID))
# [1] 4509
# [1] 4405
fpa.sit.tb.1 <- fpa.sit.tb[, list(ICS_209_INCIDENT_NUMBER = first(ICS_209_INCIDENT_NUMBER), FIRE_YEAR = first(FIRE_YEAR), 
                                  LONGITUDE=first(LONGITUDE), LATITUDE=first(LATITUDE),   
                                  DISCOVERY_DATE=first(DISCOVERY_DATE), DISCOVERY_DOY = first(DISCOVERY_DOY), 
                                  CONT_DATE=last(CONT_DATE), CONT_DOY=last(CONT_DOY), FIRE_SIZE=mean(FIRE_SIZE), 
                                  Longitude = first(Longitude), Latitude = first(Latitude), Year=first(Year), State=first(State), 
                                  Costs = first(Costs), Acres = first(Acres), Duration = first(Duration)), by=key(fpa.sit.tb)]
length(fpa.sit.tb.1$MTBS_ID);length(unique(fpa.sit.tb.1$MTBS_ID))
# [1] 4405
# [1] 4405
fpa.sit.tb.1$LogCost <- log(fpa.sit.tb.1$Costs)
fpa.sit.tb.1$LogAcre <- log(fpa.sit.tb.1$Acres)
write.csv(as.data.frame(fpa.sit.tb.1), paste0(inpath,"fpa_sit_2_mtbs.csv"), row.names = FALSE)
fpa.sit.df.1 <- as.data.frame(fpa.sit.df[!is.na(fpa.sit.df$LATITUDE) & !is.na(fpa.sit.df$LONGITUDE),]) 
write.csv(fpa.sit.df.1, paste0(outpath,"df/fpa_sit_wus.csv"), row.names = FALSE)
fpa.sit.spdf <- df2spdf(4, 5, "LONGITUDE", "LATITUDE", fpa.sit.df.1)
fpa.sit.spdf <- fpa.sit.spdf[!is.na(fpa.sit.spdf$MTBS_ID),]
names(fpa.sit.spdf)[which(names(fpa.sit.spdf)=="Year")] = "SIT_Year"
writeOGR(fpa.sit.spdf, dsn = paste0(outpath,"spdf"), layer = "fpa_sit_wus_spdf", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writePointsShape(fpa.sit.spdf, paste0(outpath,"spdf/fpa_sit_wus_spdf"))

print(names(fpa.sit.tb.1))
colnames(fpa.sit.tb.1)[which(names(fpa.sit.tb.1) == "Acres")] <- "SIT_Acres" # differentiate the acres from MTBS
colnames(fpa.sit.tb.1)[which(names(fpa.sit.tb.1) == "FIRE_YEAR")] <- "YEAR"
fpa.sit.mtbs.df <- merge(fpa.sit.tb.1, mtbs.df, by= "MTBS_ID", all=TRUE)
print(colnames(fpa.sit.mtbs.df))
head(fpa.sit.mtbs.df) # merge of FPA-FOD, SIT-209 and MTBS
write.csv(fpa.sit.mtbs.df, paste0(outpath,"df/fpa_sit_mtbs.csv"), row.names = FALSE)

# MPB data (it takes too much time here, or pre-processing in ArcGIS)
# # intermountain
# system("/anaconda/bin/ogr2ogr -f CSV /Users/dongmeichen/Documents/beetle/csvfiles/IDS_rollup.csv /Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Intermountain_Nov19183929_100/Intermountain_Nov19183929_100.gdb IDS_rollup")
# system("/anaconda/bin/ogr2ogr -f CSV /Users/dongmeichen/Documents/beetle/csvfiles/IDS_attrib.csv /Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Intermountain_Nov19183929_100/Intermountain_Nov19183929_100.gdb IDS_attrib")
# 
# Intermountain <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Intermountain_Nov19183929_100/Intermountain_Nov19183929_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# IDS_rollup <- read.csv("/Users/dongmeichen/Documents/beetle/csvfiles/IDS_rollup.csv", stringsAsFactors = F)
# IDS_attrib <- read.csv("/Users/dongmeichen/Documents/beetle/csvfiles/IDS_attrib.csv", stringsAsFactors = F)
# 
# IDS_rollup <- IDS_rollup[IDS_rollup$rollup_name == "mountain pine beetle summary", ]
# IDS_attrib <- IDS_attrib[IDS_attrib$STACK_ID %in% IDS_rollup$stack_id,]
# Intermountain <- Intermountain[Intermountain$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID, ]
# writeOGR(Intermountain, dsn = paste0(outpath,"spdf"), layer = "Intermountain", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# # northern
# Northern <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Northern_Nov19185622_100/Northern_Nov19185622_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# Northern <- Northern[Northern$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID,]
# writeOGR(Northern, dsn = paste0(outpath,"spdf"), layer = "Northern", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# # northwest
# Northwest <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Pacific_Northwest_Nov19191634_100/Pacific_Northwest_Nov19191634_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# Northwest <- Northwest[Northwest$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID,]
# writeOGR(Northwest, dsn = paste0(outpath,"spdf"), layer = "Northwest", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# # southwest
# Southwest <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Pacific_Southwest_Nov19193347_100/Pacific_Southwest_Nov19193347_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# Southwest <- Southwest[Southwest$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID,]
# writeOGR(Southwest, dsn = paste0(outpath,"spdf"), layer = "Southwest", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# # Rocky
# Rocky <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Rocky_Mountain_Nov19202113_100/Rocky_Mountain_Nov19202113_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# Rocky <- Rocky[Rocky$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID,]
# writeOGR(Rocky, dsn = paste0(outpath,"spdf"), layer = "Rocky", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# # southwestern
# Southwestern <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/mpbdata/raw/us/Southwestern_Nov19200222_100/Southwestern_Nov19200222_100.gdb", layer="IDS_Shapes", stringsAsFactors = FALSE, dropNULLGeometries = FALSE)
# Southwestern <- Southwestern[Southwestern$ALLYEARS_ID %in% IDS_attrib$ALLYEARS_ID,]
# writeOGR(Southwestern, dsn = paste0(outpath,"spdf"), layer = "Southwestern", driver = "ESRI Shapefile", overwrite_layer = TRUE)
mpb.pts <- readOGR(dsn = inpath, layer = "us_mpb_points")
mpb.pts <- spTransform(mpb.pts, crs)

png(paste0(outpath,"figures/fires_pts_westUS.png"), width = 8, height = 8, units = "in", res=300)
par(mfrow=c(2,2),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, main="FPA FOD fire records (1992-2015)")
points(fpa.westus$x, fpa.westus$y, pch=16, col="red", cex=0.3)
plot(mpb10km, main="SIT-209 fire records (1999-2016)")
points(sit.westus$x, sit.westus$y, pch=16, col="red", cex=0.3)
plot(mpb10km, main="MTBS fire records (1984-2015)")
plot(mtbs.pts.westus, pch=16, col="red", cex=0.3, add=T)
plot(mpb10km, main="FWFOD fire records (1980-2016)")
plot(fwfod_westus, pch=16, col="red", cex=0.3, add=T)
dev.off()

save.image("~/Documents/writing/fire suppression/scripts/data_analysis.RData")
