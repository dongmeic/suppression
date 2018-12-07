# libraries
library(sp)
library(rgdal)
library(raster)
library(data.table)
library(maptools)

# region of interest
mpb10km <- readOGR(dsn = "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km", layer = "mpb10km")

# WGS84
lonlat <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# Set the same projection with MPB data
crs <- proj4string(mpb10km) 
# "+proj=laea +lat_0=45 +lon_0=-112.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 

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

fire.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/firedata/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
# FPA-FOD
rds <- readOGR(dsn=paste0(fire.path, "RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb"), 
               layer="Fires", stringsAsFactors = FALSE, dropNULLGeometries = FALSE) 
fpafod <- as.data.frame(rds)
#write.csv(fpafod, paste0(fire.path,"fpafod.csv"), row.names = FALSE)
fpa.spdf <- spTransform(rds, crs)
png(paste0(out,"fpa_fod.png"), width = 10, height = 8, units = "in", res=300)
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,2,0))
plot(mpb10km, main="FPA-FOD fire records in the western US")
points(fpa.spdf, pch=16, col="red", cex=0.2)
plot(mpb10km, border="dimgray", add=TRUE)
dev.off()

# SIT-209
# Combine yearly data
# Information needed: coordinates, start and containment dates, fire suppression cost, and burned area
merge <- data.frame(ID = character(0), LON = numeric(0), LAT = numeric(0), SDATE=character(0), CDATE=character(0), COST = numeric(0), AREA= numeric(0), YEAR= numeric(0), STATE=character(0))
for (year in 1999:2016){
  ics.file <- paste0(fire.path,"SIT-209/",year,"_Incidents.csv")
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
# "SIT-209 has 104400 records during 1999 and 2016 in raw data where percent containment is 100!"
# write.csv(merge, paste0(fire.path,"ics-209.csv"), row.names = FALSE)
icsdata.a <- aggregate(cbind(Longitude, Latitude, Year, State) ~ SITID, data = merge, mode)
icsdata.b <- aggregate(cbind(Costs, Acres) ~ SITID, data = merge, max)
icsdata.c <- aggregate(StartDate ~ SITID, data=merge, function(x) min(as.Date(x, format = "%m/%d/%Y")))
icsdata.d <- aggregate(EndDate ~ SITID, data=merge, function(x) max(as.Date(x, format = "%m/%d/%Y")))
icsdata.1 <- merge(icsdata.a, icsdata.b, by = "SITID", all=TRUE)
icsdata.2 <- merge(icsdata.c, icsdata.d, by = "SITID", all=TRUE)
icsdata.3 <- merge(icsdata.1, icsdata.2, by = "SITID", all=TRUE)
print(paste("SIT-209 has ", toString(length(icsdata.3[icsdata.3$Longitude==0,]$Longitude)+length(icsdata.3[icsdata.3$Longitude>0,]$Longitude)), " erroneous location in ", toString(length(icsdata.3$SITID)), " records.", sep=""))
# "SIT-209 has 19879 erroneous location in 23691 records."
icsdata.3$Longitude <- with(icsdata.3, ifelse(Longitude < 0, Longitude, -(as.numeric(Longitude))))
# create a table retain unique records with fire suppression costs
icsdata.n <- icsdata.3
icsdata.n1 <- icsdata.n[!is.na(as.numeric(icsdata.n$Costs)) & icsdata.n$Costs >= 100,]
icsdata.n1$LogCost <- log(icsdata.n1$Costs)
icsdata.n1$LogAcre <- log(icsdata.n1$Acre)
#write.csv(icsdata.n1, paste0(fire.path,"ics-209-costs-aggregate.csv")) # this table is used to join table in ArcGIS
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