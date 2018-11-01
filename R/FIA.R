# collect data from the forest inventory and analysis database: slope ("SLOPE"), aspect ("ASPECT"), 
# stand age ("STDAGE"), forest type ("FLDTYPCD"), others ("BALIVE"-Basal area of live trees, 
# "LIVE_CANOPY_CVR_PCT"-Live canopy cover percent, "NBR_LIVE_STEMS"-Number of live stems), 
# from Condition Table, tree diameter ("DIA") from Tree Table
# breast height age ("BHAGE"), and total age ("TOTAGE") are not available for all states
# in each state, the plot number is unique, however, the plot number is repeated amonge states
# 
library(lubridate)
library(rgdal)

statecode <- c("AZ", "CA", "CO", "ID", "KS", "MT", "ND", "NE", "NM", "NV", "OK", "OR", "SD", 
               "TX", "UT", "WA","WY")
hostcode <- c(101, 102, 103, 104, 142, 161, 162, 163, 167, 168, 221, 224, 226, 241, 281, 362,
              363, 365, 366, 367, 381, 401, 404, 406, 407) # 221: ponderosa; 281: lodgepole

# functions
mode <- function(x) {
  return(names(sort(-table(x)))[1])
}

count.tree <- function(x,z){
  return(length(x[x>=z])/length(x))
}

check.exist <- function(x,code){
  if (length(x[x==code]) != 0){
    return(1)
  } else {
    return(0)
  }
}

check.hosts <- function(x){
  if (any(sapply(x, function(x) x %in% hostcode))){
    return(1)
  } else{
    return(0)
  }
}

n <- length(statecode)
# read AZ data
cond.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[1], 
                   "/", statecode[1], "_COND.csv", sep="")
cond.table <- read.csv(cond.file)
names(cond.table)
#mode(cond.table$STDAGE)
cond.stdage <- aggregate(STDAGE ~ PLT_CN, data=cond.table, mean)
cond.frtcpt <- aggregate(cbind(FLDTYPCD, LIVE_CANOPY_CVR_PCT) ~ PLT_CN, data=cond.table, mode)
cond.lodgepole <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.exist(x, 281))
colnames(cond.lodgepole)[which(names(cond.lodgepole) == "FLDTYPCD")] <- "lodgepole"
cond.ponderosa <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.exist(x, 221))
colnames(cond.ponderosa)[which(names(cond.ponderosa) == "FLDTYPCD")] <- "ponderosa"
cond.hosts <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.hosts(x))
colnames(cond.hosts)[which(names(cond.hosts) == "FLDTYPCD")] <- "hosts"
cond.sloasp <- aggregate(cbind(SLOPE, ASPECT) ~ PLT_CN, data = cond.table, range)
cond.balnbr <- aggregate(cbind(BALIVE, NBR_LIVE_STEMS) ~ PLT_CN, data = cond.table, sum)
if (length(na.omit(cond.table$FIRE_SRS)) != 0){
  cond.fire <- aggregate(FIRE_SRS ~ PLT_CN, data = cond.table, function(x) check.exist(x, 1))
} else {
  cond.fire <- data.frame(PLT_CN=cond.table$PLT_CN,FIRE_SRS=rep(NA,1,length(cond.table$PLT_CN)))
}
tree.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[1], 
                   "/", statecode[1], "_TREE.csv", sep="")
tree.table <- read.csv(tree.file)
names(tree.table)
tree.size <- aggregate(DIA ~ PLT_CN, data = tree.table, mean)
tree.plrg <- aggregate(DIA ~ PLT_CN, data = tree.table, function(x) count.tree(x,8))
if (length(na.omit(tree.table$BHAGE)) != 0){
  tree.pold <- aggregate(BHAGE ~ PLT_CN, data = tree.table, function(x) count.tree(x,80))
} else {
  tree.pold <- data.frame(PLT_CN=tree.table$PLT_CN,BHAGE=rep(NA,1,length(tree.table$PLT_CN)))
}
colnames(tree.plrg)[which(names(tree.plrg) == "DIA")] <- "PT_LRG" # percent of large trees
colnames(tree.pold)[which(names(tree.pold) == "BHAGE")] <- "PT_OLD" # percent of old trees
#tree.size.2 <- aggregate(tree.table$DIA, list(PLT_CN=tree.table$PLT_CN), mean)
#tree.dibage <- aggregate(cbind(DIA, BHAGE) ~ PLT_CN, data=tree.table, mean)
plot.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[1], 
                   "/", statecode[1], "_PLOT.csv", sep="")
plot.table <- read.csv(plot.file)
names(plot.table)
plot.table.1 <- plot.table[c("CN","STATECD","MEASYEAR","MEASMON","MEASDAY","LAT","LON","ELEV")]
colnames(plot.table.1)[which(names(plot.table.1) == "CN")] <- "PLT_CN" 
plot.table.2 <- merge(plot.table.1, cond.sloasp, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.frtcpt, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.stdage, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.balnbr, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.lodgepole, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.ponderosa, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.hosts, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, cond.fire, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, tree.size, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, tree.plrg, by= "PLT_CN", all=TRUE)
plot.table.2 <- merge(plot.table.2, tree.pold, by= "PLT_CN", all=TRUE)

ptm <- proc.time()
for (i in 2:n){
  cond.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[i], 
                     "/", statecode[i], "_COND.csv", sep="")
  cond.table <- read.csv(cond.file)
  cond.stdage <- aggregate(STDAGE ~ PLT_CN, data=cond.table, mean)
  cond.frtcpt <- aggregate(cbind(FLDTYPCD, LIVE_CANOPY_CVR_PCT) ~ PLT_CN, data=cond.table, mode)
  cond.lodgepole <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.exist(x, 281))
  colnames(cond.lodgepole)[which(names(cond.lodgepole) == "FLDTYPCD")] <- "lodgepole"
  cond.ponderosa <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.exist(x, 221))
  colnames(cond.ponderosa)[which(names(cond.ponderosa) == "FLDTYPCD")] <- "ponderosa"
  cond.hosts <- aggregate(FLDTYPCD ~ PLT_CN, data=cond.table, function(x) check.hosts(x))
  colnames(cond.hosts)[which(names(cond.hosts) == "FLDTYPCD")] <- "hosts"
  cond.sloasp <- aggregate(cbind(SLOPE, ASPECT) ~ PLT_CN, data = cond.table, range)
  cond.balnbr <- aggregate(cbind(BALIVE, NBR_LIVE_STEMS) ~ PLT_CN, data = cond.table, sum)
  if (length(na.omit(cond.table$FIRE_SRS)) != 0){
    cond.fire <- aggregate(FIRE_SRS ~ PLT_CN, data = cond.table, function(x) check.exist(x, 1))
  } else {
    cond.fire <- data.frame(PLT_CN=cond.table$PLT_CN,FIRE_SRS=rep(NA,1,length(cond.table$PLT_CN)))
  }
  tree.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[i], 
                     "/", statecode[i], "_TREE.csv", sep="")
  tree.table <- read.csv(tree.file)
  tree.size <- aggregate(DIA ~ PLT_CN, data = tree.table, mean)
  tree.plrg <- aggregate(DIA ~ PLT_CN, data = tree.table, function(x) count.tree(x,8))
  if (length(na.omit(tree.table$BHAGE)) != 0){
    tree.pold <- aggregate(BHAGE ~ PLT_CN, data = tree.table, function(x) count.tree(x,80))
  } else {
    tree.pold <- data.frame(PLT_CN=tree.table$PLT_CN,BHAGE=rep(NA,1,length(tree.table$PLT_CN)))
  }
  colnames(tree.plrg)[which(names(tree.plrg) == "DIA")] <- "PT_LRG" # number of large trees
  colnames(tree.pold)[which(names(tree.pold) == "BHAGE")] <- "PT_OLD" # number of old trees
  plot.file <- paste("/Volumes/dongmeic/beetle/data/text/FIA/westus/", statecode[i], 
                     "/", statecode[i], "_PLOT.csv", sep="")
  plot.table <- read.csv(plot.file)
  plot.table.3 <- plot.table[c("CN","STATECD","MEASYEAR","MEASMON","MEASDAY","LAT","LON","ELEV")]
  colnames(plot.table.3)[which(names(plot.table.3) == "CN")] <- "PLT_CN" 
  plot.table.4 <- merge(plot.table.3, cond.sloasp, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.frtcpt, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.stdage, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.balnbr, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.lodgepole, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.ponderosa, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.hosts, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, cond.fire, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, tree.size, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, tree.plrg, by= "PLT_CN", all=TRUE)
  plot.table.4 <- merge(plot.table.4, tree.pold, by= "PLT_CN", all=TRUE)
  plot.table.2 <- rbind(plot.table.2, plot.table.4)
  print(paste(statecode[i], "done!"))
}
proc.time() - ptm #537.381
# plot.table.3 <- within(plot.table.2, rm(FIRE_SRS)) # OR
# plot.table.3 <- subset(plot.table.2, select=-FIRE_SRS)
# plot.table.3 <- plot.table.2[ ,!names(plot.table.2) %in% "FIRE_SRS"]
plot.table.2$MEASDOY <- yday(ISOdate(plot.table.2$MEASYEAR, plot.table.2$MEASMON, plot.table.2$MEASDAY))
write.csv(plot.table.2, "/Volumes/dongmeic/beetle/data/text/FIA/FIA.csv", row.names = FALSE)

lonlat <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
crs <- CRS("+proj=laea +lon_0=-112.5 +lat_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
states <- readOGR(dsn = "/Volumes/dongmeic/share", layer = "states")
west.usa.shp <- states[states$STATE_NAME %in% c("Washington", "Montana", "North Dakota", "South Dakota","Wyoming", "Idaho", "Oregon", "Nebraska","Colorado","Arizona","Nevada","Utah","California","New Mexico","Kansas", "Oklahoma","Texas"),] # western US 
west.usa.shp.tr <- spTransform(west.usa.shp, crs)
plot.table <- read.csv("/Volumes/dongmeic/beetle/data/text/FIA/FIA.csv")
names(plot.table)
colnames(plot.table)[9:12] <- c("SLOPE_MIN","SLOPE_MAX","ASPECT_MIN","ASPECT_MAX")
plot.table <- plot.table[!is.na(plot.table$LON) & !is.na(plot.table$LAT),]
xy <- data.frame(plot.table[,c(7,6)])
coordinates(xy) <- c("LON", "LAT")
proj4string(xy) <- lonlat
xy.n <- spTransform(xy, crs)
plot.spdf <- SpatialPointsDataFrame(coords = xy.n, data = plot.table, proj4string = crs)
png()
par(mfrow=c(1,1),xpd=FALSE,mar=c(0,0,0,0))
plot(plot.spdf, pch=16, cex=0.1)
plot(west.usa.shp.tr, bord="blue", add=TRUE)
out <- "/Volumes/dongmeic/beetle/data/vector/FIA"
writeOGR(obj=plot.spdf, dsn = out, layer = "FIA", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# only host range
plot.table.host <- plot.table[plot.table$hosts == 1,]
plot.table.host <- plot.table.host[!is.na(plot.table.host$LON) & !is.na(plot.table.host$LAT),]
xy <- data.frame(plot.table.host[,c(7,6)])
coordinates(xy) <- c("LON", "LAT")
proj4string(xy) <- lonlat
xy.n <- spTransform(xy, crs)
plot.spdf <- SpatialPointsDataFrame(coords = xy.n, data = plot.table.host, proj4string = crs)
out <- "/Volumes/dongmeic/beetle/data/vector/FIA"
writeOGR(obj=plot.spdf, dsn = out, layer = "FIA_host", driver = "ESRI Shapefile", overwrite_layer = TRUE)
fia.host <- readOGR(dsn = out, layer = "FIA_host")
names(fia.host)
# [1] "PLT_CN"    "STATECD"   "MEASYEA"   "MEASMON"   "MEASDAY"   "LAT"       "LON"       "ELEV"     
# [9] "SLOPE_MI"  "SLOPE_MA"  "ASPECT_MI" "ASPECT_MA" "FLDTYPC"   "LIVE_CA"   "STDAGE"    "BALIVE"   
# [17] "NBR_LIV"   "lodgepl"   "ponders"   "hosts"     "FIRE_SR"   "DIA"       "PT_LRG"    "PT_OLD"   
# [25] "MEASDOY"   
