# Collect beetle, tree data (forested area, tree density, stand age, and FIA)
# LANDFIRE, GAP, wilderness 

library(rgdal)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

# beetle affected acres
path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
btl.shp <- readOGR(path, "mpb10km_mpb_acres", stringsAsFactors = FALSE)
head(btl.shp@data)
MPBdf <- btl.shp@data[,-3:-1]
write.csv(MPBdf, paste0(csvpath, "mpb10km_mpb_acres.csv"), row.names=FALSE)

# host presence
host.shp <- readOGR(paste0(path, "/mpb10km"), "mpb10km_corehost", stringsAsFactors = FALSE)
head(host.shp@data)
Hostdf <- host.shp@data[,4:5]
write.csv(Hostdf, paste0(csvpath, "mpb10km_corehost.csv"), row.names=FALSE)

# forested area, GAP, wilderness
landcover <- readOGR(path, "LandCover_mpb10km", stringsAsFactors = FALSE)
head(landcover@data)
LCdf <- landcover@data[,9:10]
colnames(LCdf)[1] <- "type"
write.csv(LCdf, paste0(csvpath, "mpb10km_forested_area.csv"), row.names=FALSE)
gap <- readOGR(dsn=path, layer="mpb10km_fire_protection", stringsAsFactors = FALSE)
GAPdf <- gap@data[,4:9]
write.csv(GAPdf, paste0(csvpath, "mpb10km_forest_protection.csv"), row.names=FALSE)

# tree density and stand age; FIA
stand_age <- readOGR(dsn = paste0(path, "/mpb10km"), layer = "stand_age")
head(stand_age@data)
tree_density <- readOGR(dsn = paste0(path, "/mpb10km"), layer = "tree_density")
head(tree_density@data)
stand_age$RASTERVALU[stand_age$RASTERVALU==-9999] <- NA
tree_density$RASTERVALU[tree_density$RASTERVALU==-9999] <- NA
mean_stand_age <- read.csv(paste0(csvpath, "stand_age_mean.csv"))
FIAdf <- cbind(data.frame(age=stand_age$RASTERVALU, density=tree_density$RASTERVALU), mean_stand_age)
write.csv(FIAdf, paste0(csvpath, "mpb10km_age_density.csv"), row.names=FALSE)

# LandFire
LFpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/LANDFIRE.gdb"
vcc <- readOGR(dsn=paste0(LFpath), layer="VCC", stringsAsFactors = FALSE)
mfri <- readOGR(dsn=paste0(LFpath), layer="MFRI", stringsAsFactors = FALSE)
prs <- readOGR(dsn=paste0(LFpath), layer="PRS", stringsAsFactors = FALSE)
pms <- readOGR(dsn=paste0(LFpath), layer="PMS", stringsAsFactors = FALSE)
pls <- readOGR(dsn=paste0(LFpath), layer="PLS", stringsAsFactors = FALSE)
LFdf <- data.frame(vcc=vcc$RASTERVALU, 
								 mfri=mfri$RASTERVALU, 
								 prs=prs$RASTERVALU, 
								 pms=pms$RASTERVALU,
								 pls=pls$RASTERVALU)
write.csv(LFdf, paste0(csvpath, "mpb10km_landfire.csv"), row.names=FALSE)

# location
elev <- readOGR(paste0(path, "/mpb10km"), "mpb10km_elev", stringsAsFactors = FALSE)
elev <- elev[elev$mask==1,]
head(elev@data)
write.csv(elev@data, paste0(csvpath, "mpb10km_location.csv"), row.names=FALSE)

# combine all data
df <- cbind(elev@data, MPBdf, Hostdf, LCdf, FIAdf, LFdf, GAPdf) 
df <- df[,-3]; df <- df[,-4]; head(df)
write.csv(df, paste0(csvpath, "mpb10km_nonclimate.csv"), row.names=FALSE)

df$allyears <- as.numeric(df$allyears)
MPB.gap <- df %>%
select(GAPs, allyears) %>%
group_by(GAPs) %>%
summarise(acres = sum(allyears), grids = sum(allyears>0), average=sum(allyears)/sum(allyears>0))
