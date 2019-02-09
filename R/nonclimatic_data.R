# Collect beetle, tree data (forested area, tree density, stand age, and FIA)
# LANDFIRE, GAP, wilderness 

library(rgdal)
library(dplyr)
library(ggplot2)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

# beetle affected acres
path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
btl.shp <- readOGR(path, "mpb10km_mpb_acres", stringsAsFactors = FALSE)
head(btl.shp@data)
MPBdf <- btl.shp@data[,-3:-1]
#write.csv(MPBdf, paste0(csvpath, "mpb10km_mpb_acres.csv"), row.names=FALSE)

# host presence
host.shp <- readOGR(paste0(path, "/mpb10km"), "mpb10km_corehost", stringsAsFactors = FALSE)
head(host.shp@data)
plot(host.shp[host.shp$vegetation=='1', ], cex=0.5, pch=19, col='blue')
cohost <- rasterized(host.shp, 'vegetation', mode)
FIA_host <- rasterized(host.shp, 'FIA_hosts', mode)
Hostdf <- data.frame(host=extract(cohost, mpb10km.pt, method='simple'), 
										 FIA_host=extract(FIA_host, mpb10km.pt, method='simple'))
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

# location (wrong)
elev <- readOGR(paste0(path, "/mpb10km"), "mpb10km_elev", stringsAsFactors = FALSE)
elev <- elev[elev$mask==1,]
head(elev@data)
write.csv(elev@data, paste0(csvpath, "mpb10km_location.csv"), row.names=FALSE)

# combine all data
df <- cbind(elev@data, MPBdf, Hostdf, LCdf, FIAdf, LFdf, GAPdf) 
df <- df[,-3]; df <- df[,-4]; head(df)
write.csv(df, paste0(csvpath, "mpb10km_nonclimate.csv"), row.names=FALSE)
# need to update from mapping_beetle_affected_acres.R

df <- read.csv(paste0(csvpath, "mpb10km_nonclimate.csv"))

MPB.wild <- df %>%
dplyr::select(wilderness, allyears) %>%
group_by(wilderness) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

MPB.gap <- df %>%
dplyr::select(GAPs, allyears) %>%
group_by(GAPs) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

MPB.mfri <- df %>% 
subset(forest==1 & mfri %in% c(1:22)) %>%
dplyr::select(mfri, allyears) %>%
group_by(mfri) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
mpb.acre.plot <- function(df, outnm, w){
	png(paste0(outpath, "MPB_", outnm, ".png"), width=w, height=9, units="in", res=300)
	par(mfrow=c(3,1),xpd=FALSE,mar=c(3,3,3,0))
	barplot(df$acres, main = "Beetle affected acres", axes = F, cex.main=2)
	axis(2, cex.axis=1.5)
	barplot(df$grids,main = "Number of beetle-affected grid cells",  axes = F, cex.main=2)
	axis(2, cex.axis=1.5)
	barplot(df$average, names.arg = data.frame(df)[,outnm], main = "Beetle affected acres per grid cell", cex.names = 1.6, cex.lab=1.6, cex.axis=1.6, cex.main=2.2)
	dev.off()
}
mpb.acre.plot(MPB.mfri, 'mfri', 12)
mpb.acre.plot(MPB.gap, 'GAPs', 6)

MPB.vcc <- df %>% 
subset(forest==1 & vcc %in% c(1:6)) %>%
dplyr::select(vcc, allyears) %>%
group_by(vcc) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

mpb.acre.plot(MPB.vcc, 'vcc', 6)

MPB.pls <- df %>% 
subset(forest==1 & pls %in% c(1:20)) %>%
dplyr::select(pls, allyears) %>%
group_by(pls) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

mpb.acre.plot(MPB.pls, 'pls', 12)

MPB.pms <- df %>% 
subset(forest==1 & pms %in% c(1:20)) %>%
dplyr::select(pms, allyears) %>%
group_by(pms) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

mpb.acre.plot(MPB.pms, 'pms', 12)

MPB.prs <- df %>% 
subset(forest==1 & prs %in% c(1:20)) %>%
dplyr::select(prs, allyears) %>%
group_by(prs) %>%
summarise(acres = sum(allyears, na.rm=TRUE), grids = sum(!is.na(allyears)), average=sum(allyears, na.rm=TRUE)/sum(!is.na(allyears)))

mpb.acre.plot(MPB.prs, 'prs', 12)

pls <- data.frame(MPB.pls)
colnames(pls)[1] <- 'group'
pls$lf <- rep('pls', dim(pls)[1])
pms <- data.frame(MPB.pms)
colnames(pms)[1] <- 'group'
pms$lf <- rep('pms', dim(pms)[1])
prs <- data.frame(MPB.prs)
colnames(prs)[1] <- 'group'
prs$lf <- rep('prs', dim(prs)[1])

df.lf <- rbind(pls, pms, prs)
write.csv(df.lf, paste0(csvpath, "mpb_acres_LandFire.csv"), row.names=FALSE)

png(paste0(outpath, "MPB_LF_severity.png"), width=8, height=4, units="in", res=300)
ggplot(data=df.lf, aes(x=group, y=average, fill=lf)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(name="Severity", labels=c('Low', 'Mixed', 'Replacement'), palette="YlOrRd")+
  theme_minimal() + labs(x="Percent category of fire severity", y="Beetle affected acres by grid cell")
dev.off()

png(paste0(outpath, "MPB_LF_severity_grid.png"), width=8, height=4, units="in", res=300)
ggplot(data=df.lf, aes(x=group, y=grids, fill=lf)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(name="Severity", labels=c('Low', 'Mixed', 'Replacement'), palette="YlOrRd")+
  theme_minimal() + labs(x="Percent category of fire severity", y="Number of beetle affected grid cells")
dev.off()

png(paste0(outpath, "MPB_LF_severity_acres.png"), width=8, height=4, units="in", res=300)
ggplot(data=df.lf, aes(x=group, y=acres, fill=lf)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(name="Severity", labels=c('Low', 'Mixed', 'Replacement'), palette="YlOrRd")+
  theme_minimal() + labs(x="Percent category of fire severity", y="Total beetle affected acres")
dev.off()