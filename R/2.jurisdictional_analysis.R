## Dongmei CHEN
# objectives: jurisdictional analysis
# reference: 1. /Users/dongmeichen/Documents/scripts/beetle/r/fire_suppression/jurisdictional_v2.R;
#            2. /Users/dongmeichen/GitHub/suppression/figures_maps_regression.R;
#            3. /Users/dongmeichen/GitHub/suppression/regression.R
# input: 1. spatial join data in /Volumes/dongmeic/beetle/data/vector/spatial_join;
#        2. /Users/dongmeichen/Documents/writing/fire suppression/output/v4/spdf/fpa_sit_wus_spdf.shp;

# output: data frames or spatial data frames

# libraries
library(rgdal)
library(gvlma)

#setwd("/Users/dongmeichen/Documents/writing/fire suppression/output/v4/maps")
load("~/Documents/writing/fire suppression/scripts/data_analysis.RData")

# data needed: fpa.sit.spdf, mpb.pts

# national parks
parks <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer= "nps_join_dist", stringsAsFactors = FALSE)
parks.dens <- readOGR(dsn ="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "nps_join_dens", stringsAsFactors = FALSE)
length(parks.dens$TARGET_FID); length(unique(parks.dens$TARGET_FID))
parks.dens.df <- parks.dens@data[c("Join_Count", "TARGET_FID")]
parks.dens.df$Join_Count <- as.numeric(parks.dens.df$Join_Count)
parks@data <- merge(parks@data, parks.dens.df, by="TARGET_FID", all=TRUE)
names(parks@data)[which(names(parks@data)=="Join_Count.y")] = "dens"
fpa.sit.join <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "fpa_sit_dist", stringsAsFactors = FALSE)
fpa.sit.join$parks <- over(fpa.sit.join, parks)$TARGET_FID
burnedArea.in.parks <- aggregate(Acres ~ parks, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.parks <- aggregate(Duration ~ parks, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.parks <- aggregate(Costs ~ parks, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.parks <- aggregate(FIRE_SIZE ~ parks, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.parks <- aggregate(dist ~ parks, data=fpa.sit.join, mean, na.rm=TRUE)
proj4string(mpb.pts) <- proj4string(parks)
mpb.pts$parks <- over(mpb.pts, parks)$TARGET_FID
mpb.in.parks <- aggregate(ACRES ~ parks, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.parks)[which(names(mpb.in.parks) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.parks, fireDuration.in.parks, by= "parks", all=TRUE)
df.2 <- merge(costEst.in.parks, fireSize.in.parks, by= "parks", all=TRUE)
df.3 <- merge(dist.in.parks, mpb.in.parks, by= "parks", all=TRUE)
df <- merge(df.1, df.2, by= "parks", all=TRUE)
df <- merge(df, df.3, by= "parks", all=TRUE)
colnames(df)[which(names(df) == "parks")] <- "TARGET_FID"
parks.m <- merge(parks, df, by="TARGET_FID", all=TRUE)
parks.m <- parks.m[!is.na(parks.m$Costs),]

# native forests
forests <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer= "forests_join_dist", stringsAsFactors = FALSE)
forests.dens <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "forests_join_dens", stringsAsFactors = FALSE)
length(forests.dens$TARGET_FID); length(unique(forests.dens$TARGET_FID))
forests.dens.df <- forests.dens@data[c("Join_Count", "TARGET_FID")]
forests.dens.df$Join_Count <- as.numeric(forests.dens.df$Join_Count)
forests@data <- merge(forests@data, forests.dens.df, by="TARGET_FID", all=TRUE)
names(forests@data)[which(names(forests@data)=="Join_Count.y")] = "dens"

fpa.sit.join$forests <- over(fpa.sit.join, forests)$TARGET_FID
burnedArea.in.forests <- aggregate(Acres ~ forests, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.forests <- aggregate(Duration ~ forests, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.forests <- aggregate(Costs ~ forests, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.forests <- aggregate(FIRE_SIZE ~ forests, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.forests <- aggregate(dist ~ forests, data=fpa.sit.join, mean, na.rm=TRUE)
mpb.pts$forests <- over(mpb.pts, forests)$TARGET_FID
mpb.in.forests <- aggregate(ACRES ~ forests, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.forests)[which(names(mpb.in.forests) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.forests, fireDuration.in.forests, by= "forests", all=TRUE)
df.2 <- merge(costEst.in.forests, fireSize.in.forests, by= "forests", all=TRUE)
df.3 <- merge(dist.in.forests, mpb.in.forests, by= "forests", all=TRUE)
df <- merge(df.1, df.2, by= "forests", all=TRUE)
df <- merge(df, df.3, by= "forests", all=TRUE)
colnames(df)[which(names(df) == "forests")] <- "TARGET_FID"
forests.m <- merge(forests, df, by="TARGET_FID", all=TRUE)
forests.m <- forests.m[!is.na(forests.m$Costs),]

# wildness area
wildness <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer= "wildness_join_dist", stringsAsFactors = FALSE)
wildness.dens <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "wildness_join_dens", stringsAsFactors = FALSE)
length(wildness.dens$TARGET_FID); length(unique(wildness.dens$TARGET_FID))
wildness.dens.df <- wildness.dens@data[c("Join_Count", "TARGET_FID")]
wildness.dens.df$Join_Count <- as.numeric(wildness.dens.df$Join_Count)
wildness@data <- merge(wildness@data, wildness.dens.df, by="TARGET_FID", all=TRUE)
names(wildness@data)[which(names(wildness@data)=="Join_Count.y")] = "dens"

fpa.sit.join$wildness <- over(fpa.sit.join, wildness)$TARGET_FID
burnedArea.in.wildness <- aggregate(Acres ~ wildness, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.wildness <- aggregate(Duration ~ wildness, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.wildness <- aggregate(Costs ~ wildness, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.wildness <- aggregate(FIRE_SIZE ~ wildness, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.wildness <- aggregate(dist ~ wildness, data=fpa.sit.join, mean, na.rm=TRUE)
mpb.pts$wildness <- over(mpb.pts, wildness)$TARGET_FID
mpb.in.wildness <- aggregate(ACRES ~ wildness, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.wildness)[which(names(mpb.in.wildness) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.wildness, fireDuration.in.wildness, by= "wildness", all=TRUE)
df.2 <- merge(costEst.in.wildness, fireSize.in.wildness, by= "wildness", all=TRUE)
df.3 <- merge(dist.in.wildness, mpb.in.wildness, by= "wildness", all=TRUE)
df <- merge(df.1, df.2, by= "wildness", all=TRUE)
df <- merge(df, df.3, by= "wildness", all=TRUE)
colnames(df)[which(names(df) == "wildness")] <- "TARGET_FID"
wildness.m <- merge(wildness, df, by="TARGET_FID", all=TRUE)
wildness.m <- wildness.m[!is.na(wildness.m$Costs),]

# ecological sections
ecosect <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer= "ecos_join_dist", stringsAsFactors = FALSE)
ecosect.dens <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "ecos_join_dens", stringsAsFactors = FALSE)
length(ecosect.dens$TARGET_FID); length(unique(ecosect.dens$TARGET_FID))
ecosect.dens.df <- ecosect.dens@data[c("Join_Count", "TARGET_FID")]
ecosect.dens.df$Join_Count <- as.numeric(ecosect.dens.df$Join_Count)
ecosect@data <- merge(ecosect@data, ecosect.dens.df, by="TARGET_FID", all=TRUE)
names(ecosect@data)[which(names(ecosect@data)=="Join_Count.y")] = "dens"

fpa.sit.join$ecosect <- over(fpa.sit.join, ecosect)$TARGET_FID
burnedArea.in.ecosect <- aggregate(Acres ~ ecosect, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.ecosect <- aggregate(Duration ~ ecosect, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.ecosect <- aggregate(Costs ~ ecosect, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.ecosect <- aggregate(FIRE_SIZE ~ ecosect, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.ecosect <- aggregate(dist ~ ecosect, data=fpa.sit.join, mean, na.rm=TRUE)
mpb.pts$ecosect <- over(mpb.pts, ecosect)$TARGET_FID
mpb.in.ecosect <- aggregate(ACRES ~ ecosect, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.ecosect)[which(names(mpb.in.ecosect) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.ecosect, fireDuration.in.ecosect, by= "ecosect", all=TRUE)
df.2 <- merge(costEst.in.ecosect, fireSize.in.ecosect, by= "ecosect", all=TRUE)
df.3 <- merge(dist.in.ecosect, mpb.in.ecosect, by= "ecosect", all=TRUE)
df <- merge(df.1, df.2, by= "ecosect", all=TRUE)
df <- merge(df, df.3, by= "ecosect", all=TRUE)
colnames(df)[which(names(df) == "ecosect")] <- "TARGET_FID"
ecosect.m <- merge(ecosect, df, by="TARGET_FID", all=TRUE)
ecosect.m <- ecosect.m[!is.na(ecosect.m$Costs),]

# protected area
protected <- readOGR(dsn = "/Volumes/dongmeic/beetle/data/vector/spatial_join", layer= "protected_join_dist", stringsAsFactors = FALSE)
protected.dens <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "protected_join_dens", stringsAsFactors = FALSE)
protected.mpb <- readOGR(dsn="/Volumes/dongmeic/9.fall_2017/data", layer="protected_mpb", stringsAsFactors = FALSE)
length(protected.dens$TARGET_FID); length(unique(protected.dens$TARGET_FID))
protected.dens.df <- protected.dens@data[c("Join_Count", "TARGET_FID")]
protected.mpb.df <- protected.mpb@data[c("sum_mpb", "TARGET_FID")]
protected.dens.df$Join_Count <- as.numeric(protected.dens.df$Join_Count)
protected@data <- merge(protected@data, protected.dens.df, by="TARGET_FID", all=TRUE)
protected@data <- merge(protected@data, protected.mpb.df, by="TARGET_FID", all=TRUE)
names(protected@data)[which(names(protected@data)=="Join_Count.y")] = "dens"

fpa.sit.join$protected <- over(fpa.sit.join, protected)$TARGET_FID
burnedArea.in.protected <- aggregate(Acres ~ protected, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.protected <- aggregate(Duration ~ protected, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.protected <- aggregate(Costs ~ protected, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.protected <- aggregate(FIRE_SIZE ~ protected, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.protected <- aggregate(dist ~ protected, data=fpa.sit.join, mean, na.rm=TRUE)

df.1 <- merge(burnedArea.in.protected, fireDuration.in.protected, by= "protected", all=TRUE)
df.2 <- merge(costEst.in.protected, fireSize.in.protected, by= "protected", all=TRUE)
df <- merge(df.1, df.2, by= "protected", all=TRUE)
df <- merge(df, dist.in.protected, by= "protected", all=TRUE)
colnames(df)[which(names(df) == "protected")] <- "TARGET_FID"
protected.m <- merge(protected, df, by="TARGET_FID", all=TRUE)
protected.m <- protected.m[!is.na(protected.m$Costs),]

# roadless
roadless <- readOGR(dsn="/Volumes/dongmeic/beetle/data/vector/spatial_join", layer = "roadless_join_dens", stringsAsFactors = FALSE)
length(roadless.dens$TARGET_FID); length(unique(roadless.dens$TARGET_FID))

fpa.sit.join$roadless <- over(fpa.sit.join, roadless)$TARGET_FID
burnedArea.in.roadless <- aggregate(Acres ~ roadless, data=fpa.sit.join, sum, na.rm=TRUE)
fireDuration.in.roadless <- aggregate(Duration ~ roadless, data=fpa.sit.join, mean, na.rm=TRUE)
costEst.in.roadless <- aggregate(Costs ~ roadless, data=fpa.sit.join, sum, na.rm=TRUE)
fireSize.in.roadless <- aggregate(FIRE_SIZE ~ roadless, data=fpa.sit.join, mean, na.rm=TRUE)
dist.in.roadless <- aggregate(dist ~ roadless, data=fpa.sit.join, mean, na.rm=TRUE)
mpb.pts$roadless <- over(mpb.pts, roadless)$TARGET_FID
mpb.in.roadless <- aggregate(ACRES ~ roadless, data=mpb.pts, sum, na.rm=TRUE)
colnames(mpb.in.roadless)[which(names(mpb.in.roadless) == "ACRES")] <- "MPB_ACRES"

df.1 <- merge(burnedArea.in.roadless, fireDuration.in.roadless, by= "roadless", all=TRUE)
df.2 <- merge(costEst.in.roadless, fireSize.in.roadless, by= "roadless", all=TRUE)
df.3 <- merge(dist.in.roadless, mpb.in.roadless, by= "roadless", all=TRUE)
df <- merge(df.1, df.2, by= "roadless", all=TRUE)
df <- merge(df, df.3, by= "roadless", all=TRUE)
colnames(df)[which(names(df) == "roadless")] <- "TARGET_FID"
roadless.m <- merge(roadless, df, by="TARGET_FID", all=TRUE)
roadless.m <- roadless.m[!is.na(roadless.m$Costs),]

# regression
varlist <- c("Acres","Duration","Costs","dens","FIRE_SIZE","dist.y","MPB_ACRES")
parks.m.df <- as.data.frame(parks.m)
parks.df <- parks.m.df[varlist]
colnames(parks.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
parks.df$LogBA <- log(parks.df$Burn_acres)
parks.df$LogCost <- log(parks.df$Costs)
parks.df$LogMPB <- log(parks.df$MPB_acres)
parks.df$LogSize <- log(parks.df$Fire_size)
parks.df$LogDens <- log(parks.df$Density)
parks.df$LogDist <- log(parks.df$Distance)
parks.df <- parks.df[parks.df$LogBA != -Inf & 
                       parks.df$LogCost != -Inf & 
                       parks.df$LogDist != -Inf &
                       parks.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
parks.df <- parks.df[varlist.n]
parks.df <- na.omit(parks.df)
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=parks.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=parks.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=parks.df)
summary(lr)
cor.test(parks.df$MPB_ACRES, parks.df$dens)

forests.m.df <- as.data.frame(forests.m)
forests.df <- forests.m.df[varlist]
colnames(forests.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
forests.df$LogBA <- log(forests.df$Burn_acres)
forests.df$LogCost <- log(forests.df$Costs)
forests.df$LogMPB <- log(forests.df$MPB_acres)
forests.df$LogSize <- log(forests.df$Fire_size)
forests.df$LogDens <- log(forests.df$Density)
forests.df$LogDist <- log(forests.df$Distance)
forests.df <- forests.df[forests.df$LogBA != -Inf & 
                       forests.df$LogCost != -Inf & 
                       forests.df$LogDist != -Inf &
                       forests.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
forests.df <- forests.df[varlist.n]
forests.df <- na.omit(forests.df)
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=forests.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=forests.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=forests.df)
summary(lr)
cor.test(forests.df$MPB_ACRES, forests.df$dens)

wildness.m.df <- as.data.frame(wildness.m)
wildness.df <- wildness.m.df[varlist]
colnames(wildness.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
wildness.df$LogBA <- log(wildness.df$Burn_acres)
wildness.df$LogCost <- log(wildness.df$Costs)
wildness.df$LogMPB <- log(wildness.df$MPB_acres)
wildness.df$LogSize <- log(wildness.df$Fire_size)
wildness.df$LogDens <- log(wildness.df$Density)
wildness.df$LogDist <- log(wildness.df$Distance)
wildness.df <- wildness.df[wildness.df$LogBA != -Inf & 
                       wildness.df$LogCost != -Inf & 
                       wildness.df$LogDist != -Inf &
                       wildness.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
wildness.df <- wildness.df[varlist.n]
wildness.df <- na.omit(wildness.df)
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=wildness.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=wildness.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=wildness.df)
summary(lr)
cor.test(wildness.df$MPB_ACRES, wildness.df$dens)

ecosect.m.df <- as.data.frame(ecosect.m)
ecosect.df <- ecosect.m.df[varlist]
colnames(ecosect.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
ecosect.df$LogBA <- log(ecosect.df$Burn_acres)
ecosect.df$LogCost <- log(ecosect.df$Costs)
ecosect.df$LogMPB <- log(ecosect.df$MPB_acres)
ecosect.df$LogSize <- log(ecosect.df$Fire_size)
ecosect.df$LogDens <- log(ecosect.df$Density)
ecosect.df$LogDist <- log(ecosect.df$Distance)
ecosect.df <- ecosect.df[ecosect.df$LogBA != -Inf & 
                       ecosect.df$LogCost != -Inf & 
                       ecosect.df$LogDist != -Inf &
                       ecosect.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
ecosect.df <- ecosect.df[varlist.n]
ecosect.df <- na.omit(ecosect.df)
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=ecosect.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=ecosect.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=ecosect.df)
summary(lr)
cor.test(ecosect.df$MPB_ACRES, ecosect.df$dens)

protected.m.df <- as.data.frame(protected.m)
colnames(protected.m.df)[which(colnames(protected.m.df)=="sum_mpb")] <- "MPB_ACRES"
protected.df <- protected.m.df[varlist]
colnames(protected.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
protected.df$LogBA <- log(protected.df$Burn_acres)
protected.df$LogCost <- log(protected.df$Costs)
protected.df$LogMPB <- log(protected.df$MPB_acres)
protected.df$LogSize <- log(protected.df$Fire_size)
protected.df$LogDens <- log(protected.df$Density)
protected.df$LogDist <- log(protected.df$Distance)
protected.df <- protected.df[protected.df$LogBA != -Inf & 
                           protected.df$LogCost != -Inf & 
                           protected.df$LogDist != -Inf &
                           protected.df$LogMPB != -Inf &
                           protected.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
protected.df <- protected.df[varlist.n]
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=protected.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=protected.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=protected.df)
summary(lr)
cor.test(protected.df$MPB_ACRES, protected.df$dens)

roadless.m.df <- as.data.frame(roadless.m)
colnames(roadless.m.df)[which(colnames(roadless.m.df)=="Join_Count")] <- "dens"
colnames(roadless.m.df)[which(colnames(roadless.m.df)=="dist")] <- "dist.y"
roadless.df <- roadless.m.df[varlist]
roadless.df$dens <- as.numeric(roadless.df$dens)
colnames(roadless.df) <- c("Burn_acres", "Duration", "Costs", "Density", "Fire_size", "Distance", "MPB_acres")
roadless.df$LogBA <- log(roadless.df$Burn_acres)
roadless.df$LogCost <- log(roadless.df$Costs)
roadless.df$LogMPB <- log(roadless.df$MPB_acres)
roadless.df$LogSize <- log(roadless.df$Fire_size)
roadless.df$LogDens <- log(roadless.df$Density)
roadless.df$LogDist <- log(roadless.df$Distance)
roadless.df <- roadless.df[roadless.df$LogBA != -Inf & 
                           roadless.df$LogCost != -Inf & 
                           roadless.df$LogDist != -Inf &
                           roadless.df$LogDens != -Inf,]
varlist.n <- c("LogMPB", "LogCost", "LogSize", "LogBA", "Duration", "LogDens", "LogDist", "MPB_acres", "Costs", "Fire_size", "Burn_acres", "Density", "Distance")
roadless.df <- roadless.df[varlist.n]
roadless.df <- na.omit(roadless.df)
mlr <- lm(MPB_acres ~ Costs + Duration + Burn_acres + Fire_size + Density + Distance, data=roadless.df)
mlr <- lm(LogMPB ~ LogCost + Duration + LogBA + LogSize + LogDens + LogDist, data=roadless.df)
summary(mlr)
layout(matrix(c(1,2,3,4),2,2))
print(plot(mlr, cex=0.2))
gvmodel <- gvlma(mlr) 
print(summary(gvmodel))
step <- stepAIC(mlr, direction="both")
print(step$anova)
lr <- lm(MPB_ACRES ~ Costs, data=roadless.df)
summary(lr)
cor.test(roadless.df$MPB_ACRES, roadless.df$dens)

parks.df$Jurisd <- "Parks"
forests.df$Jurisd <- "Forests"
wildness.df$Jurisd <- "Wildness"
roadless.df$Jurisd <- "Roadless"
protected.df$Jurisd <- "Protected"
ecosect.df$Jurisd <- "Ecosections"
ndf <- rbind(parks.df, forests.df, wildness.df, roadless.df, ecosect.df)
write.csv(ndf, "/Users/dongmeichen/Documents/writing/fire suppression/output/v4/df/jurisditional_scaling_point_df.csv", row.names = FALSE)

save.image("~/Documents/writing/fire suppression/scripts/data_analysis.RData")
