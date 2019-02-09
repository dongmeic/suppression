source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
indata <- read.csv(paste0(csvpath, 'mpb10km_input_data.csv')) # from interactions.R
costs.df <- read.csv(paste0(csvpath, 'suppressed_costs.csv')) # from suppressed_fires.R
fires.df <- read.csv(paste0(csvpath, 'suppressed_fires.csv')) # from suppressed_fires.R
df <- cbind(indata, costs.df, fires.df)
write.csv(df, paste0(csvpath, 'mpb10km_data.csv'), row.names=FALSE)

# total number of grid cells
n <- 36965

# number of grid cells with beetle affected acres
df.btl <- df[!is.na(df$beetleAcres),]
n.btl <- dim(df.btl)[1]
p.btl <- n.btl/n # 0.2422291

# beetle-affected grid cells in forested area
df.btl.ft <- subset(df.btl, forest==1)
p.btl.ft <- dim(df.btl.ft)[1]/n.btl # 0.5337279
plot(df.btl.ft$lon, df.btl.ft$lat, cex=0.5, col='blue', pch=19)

# number of grid cells with fire suppression costs
df.cost <- df[!is.na(df$SprsCosts),]
dim(df.cost)[1]/n # 0.1292304

# number of grid cells with suppressed fires
df.sprs <- df[!is.na(df$SprsFires),]
dim(df.sprs)[1]/n # 0.3753821

# number of grid cells with both suppression costs and suppressed fires
df.sprs.cost <- df[!is.na(df$SprsCosts) & !is.na(df$SprsFires),]
dim(df.sprs.cost)[1]/n # 0.07774922

# number of grid cells with host presence
df.host <- subset(df, host==1)
dim(df.host)[1]/n # 0.4115515
plot(df.host$lon, df.host$lat, cex=0.5, col='blue', pch=19)

# number of grid cells with both beetle and host
df.host.btl <- subset(df, host==1 & !is.na(beetleAcres))
dim(df.host.btl)[1]/n # 0.2245367
dim(df.host.btl)[1]/n.btl # 0.92696
plot(df.host.btl$lon, df.host.btl$lat, cex=0.5, col='blue', pch=19)
plot(df.host$lon, df.host$lat, cex=0.5, col='blue', pch=19)

# examine dataset
spdf <- df2spdf(1, 2, 'lon', 'lat', df)
plot(spdf[!is.na(spdf$beetleAcres),], cex=0.05, pch=19, col='blue')
Tmin <- rasterized(spdf, 'Tmin', mean)
plot(Tmin)
#plot(spdf[!is.na(spdf$beetleAcres),], cex=0.05, pch=19, col='blue', add=T)
plot(spdf[spdf$forest==1,], cex=0.05, pch=19, col='green') # forest
plot(spdf[!(spdf$vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)),], cex=0.05, pch=19, col='grey')

plot(spdf[!is.na(spdf$SprsCosts),], cex=0.05, pch=19, col='red')
plot(spdf[!is.na(spdf$SprsFires),], cex=0.05, pch=19, col='red')
plot(spdf[!is.na(spdf$mStdAge),], cex=0.05, pch=19, col='green')

# compare number of grids with host presence and beetle affected acres in different fire regimes
df.btl <- df[!is.na(df$beetleAcres) & df$host == 1 & 
						 !(df$vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
						 !(df$mfri %in% c(111, 112, 131, 132, 133)) &
						 !(df$prs %in% c(111, 112, 131, 132)) &
						 !(df$pms %in% c(111, 112, 131, 132)) &
						 !(df$pls %in% c(111, 112, 131, 132)),]

