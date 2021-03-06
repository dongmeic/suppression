library(rasterVis)
library(colorRamps)
library(BAMMtools)
library(RColorBrewer)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
indata <- read.csv(paste0(csvpath, 'mpb10km_data.csv'))
#df <- subset(indata, !is.na(beetleAcres))

# vcc values > 6 indicate areas of no vegetation, replace with 0s
indata$vcc[indata$vcc > 6] <- 0
indata$prs[indata$prs > 20] <- 0
indata$pms[indata$pms > 20] <- 0
indata$pls[indata$pls > 20] <- 0

# Mean fire return interval where there are no trees is effectively 
# infinite--set to 22 (> 1000 years)
indata$mfri[indata$mfri > 22] <- 0

indata$beetleAcres[is.na(indata$beetleAcres)] <- 0
indata$mStdAge[is.na(indata$mStdAge) & indata$forest == 0] <- 0
indata$density[is.na(indata$density) & indata$forest == 0] <- 0
indata$PctLarge[is.na(indata$PctLarge) & indata$forest == 0] <- 0
indata$PctOld[is.na(indata$PctOld) & indata$forest == 0] <- 0

indata$SprsCosts[is.na(indata$SprsCosts) & indata$forest == 0] <- 0
indata$SprsAcres[is.na(indata$SprsAcres) & indata$forest == 0] <- 0
indata$SprsCPA[is.na(indata$SprsCPA) & indata$forest == 0] <- 0
indata$SprsFires[is.na(indata$SprsFires) & indata$forest == 0] <- 0
indata$PctSprs[is.na(indata$PctSprs) & indata$forest == 0] <- 0
indata$SprsAcre[is.na(indata$SprsAcre) & indata$forest == 0] <- 0
indata$SprsDays[is.na(indata$SprsDays) & indata$forest == 0] <- 0
indata$OutDays[is.na(indata$OutDays) & indata$forest == 0] <- 0

GAPs <- read.csv(paste0(csvpath, 'mpb10km_GAPs.csv'))
indata <- cbind(indata, GAPs)
indata$GAPs[is.na(indata$GAPs) & indata$forest == 0] <- 0
df <- subset(indata, beetleAcres > 0)
spdf <- df2spdf(1, 2, 'lon', 'lat', df)

mapping.LF <- function(shp, var){	
	if(var %in% c('pls', 'pms', 'prs')){
		labels <- c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30",
						"31-35", "36-40", "41-45", "46-50", "51-55", "56-60",
						"61-65", "66-70", "71-75", "76-80", "81-85", "86-90", "91-95", "96-100")
		if(var == 'pls'){
			title <- "Percent of low-severity fires (%)"
		}else if(var == 'pms'){
			title <- "Percent of mixed-severity fires (%)"
		}else{
			title <- "Percent of replacement-severity fires (%)"
			labels <- c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30",
						"31-35", "36-40", "41-45", "46-50", "51-55", "56-60",
						"61-65", "66-70", "71-75", "76-80", "81-85", "86-90", "96-100")
		}
		cols <- matlab.like(20)
	}else if(var == 'mfri'){
		labels <- c("6-10", "11-15", "16-20", "21-25", "26-30",
						"31-35", "36-40", "41-45", "46-50", "51-60", "61-70",
						"71-80", "81-90", "91-100", "101-125", "126-150", "151-200",
						"201-300", "301-500", "501-1000", ">1000")
		cols <- rev(matlab.like(22))
		title <- "Mean fire return interval (Years)"
	}else if(var == 'vcc'){
		labels <- c("0-16", "17-33", "34-50", "51-66", "67-83", "84-100")
		cols <- brewer.pal(7,'BuGn')[-1]
		title <- "Vegetation condition class (%)"
	}
	shp <- shp[shp@data[,var] > 0,]
	r <- rasterize(shp, mpb10km.pts.r, var, fun=mean, na.rm=TRUE) 
	r <- as.factor(r)
	rat <- levels(r)[[1]]
	rat[["labels"]] <- labels
	levels(r) <- rat
	p <- levelplot(r, col.regions=cols, xlab="", ylab="",par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

mapping.sprs <- function(shp, var, title, cols="YlOrRd"){	
	shp <- shp[shp@data[,var] != Inf & !is.na(shp@data[,var]),]
	qt99 <- quantile(shp@data[,var], 0.99)
	shp <- shp[shp@data[,var] <= qt99,]
	r <- rasterize(shp, mpb10km.pts.r, var, fun=mean, na.rm=TRUE)
	ncls <- 6
	brks <- getJenksBreaks(getValues(r), ncls)
	#print(brks)
	p <- levelplot(r, col.regions=brewer.pal(ncls,cols)[-1], cuts=ncls-1, at=brks, xlab="", ylab="", par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

mapping.btl <- function(shp, var, title='MPB affected acres', cols="YlOrRd"){
	r <- rasterize(shp, mpb10km.pts.r, var, fun=mean, na.rm=TRUE)
	ncls <- 6
	brks <- getJenksBreaks(getValues(r), ncls+1)
	p <- levelplot(r, col.regions=brewer.pal(ncls,cols), cuts=ncls, at=brks, xlab="", ylab="", par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

mapping.tree <- function(shp, var, title, cols="PuBuGn"){
	r <- rasterize(shp, mpb10km.pts.r, var, fun=mean, na.rm=TRUE)
	ncls <- 6
	brks <- getJenksBreaks(getValues(r), ncls)
	if(var == 'host' | var == 'forest'){
		labels <- c("No", "Yes")
		cols <- c("#d0d1e6", "#016c59")
		r <- as.factor(r)
		rat <- levels(r)[[1]]
		rat[["labels"]] <- labels
		levels(r) <- rat
		p <- levelplot(r, col.regions=cols, xlab="", ylab="",par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	}else{
		p <- levelplot(r, col.regions=brewer.pal(ncls,cols)[-1], cuts=ncls-1, at=brks, xlab="", ylab="", par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	}
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

mapping.GAPs <- function(shp, var, title){
	r <- rasterize(shp, mpb10km.pts.r, var, fun=mean, na.rm=TRUE)
	r <- as.factor(r)
	rat <- levels(r)[[1]]
	rat[["labels"]] <- c('0', '1', '2', '3', '4')
	levels(r) <- rat
	p <- levelplot(r, col.regions=c('Grey', brewer.pal(4,'Set1')), xlab="", ylab="",
						par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

# output figures
# tree maps
vars <- c("mStdAge", "density", "PctLarge", "PctOld", "host", 'beetleAcres')
titles <- c('Stand age (Years)', 'Tree density (No. trees per acre)', 'Ratio of large trees', 
						'Ratio of old trees', 'MPB core host presence', 'Beetle affected acres')
pos <- cbind(c(1,1),c(1,2),c(1,3),
						 c(2,1),c(2,2),c(2,3))
png(paste0(out,'stand_variable_maps_0.png'), width=12, height=8, units="in", res=300)
par(mfrow=c(2,3), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	if(var != 'beetleAcres'){
		p <- mapping.tree(spdf, var, titles[which(vars==var)])
	}else{
		p <- mapping.btl(spdf, var)
	}
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 3, 2), newpage=FALSE) 
	print(var)
}
dev.off()

# fire regime maps
vars <- c("vcc", "mfri", "prs", "pms", "pls", 'beetleAcres')
png(paste0(out,'fire_regime_variable_maps_0.png'), width=12, height=8, units="in", res=300)
par(mfrow=c(2,3), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	if(var != 'beetleAcres'){
		p <- mapping.LF(spdf, var)
	}else{
		p <- mapping.btl(spdf, var)
	}
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 3, 2), newpage=FALSE) 
	print(var)
}
dev.off()
# fire data maps
vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 
					'SprsAcre', 'SprsDays', 'OutDays', 'beetleAcres')
titles <- c('Suppression costs ($)', 'Suppression acres', 'Suppression costs per acre ($)', 
						'No. of fires suppressed', 'Ratio of suppressed fires',
						'Mean fire size (Acres)', 'Containment duration (Days)', 'Fire out duration (Days)')

pos <- cbind(c(1,1),c(1,2),c(1,3),
						 c(2,1),c(2,2),c(2,3),
						 c(3,1),c(3,2),c(3,3))
						 
png(paste0(out,'suppression_variable_maps_0.png'), width=12, height=12, units="in", res=300)
par(mfrow=c(3,3), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	if(var != 'beetleAcres'){
		p <- mapping.sprs(spdf, var, titles[which(vars==var)])
	}else{
		p <- mapping.btl(spdf, var)
	}
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 3, 3), newpage=FALSE) 
	print(var)
}
dev.off()

# fire severity map
df$severity <- ifelse(df$prs >= 17 & df$mfri >= 16, 'replacement', ifelse(df$pls >= 17 & df$mfri <= 4, 'low', 'mixed'))
df$severity.no <- ifelse(df$severity == 'replacement', 3, ifelse(df$severity == 'low', 1, 2))
spdf <- df2spdf(1, 2, 'lon', 'lat', df)

mapping.severity <- function(){
	labels <- c("L", "M", "R")
	cols <- c("#e41a1c", "#4daf4a", "#377eb8")
	r <- rasterize(spdf, mpb10km.pts.r, "severity.no", fun=mean, na.rm=TRUE)
	r <- as.factor(r)
	rat <- levels(r)[[1]]
	rat[["labels"]] <- labels
	levels(r) <- rat
	title <- "Fire severity"
	p <- levelplot(r, col.regions=cols, xlab="", ylab="",par.settings = list(axis.line = list(col = "transparent")), 
					scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	return(p)
}

png(paste0(out, "fire_severity_0.png"), width = 8, height = 8, units = "in", res=300)
#print(p)
mapping.severity()
dev.off()

# reorganize the plots
# tree maps
vars <- c("mStdAge", "density", "PctLarge", "PctOld", "forest", 'GAPs')
titles <- c('Stand age (Years)', 'Tree density (No. trees per acre)', 'Ratio of large trees', 
						'Ratio of old trees', 'Forested area', 'GAP status')
pos <- cbind(c(1,1),c(1,2),c(1,3),
						 c(2,1),c(2,2),c(2,3))
png(paste0(out,'stand_variable_maps_0.png'), width=12, height=8, units="in", res=300)
par(mfrow=c(2,3), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	if(var != 'GAPs'){
		p <- mapping.tree(spdf, var, titles[which(vars==var)])
	}else{
		p <- mapping.GAPs(spdf, var, 'GAP status')
	}
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 3, 2), newpage=FALSE) 
	print(var)
}
dev.off()

# fire regime maps
vars <- c("vcc", "mfri", "prs", "pms", "pls", "severity.no")
pos <- cbind(c(1,1),c(1,2),c(1,3),
						 c(2,1),c(2,2),c(2,3))
png(paste0(out,'fire_regime_variable_maps_0.png'), width=12, height=8, units="in", res=300)
par(mfrow=c(2,3), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	if(var != 'severity.no'){
		p <- mapping.LF(spdf, var)
	}else{
		p <- mapping.severity()
	}
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 3, 2), newpage=FALSE) 
	print(var)
}
dev.off()
# fire data maps
vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 
					'SprsAcre', 'SprsDays', 'OutDays')
titles <- c('Suppression costs ($)', 'Suppression acres', 'Suppression costs per acre ($)', 
						'No. of fires suppressed', 'Ratio of suppressed fires',
						'Mean fire size (Acres)', 'Containment duration (Days)', 'Fire out duration (Days)')

pos <- cbind(c(1,1),c(1,2),c(1,3),c(1,4),
						 c(2,1),c(2,2),c(2,3),c(2,4))
						 
png(paste0(out,'suppression_variable_maps_0.png'), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4), xpd=FALSE, mar=rep(0.5,4))
for(var in vars){
	p <- mapping.sprs(spdf, var, titles[which(vars==var)])
	print(p,split=c(pos[,which(vars==var)][2], pos[,which(vars==var)][1], 4, 2), newpage=FALSE) 
	print(var)
}
dev.off()