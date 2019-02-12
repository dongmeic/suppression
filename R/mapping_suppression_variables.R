library(rasterVis)
library(colorRamps)
library(BAMMtools)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
indata <- read.csv(paste0(csvpath, 'mpb10km_data.csv'))
df <- subset(indata, !is.na(beetleAcres))

df$vcc[df$vcc > 6] <- 0
df$prs[df$prs > 20] <- 0
df$pms[df$pms > 20] <- 0
df$pls[df$pls > 20] <- 0
df$mfri[df$mfri > 22] <- 0

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

shp <- spdf

var <- 'beetleAcres'
title <- 'MPB affected acres'

mapping.LF(shp, 'vcc')
mapping.LF(shp, 'mfri')
mapping.LF(shp, 'pls')
mapping.LF(shp, 'pms')
mapping.LF(shp, 'prs')
mapping.sprs(shp, 'SprsCPA', 'Suppression costs per acre ($)')
mapping.sprs(shp, 'SprsCosts', 'Suppression costs ($)')
mapping.sprs(shp, 'SprsAcres', 'Suppression acres')
mapping.sprs(shp, 'SprsDays', 'Containment duration (Days)')
mapping.sprs(shp, 'OutDays', 'Fire out duration (Days)')
mapping.sprs(shp, 'SprsFires', 'Number of naturally-caused fires suppressed')
mapping.sprs(shp, 'PctSprs', 'Ratio of suppressed naturally-caused fires')
mapping.btl(shp, 'beetleAcres')











