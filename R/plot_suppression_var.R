library(rgdal)
library(dplyr)
library(ggplot2)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath,"mpb10km_with_beetle_data.csv"))
sprs.vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 
					'SprsAcre', 'SprsDays', 'OutDays')
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"

get.df <- function(df, var1, var2, fun){
	if(var2 == 'mfri'){
		cond <- paste0(var2, ' %in% c(1:22)')
	}else if(var2 == 'vcc'){
		cond <- paste0(var2, ' %in% c(1:6)')
	}else if(var2 %in% c('pls', 'pms', 'prs')){
		cond <- paste0(var2, ' %in% c(1:20)')
	}else if(var2 == 'severity.no'){
		cond <- paste0(var2, ' %in% c(1:3)')
	}
	strings <- paste0('df %>% subset(forest==1 & ', cond, ') %>% dplyr::select(')
	strings <- paste0(strings, var2, ',', var1, ') %>% group_by(', var2)
	strings <- paste0(strings,  ') %>% summarise(', fun, '=', fun, '(', var1)
	strings <- paste0(strings, ', na.rm=TRUE), grids = sum(!is.na(', var1)
	if(fun=='sum' & var1 %in% sprs.vars){
		strings <- paste0(strings,')), average=sum(', var1, ', na.rm=TRUE)/sum(!is.na(', var1)
		strings <- paste0(strings,')), percent=sum(!is.na(', var1, '))/length(',var1,'))')
	}else if(fun=='sum'){
		strings <- paste0(strings,')), average=sum(', var1, ', na.rm=TRUE)/sum(!is.na(', var1, ')))')
	}else{
		strings <- paste0(strings,')), percent=sum(!is.na(', var1, '))/length(',var1,'))')
	}
	sdf <- eval(parse(text=strings))
	return(sdf)
}

lf.vars <- c('vcc', 'mfri', 'prs', 'pms', 'pls', 'severity.no')
titles <- c('Vegetation condition class', 'Mean fire return interval', 'Percent of replacement-severity fires',
						'Percent of mixed-severity fires', 'Percent of low-severity fires', 'Fire severity')

outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
get.plot.grids <- function(df, var, title){
	barplot(df$grids, names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
}

# consistent
plot.grids <- function(plotvar = 'beetleAcres', fun='sum'){
	png(paste0(outpath, plotvar, "_grid_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot.grids(sdf, var, titles[lf.vars==var])
	}
	dev.off()
}
plot.grids()

get.plot.sum <- function(df, var, title){
	barplot(df$sum, names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
}

plot.sum <- function(plotvar = 'beetleAcres', fun='sum'){
	png(paste0(outpath, plotvar, "_sum_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot.sum(sdf, var, titles[lf.vars==var])
	}
	dev.off()
}
plot.sum()
plot.sum(plotvar = 'SprsFires')
plot.sum(plotvar = 'SprsCosts')

get.plot.mean <- function(df, var, title){
	barplot(df$mean, names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
}

plot.mean <- function(plotvar = 'PctSprs', fun='mean'){
	png(paste0(outpath, plotvar, "_mean_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot.mean(sdf, var, titles[lf.vars==var])
	}
	dev.off()
}
plot.mean()
plot.mean(plotvar = 'OutDays')
plot.mean(plotvar = 'SprsDays')
plot.mean(plotvar = 'mStdAge')
plot.mean(plotvar = 'density')
plot.mean(plotvar = 'PctLarge')
plot.mean(plotvar = 'PctOld')
plot.mean(plotvar = 'SprsAcre')

get.plot.average <- function(df, var, title){
	barplot(df$average, names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
}

# function is 'summary'
plot.average <- function(plotvar = 'beetleAcres', fun='sum'){
	png(paste0(outpath, plotvar, "_average_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot.average(sdf, var, titles[lf.vars==var])
	}
	dev.off()
}
plot.average()
plot.average(plotvar = 'SprsFires')
plot.average(plotvar = 'SprsCosts')


get.plot.percent <- function(df, var, title){
	barplot(df$percent, names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)
}

plot.percent <- function(plotvar = 'SprsCosts', fun='sum'){
	png(paste0(outpath, plotvar, "_percent_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot.percent(sdf, var, titles[lf.vars==var])
	}
	dev.off()
}
plot.percent()
plot.percent(plotvar = 'SprsFires')