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
get.plot <- function(df, var, agr.var, title){
	strings <- paste0('barplot(df$', agr.var, ', names.arg = data.frame(df)[,var], main = title, cex.names = 1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2)')
	eval(parse(text=strings))
}

# consistent
plot.lf <- function(plotvar = 'beetleAcres', agr.var = 'grids', fun='sum'){
	png(paste0(outpath, plotvar, "_", agr.var, "_plots.png"), width=15, height=6, units="in", res=300)
	par(mfrow=c(2,3),xpd=FALSE,mar=c(3,3,3,0))
	for(var in lf.vars){
		sdf <- get.df(df, plotvar, var, fun)
		get.plot(sdf, var, agr.var, titles[lf.vars==var])
	}
	dev.off()
}
plot.lf()
plot.lf(agr.var = 'average')
plot.lf(plotvar = 'PctSprs', agr.var = 'mean', fun='mean')
plot.lf(plotvar = 'OutDays', agr.var = 'mean', fun='mean')
plot.lf(plotvar = 'SprsCosts', agr.var = 'percent', fun='sum')
