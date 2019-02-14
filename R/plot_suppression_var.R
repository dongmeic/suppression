library(rgdal)
library(raster)
library(dplyr)
library(RColorBrewer)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath,"mpb10km_with_beetle_data.csv"))
sprs.vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 
					'SprsAcre', 'SprsDays', 'OutDays')
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"

df.cpa <- df[df$SprsCPA != Inf, ]

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

plot.lf <- function(df=df, plotvar = 'beetleAcres', agr.var = 'grids', fun='sum'){
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
plot.lf(plotvar = 'SprsAcre', agr.var = 'mean', fun='mean')
plot.lf(plotvar = 'SprsAcres', agr.var = 'average', fun='sum')
plot.lf(plotvar = 'SprsFires', agr.var = 'average', fun='sum')
plot.lf(plotvar = 'PctLarge', agr.var = 'mean', fun='mean')
plot.lf(plotvar = 'density', agr.var = 'mean', fun='mean')
plot.lf(df=df.cpa, plotvar = 'SprsCPA', agr.var = 'median', fun='median')

plt.vars <- c('beetleAcres', 'mStdAge', 'density', 'PctOld', sprs.vars)
sprs.titles <- c('Suppression costs', 'Suppression acres', 'Unit suppression costs',
								 'No. fires suppressed', 'Ratio of fires suppressed', 'Fire size suppressed',
								 'Containment duration', 'Fire out duration')
plt.titles <- c('MPB affected acres', 'Stand age', 'Tree density', 'Ratio of old trees', sprs.titles)
agr.vars <- c('average', 'mean', 'mean', 'mean', 'median', 'median', 'median', 'average', 'mean',
							'median', 'mean', 'mean')
funs <- c('sum', 'mean', 'mean', 'mean', 'median', 'median', 'median', 'sum', 'mean',
					'median', 'mean', 'mean')
plot.vcc <- function(){
	png(paste0(outpath, "vcc_plots.png"), width=12, height=9, units="in", res=300)
	par(mfrow=c(3,4),xpd=FALSE,mar=c(3,3,3,0))
	for(var in plt.vars){
		if(var == 'SprsCPA'){
			sdf <- get.df(df.cpa, var, 'vcc', fun=funs[plt.vars==var])
		}else{
			sdf <- get.df(df, var, 'vcc', fun=funs[plt.vars==var])
		}	
		get.plot(sdf, 'vcc', agr.var=agr.vars[plt.vars==var], plt.titles[plt.vars==var])
	}
	dev.off()
}
plot.vcc()

plot.severity <- function(){
	png(paste0(outpath, "severity_plots.png"), width=12, height=9, units="in", res=300)
	par(mfrow=c(3,4),xpd=FALSE,mar=c(3,3,3,0))
	for(var in plt.vars){
		if(var == 'SprsCPA'){
			sdf <- get.df(df.cpa, var, 'severity.no', fun=funs[plt.vars==var])
		}else{
			sdf <- get.df(df, var, 'severity.no', fun=funs[plt.vars==var])
		}	
		get.plot(sdf, 'severity.no', agr.var=agr.vars[plt.vars==var], plt.titles[plt.vars==var])
	}
	dev.off()
}
plot.severity()

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
shp.path <- '/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles'
roadless.shp <- readOGR(dsn=shp.path, layer='mpb10km_wilderness_roadless', stringsAsFactors=F)
wilderness.t <- rasterized(roadless.shp, "wilderness", roadless)
roadless.t <- rasterized(roadless.shp, "roadless", roadless)

# vegetation condition class with fire severity

var <- 'beetleAcres'
fun <- 'sum'
get.table <- function(df, var, agr.var, fun){
	df.low <- df[df$severity == 'low', ]
	df.mixed <- df[df$severity == 'mixed', ]
	df.replacement <- df[df$severity == 'replacement', ]
	sdf <- as.data.frame(get.df(df, var, 'vcc', fun=fun))
	sdf.low <- as.data.frame(get.df(df.low, var, 'vcc', fun=fun))
	sdf.mixed <- as.data.frame(get.df(df.mixed, var, 'vcc', fun=fun))
	sdf.replacement <- as.data.frame(get.df(df.replacement, var, 'vcc', fun=fun))
	sdf.group <- rbind(sdf.low, sdf.mixed, sdf.replacement)
	sdf.group$severity <- c(rep('low', dim(sdf.low)[1]), rep('mixed', dim(sdf.mixed)[1]), 
										rep('replacement', dim(sdf.replacement)[1]))
	mat <- matrix(0, ncol=length(unique(sdf.group$vcc)), nrow=length(unique(sdf.group$severity)))
	rownames(mat) <- sort(unique(sdf.group$severity))
	colnames(mat) <- sort(unique(sdf.group$vcc))
	for (sev in sort(unique(sdf.group$severity))) {
		for (v in sort(unique(sdf.group$vcc))) {
			 sev.sum <- sum(sdf.group[sdf.group$vcc == v, 'grids'])
			 if(length(sdf.group[sdf.group$severity == sev & sdf.group$vcc == v, 'grids']) != 0){
					mat[sev, v] <- sdf.group[sdf.group$severity == sev & sdf.group$vcc == v, 'grids'] / sev.sum
			 }  
		}
	}
	data <- mat * rbind(sdf[,agr.var], sdf[,agr.var],sdf[,agr.var])
	return(data)
}

data <- get.table(df, 'beetleAcres', 'average', 'sum')

get.plot.col <- function(df, title){
	barplot(df, col=c('#636363', '#bdbdbd', '#f0f0f0'), main = title, cex.names = 1.2, cex.lab=1.2, cex.axis=1.2, cex.main=1.5)
}
get.plot.col(data, 'MPB affected acres')

plot.vcc.severity <- function(){
	png(paste0(outpath, "vcc_severity_plots.png"), width=12, height=9, units="in", res=300)
	par(mfrow=c(3,4),xpd=FALSE,mar=c(3,3,3,0))
	for(var in plt.vars){
		if(var == 'SprsCPA'){
			dt <- get.table(df.cpa, var, agr.var=agr.vars[plt.vars==var], fun=funs[plt.vars==var])
		}else{
			dt <- get.table(df, var, agr.var=agr.vars[plt.vars==var], fun=funs[plt.vars==var])
		}	
		get.plot.col(dt, plt.titles[plt.vars==var])
		print(var)
	}
	dev.off()
}
plot.vcc.severity()

get.table.sev <- function(df, var, agr.var, fun){
	df.vcc1 <- df[df$vcc == 1, ]
	df.vcc2 <- df[df$vcc == 2, ]
	df.vcc3 <- df[df$vcc == 3, ]
	df.vcc4 <- df[df$vcc == 4, ]
	df.vcc5 <- df[df$vcc == 5, ]
	df.vcc6 <- df[df$vcc == 6, ]
	sdf <- as.data.frame(get.df(df, var, 'severity.no', fun=fun))
	sdf.vcc1 <- as.data.frame(get.df(df.vcc1, var, 'severity.no', fun=fun))
	sdf.vcc2 <- as.data.frame(get.df(df.vcc2, var, 'severity.no', fun=fun))
	sdf.vcc3 <- as.data.frame(get.df(df.vcc3, var, 'severity.no', fun=fun))
	sdf.vcc4 <- as.data.frame(get.df(df.vcc4, var, 'severity.no', fun=fun))
	sdf.vcc5 <- as.data.frame(get.df(df.vcc5, var, 'severity.no', fun=fun))
	sdf.vcc6 <- as.data.frame(get.df(df.vcc6, var, 'severity.no', fun=fun))
	sdf.group <- rbind(sdf.vcc1, sdf.vcc2, sdf.vcc3, sdf.vcc4, sdf.vcc5, sdf.vcc6)
	sdf.group$vcc <- c(rep('vcc1', dim(sdf.vcc1)[1]), rep('vcc2', dim(sdf.vcc2)[1]), 
										 rep('vcc3', dim(sdf.vcc3)[1]), rep('vcc4', dim(sdf.vcc4)[1]),
										 rep('vcc5', dim(sdf.vcc5)[1]), rep('vcc6', dim(sdf.vcc6)[1]))
	mat <- matrix(0, ncol=length(unique(sdf.group$severity.no)), nrow=length(unique(sdf.group$vcc)))
	rownames(mat) <- sort(unique(sdf.group$vcc))
	colnames(mat) <- sort(unique(sdf.group$severity.no))
	for (v in sort(unique(sdf.group$vcc))) {
		for (sev in sort(unique(sdf.group$severity.no))) {
			 v.sum <- sum(sdf.group[sdf.group$severity.no == sev, 'grids'])
			 if(length(sdf.group[sdf.group$vcc == v & sdf.group$severity.no == sev, 'grids']) != 0){
					mat[v, sev] <- sdf.group[sdf.group$vcc == v & sdf.group$severity.no == sev, 'grids'] / v.sum
			 }  
		}
	}
	data <- mat * rbind(sdf[,agr.var], sdf[,agr.var], sdf[,agr.var], sdf[,agr.var], sdf[,agr.var],sdf[,agr.var])
	return(data)
}

get.plot.col.sev <- function(df, title){
	barplot(df, col=rev(brewer.pal(6,"Greys")), main = title, cex.names = 1.2, cex.lab=1.2, cex.axis=1.2, cex.main=1.5)
}

get.plot.col.sev(data, 'MPB affected acres')
