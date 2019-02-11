# Table plots: fire severity and fire suppression variables (costs, number of suppressed fires, 
# percent of suppressed fires, duration of containment?)

library(dplyr)
library(ggplot2)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

mode <- function(x, na.rm = TRUE) {
  if(length(x) > 0){
  	return(as.numeric(names(sort(-table(x)))[1]))
  }else{
  	return(0)
  }
}

indata <- read.csv(paste0(csvpath, 'mpb10km_input_data.csv')) # from interactions.R
costs.df <- read.csv(paste0(csvpath, 'suppressed_costs.csv')) # from suppressed_fires.R
fires.df <- read.csv(paste0(csvpath, 'suppressed_fires.csv')) # from suppressed_fires.R
df <- cbind(indata, costs.df, fires.df)

get.df <- function(df, var1, var2, fun){
	if(var2 == 'mfri'){
		cond <- paste0(var2, ' %in% c(1:22)')
	}else if(var2 == 'vcc'){
		cond <- paste0(var2, ' %in% c(1:6)')
	}else if(var2 %in% c('pls', 'pms', 'prs')){
		cond <- paste0(var2, ' %in% c(1:20)')
	}
	strings <- paste0('df %>% subset(forest==1 & ', cond, ') %>% dplyr::select(')
	strings <- paste0(strings, var2, ',', var1, ') %>% group_by(', var2)
	strings <- paste0(strings,  ') %>% summarise(', fun, '=', fun, '(', var1)
	strings <- paste0(strings, ', na.rm=TRUE), grids = sum(!is.na(', var1)
	if(fun=='sum'){
		strings <- paste0(strings,')), average=sum(', var1, ', na.rm=TRUE)/sum(!is.na(', var1, ')))')
	}else{
		strings <- paste0(strings,')), percent=sum(!is.na(', var1, '))/length(',var1,'))')
	}
	sdf <- eval(parse(text=strings))
	return(sdf)
}

outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
get.plot <- function(df, outnm, title, w){
	png(paste0(outpath, title, outnm, ".png"), width=w, height=9, units="in", res=300)
	par(mfrow=c(3,1),xpd=FALSE,mar=c(3,3,3,0))
	if(title %in% c('percent of fires suppressed ', 'fire size suppressed ', 'containment duration ', 'fire out duration ')){
		if(title == 'suppressed fire size '){
			barplot(df$mode, main = paste("mode of", title), axes = F, cex.main=2)
		}else{
			barplot(df$mean, main = paste("mean of", title), axes = F, cex.main=2)
		}
		axis(2, cex.axis=1.5)
		barplot(df$grids, main = "number of grid cells",  axes = F, cex.main=2)
		axis(2, cex.axis=1.5)
		barplot(df$percent, names.arg = data.frame(df)[,outnm], main = paste0(title, "(%)"), cex.names = 1.6, cex.lab=1.6, cex.axis=1.6, cex.main=2.2)
	}else{
		barplot(df$sum, main = paste("total", title), axes = F, cex.main=2)
		axis(2, cex.axis=1.5)
		barplot(df$grids, main = "number of grid cells",  axes = F, cex.main=2)
		axis(2, cex.axis=1.5)
		barplot(df$average, names.arg = data.frame(df)[,outnm], main = paste0(title, "per grid cell"), cex.names = 1.6, cex.lab=1.6, cex.axis=1.6, cex.main=2.2)
	}
	dev.off()
}

vars <- c('SprsCosts', 'SprsAcres', 'SprsFires', 'PctSprs', 'SprsAcre', 'SprsDays', 'OutDays')
titles <- c('costs ', 'suppressed acres ', 'number of fires suppressed ',
						'percent of fires suppressed ', 'fire size suppressed ', 'containment duration ',
						'fire out duration ')
vals <- c('mfri', 'vcc', 'pls', 'pms', 'prs')
ws <- c(12,6,12,12,12)
funs <- c('sum', 'sum', 'sum', 'mean', 'mean', 'mean', 'mean')

for(var in vars){
	for(val in vals){	
		sdf <- get.df(df, var, val, funs[vars==var])
		get.plot(sdf, val, titles[vars==var], ws[vals==val])
		print(paste(var, val))
	}	
}
