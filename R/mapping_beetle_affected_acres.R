
setwd('/gpfs/projects/gavingrp/dongmeic/beetle/output/maps')
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath, "mpb10km_nonclimate.csv"))

shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km"
mpb10km <- readOGR(dsn = shppath, layer = "mpb10km")
crs <- proj4string(mpb10km)

get.spdf <- function(df, year){
	ndf <- df[,c('x', 'y', grep(year, colnames(df), value=TRUE))]
	xy <- data.frame(ndf[,c(1,2)])
	coordinates(xy) <- c('x', 'y')
	proj4string(xy) <- crs
	spdf <- SpatialPointsDataFrame(coords = xy, data = ndf, proj4string = crs)
	return(spdf)
}

nclr <- 5
color <- "YlOrBr"
plotclr <- brewer.pal(nclr,color)

mpb_acres_mapping <- function(df, year){
	spdf <- get.spdf(df, year)
	plotvar <- log(as.numeric(spdf@data[,3])+1)
	class <- classIntervals(plotvar, nclr, style="kmeans", dataPrecision=1)
	colcode <- findColours(class, plotclr)
	png(paste0("MPB_",year,"_test.png"), width=5, height=5, units="in", res=300)
	par(mfrow=c(1,1),mar=c(0.5,0,1.5,0))
	plot(spdf, col=colcode, main=year, pch=19, cex=0.1)
	plot(mpb10km, bord=rgb(0.7,0.7,0.7,0.7), add=T)
	legend('bottomleft',legend=names(attr(colcode, "table")),
						 fill=attr(colcode, "palette"), cex=1.2, title='', bty="n")
	dev.off()
}

mpb_acres_mapping(df, 1997)	
			 
mpb_acres_ts <- function(df, outnm){
	png(paste0(outnm, ".png"), width=15, height=12, units="in", res=300)
	par(mfrow=c(4,5),mar=c(0.5,0.5,1.5,0))
	for (year in 1997:2016){
		spdf <- get.spdf(df, year)
		plotvar <- log(as.numeric(spdf@data[,3])+1)
		#class <- classIntervals(plotvar, nclr, style="kmeans", dataPrecision=1)
		class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=c(0, 1.3, 3.4, 5.4, 7.3, 11.1), dataPrecision=1)
		colcode <- findColours(class, plotclr)
		plot(spdf, col=colcode, pch=19, cex=0.1)
		title(main=year, adj = 0.5, line = -1, cex.main=2)
		plot(mpb10km, bord=rgb(0.7,0.7,0.7,0.7), add=T)
		#legend('bottomleft',legend=names(attr(colcode, "table")),fill=attr(colcode, "palette"), title='', bty="n")
		print(year)
	}	
	dev.off()
}

mpb_acres_ts(df, 'MPB_acres_ts')

gdb <- '/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb/us_points.gdb'
source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")

for(year in 1997:2016){
	if(year == 1997){
		mpb <- readOGR(dsn = gdb, layer = paste0("us_mpb_", year))
		mpb <- spTransform(mpb, crs)
		r0 <- rasterized(mpb, "ACRES", sum)
		#r <- r0	
	}else{
		mpb <- readOGR(dsn = gdb, layer = paste0("us_mpb_", year))
		mpb <- spTransform(mpb, crs)
		r1 <- rasterized(mpb, "ACRES", sum)
		#r <- r + r1	
	}
	print(year)
}
