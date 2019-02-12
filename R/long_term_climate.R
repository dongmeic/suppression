library(ncdf4)
library(raster)
library(rgdal)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

inpath <- '/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var'
setwd(inpath)

file <- 'na10km_v2_Tmean_1902.2016.3d.nc'
years <- 1902:2016; nyr <- length(years)

vars <- c('Tmean', 'Pmean')

ncin <- nc_open("/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/na10km_v2.nc")
na10km.prj <- ncatt_get(ncin,"lambert_azimuthal_equal_area","CRS.PROJ.4")$value
mpb10km.pt <- readOGR("/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km","mpb10km_us_gridpts")
mpb10km.prj <- proj4string(mpb10km.pt)

extract.pt.from.r <- function(var, i){	
	ncfile <- paste0("na10km_v2_",var, "_", years[1], ".", years[nyr], ".3d.nc")
	r <- raster(ncfile, level=1, varname = var, band=i)	
	proj4string(r) <- CRS(na10km.prj)
	r <- projectRaster(r, crs=mpb10km.prj)
	extract(r, mpb10km.pt, method='simple')
}

outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/mpb10km_bioclm/"
ptm <- proc.time()
foreach(var=vars)%dopar%{
	for(i in 1:nyr){
		if(i == 1){
			df <- data.frame(y1902=extract.pt.from.r(var,i))
		}else{
			col.values <- extract.pt.from.r(var, i)
			df <- cbind(df, col.values)
			colnames(df)[dim(df)[2]] <- paste0('y', years[i])
		}
		print(years[i])
	}
	write.csv(df, paste0(outpath, "mpb10km_", var, "_1902_2016.csv"), row.names=FALSE)
}
proc.time() - ptm
# done



