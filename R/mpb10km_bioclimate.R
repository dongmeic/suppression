library(ncdf4)
library(raster)
library(rgdal)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var"
setwd(inpath)
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/mpb10km_bioclm/"

years <- 1996:2015; nyr <- length(years)

monthly <- c("OctTmin", "fallTmean", "JanTmin", "MarTmin", "Tmin", "Tmean", 
			"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", "PcumOctSep", 
			"summerP0", "summerP1", "summerP2", "PPT", "Pmean", "POctSep", "PMarAug")
coldsnaps <- c("Lcs", "Ecs", "Ncs", "Acs")
drops <- c("drop0", "drop5", "drop10", "drop15", "drop20", "drop20plus","max.drop")
mindays <- c("Oct20","Oct30","Oct40","Jan20","Jan30","Jan40","Mar20","Mar30",
					"Mar40","winter20","winter30","winter40")
mins <- c("OctMin","JanMin","MarMin","winterMin","minT")
wd <- c("wd", "cwd", "vpd", "mi", "pt.coef", "cv.gsp")
maxs <- c("maxAugT", "summerT40", "OptTsum", "AugMaxT", "maxT")
dd <- c("ddAugJul", "ddAugJun")
vars <- c(monthly, coldsnaps, drops, mindays, mins, wd, maxs, dd)

ncin <- nc_open("/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/na10km_v2.nc")
na10km.prj <- ncatt_get(ncin,"lambert_azimuthal_equal_area","CRS.PROJ.4")$value
mpb10km.pt <- readOGR("/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km","mpb10km_us_gridpts")
mpb10km.prj <- proj4string(mpb10km.pt)

extract.pt.from.r <- function(var, i){
	if(var %in% monthly){
		ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
		r <- raster(ncfile, level=1, varname = var, band=i)	
	}else{
		ncfile <- paste0("daily/na10km_v2_daymet_na_",var, "_",years[1],".",years[nyr],".3d.nc")
		r <- raster(ncfile, varname = var, band=i)
	}
	proj4string(r) <- CRS(na10km.prj)
	r <- projectRaster(r, crs=mpb10km.prj)
	extract(r, mpb10km.pt, method='simple')
}

foreach(i=1:nyr)%dopar%{
	var <- "OctTmin"
	df <- data.frame(OctTmin=extract.pt.from.r(var,i))
	for(var in vars[-1]){
		col.values <- extract.pt.from.r(var, i)
		df <- cbind(df, col.values)
		colnames(df)[dim(df)[2]] <- var
	}
	write.csv(df, paste0(outpath, "mpb10km_bioclm_", years[i], ".csv"), row.names=FALSE)
}

# create mean values of all years
extract.pt.from.b <- function(var){
	if(var %in% monthly){
		ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
		b <- brick(ncfile, level=1, varname = var)
	}else{
		ncfile <- paste0("daily/na10km_v2_daymet_na_",var, "_",years[1],".",years[nyr],".3d.nc")
		b <- brick(ncfile, varname = var)
	}
	r <- calc(b, mean)
	proj4string(r) <- CRS(na10km.prj)
	r <- projectRaster(r, crs=mpb10km.prj)
	extract(r, mpb10km.pt, method='simple')
}

var <- "OctTmin"
df <- data.frame(OctTmin=extract.pt.from.b(var))
for(var in vars[-1]){
	col.values <- extract.pt.from.b(var)
	df <- cbind(df, col.values)
	colnames(df)[dim(df)[2]] <- var
	#print(var)
}
write.csv(df, paste0(outpath, "mpb10km_bioclm_mean.csv"), row.names=FALSE)

print("all done!")
