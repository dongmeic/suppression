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
#    user   system  elapsed
# 315.892   14.780 1233.997

# correlation between year and temperature and precipitation
tmean <- read.csv(paste0(outpath, "mpb10km_Tmean_1902_2016.csv"))
pmean <- read.csv(paste0(outpath, "mpb10km_Pmean_1902_2016.csv"))

coeff <- function(x, coeff=T){
	df <- data.frame(time=years, clim=as.numeric(x))
	mod <- glm(clim ~ time, family = gaussian(), data=df)
	if(coeff){
		return(summary(mod)$coefficients[2,1])
	}else{
		return(summary(mod)$coefficients[1,1])
	}	
}

ptm <- proc.time()
tcoeff <- vector()
pcoeff <- vector()
tintct <- vector()
pintct <- vector()
for(i in 1:dim(tmean)[1]){
	if(sum(!is.na(tmean[i,])) != 0){
		v1 <- coeff(tmean[i,])
		v2 <- coeff(pmean[i,])
		v3 <- coeff(tmean[i,], coeff=F)
		v4 <- coeff(pmean[i,], coeff=F)
	
		tintct[i] <- v3
		pintct[i] <- v4
		tcoeff[i] <- v1
		pcoeff[i] <- v2
	
		print(i)	
	}
}
proc.time() - ptm

df <- data.frame(tcoeff=tcoeff, pcoeff=pcoeff, tintct=tintct, pintct=pintct)
write.csv(df, paste0(outpath,"coeff_time.csv"), row.names=FALSE)




