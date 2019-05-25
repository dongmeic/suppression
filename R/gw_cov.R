library(spgwr)
library(rgdal)

path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
x <- readOGR(dsn=path, layer="mpb10km_data_gwr")

#gw_cov <- gw.cov(x, vars=2:5, adapt=0.2, cor=TRUE, var.term=TRUE, longlat=FALSE)

#writeOGR(gw_cov$SDF, dsn=path, layer="local_statistics", driver="ESRI Shapefile", overwrite_layer=TRUE)

# some error occurred in the gw.cov function; subscript out of bounds
for(i in 13:39){
	for(j in 13:39){
		if(i != j & names(x)[i] != 'Tmean' & names(x)[j] != 'Tmean'){
			gw_cov <- gw.cov(x, vars=c(names(x)[i],names(x)[j]), adapt=0.2, cor=TRUE, var.term=TRUE, longlat=FALSE)
			cov <- grep("cov", names(gw_cov$SDF),value=TRUE)
			cor <- grep("cor", names(gw_cov$SDF),value=TRUE)
			print(spplot(gw_cov$SDF, cov, main=cov))
			print(spplot(gw_cov$SDF, cor, main=cor))
		}
	}
}
