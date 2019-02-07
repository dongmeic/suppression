library(rgdal)

source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
plot.table <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/FIA.csv")
names(plot.table)
colnames(plot.table)[9:12] <- c("SLOPE_MIN","SLOPE_MAX","ASPECT_MIN","ASPECT_MAX")
plot.table <- plot.table[!is.na(plot.table$LON) & !is.na(plot.table$LAT),]
plot.spdf <- df2spdf(7,6,"LON", "LAT",plot.table)
shp.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/FIA"
#writeOGR(obj=plot.spdf, dsn = shp.path, layer = "FIA", driver = "ESRI Shapefile", overwrite_layer = TRUE)

FIA <- readOGR(dsn=shp.path, layer = "FIA", stringsAsFactors = FALSE)
stand_age <- readOGR(dsn = mpb10km.path, layer = "stand_age")
tree_density <- readOGR(dsn = mpb10km.path, layer = "tree_density")
stand_age$RASTERVALU[stand_age$RASTERVALU==-9999] <- NA
stand_age_r <- rasterize(stand_age, mpb10km.pts.r, "RASTERVALU", max, na.rm=FALSE)
tree_density$RASTERVALU[tree_density$RASTERVALU==-9999] <- NA
tree_density_r <- rasterize(tree_density, mpb10km.pts.r, "RASTERVALU", max, na.rm=FALSE)
elev_r <- rasterized(FIA, "ELEV", mean)
STDAGE <- rasterized(FIA, "STDAGE", median)
NBR_LIV <- rasterized(FIA, "NBR_LIV", median)
LIVE_CA <- rasterized(FIA, "LIVE_CA", median)
BALIVE <- rasterized(FIA, "BALIVE", median)
PT_LRG <- rasterized(FIA, "PT_LRG", median)
PT_OLD <- rasterized(FIA, "PT_OLD", median)
host_r <- rasterized(FIA, "hosts", host)

stand_age <- cover(stand_age_r, STDAGE)
stand_age_updated <- extract(stand_age, mpb10km.pt, method='simple')
pct_large <- extract(PT_LRG, mpb10km.pt, method='simple')
pct_old <- extract(PT_OLD, mpb10km.pt, method='simple')
df <- data.frame(mStdAge=stand_age_updated, PctLarge=pct_large, PctOld=pct_old)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
write.csv(df, paste0(csvpath, "stand_age_mean.csv"), row.names=FALSE)
mapping("stand_age_updated", stand_age, "Stand age", d=0, "Reds", "kmeans")
mapping("stand_age", stand_age_r, "Stand age", d=0, "Reds", "kmeans")
mapping("tree_density", tree_density_r, "Tree density", d=0, "Reds", "kmeans")
mapping("elev_FIA", elev_r, "Elevation", d=0, "Greys", "kmeans")
mapping("STDAGE_FIA", STDAGE, "Stand age", d=0, "Reds", "kmeans")
mapping("NBR_LIV", NBR_LIV, "Number of live stems", d=0, "Reds", "kmeans")
mapping("LIVE_CA", LIVE_CA, "Live canopy cover percent", d=0, "Reds", "kmeans")
mapping("BALIVE", BALIVE, "Basal area of live trees", d=0, "Reds", "kmeans")
mapping("PT_LRG", PT_LRG, "Percent of large trees", d=2, "Reds", "kmeans")
mapping("PT_OLD", PT_OLD, "Percent of old trees", d=2, "Reds", "kmeans")

labels <- c("No", "Yes")
cols <- c("lightgrey", "darkred")
title <- "Mountain pine beetle host presence"

mapping.host <- function(shp, var, outnm){
	r <- rasterized(shp, var, host)
	r <- as.factor(r)
	rat <- levels(r)[[1]]
	rat[["labels"]] <- labels
	levels(r) <- rat
	p <- levelplot(r, col.regions=cols, xlab="", ylab="",par.settings = list(axis.line = list(col = "transparent")), 
						scales = list(draw = FALSE), margin=F, main=title)
	p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
	png(paste0(out,outnm,".png"), width=8, height=6, units="in", res=300)
	par(mfrow=c(1,1), xpd=FALSE, mar=rep(0.5,4))
	print(p)
	dev.off()
}

mapping.host(FIA, 'hosts', 'MPB_host_FIA')

cohost <- readOGR('/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/mpb10km', 'mpb10km_corehost')
mapping.host(cohost, 'FIA_hosts', 'MPB_host_FIA_mpb10km')
mapping.host(cohost, 'vegetation', 'MPB_cohost_mpb10km')
