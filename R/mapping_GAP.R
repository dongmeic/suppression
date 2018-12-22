library(rasterVis)
library(colorRamps)
source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
gap <- readOGR(dsn=inpath, layer="mpb10km_fire_protection", stringsAsFactors = FALSE)
gap@data$GAP2 <- ifelse(gap@data$GAP2 > 0, 1, 0)
gap@data$GAP3 <- ifelse(gap@data$GAP3 > 0, 1, 0)
gap@data$GAP4 <- ifelse(gap@data$GAP4 > 0, 1, 0)


labels <- c("No", "Yes")
cols <- c("lightgrey", "darkred")

mapping.GAP <- function(shp, var, labels, cols, title, outnm){
	r <- rasterized(shp, var, mean)
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

mapping.GAP(gap, "wilderness", labels, cols, "Wildnerness area", "wilderness")
mapping.GAP(gap, "GAP", labels, cols, "GAP status 1", "GAP1")
mapping.GAP(gap, "GAP2", labels, cols, "GAP status 2", "GAP2")
mapping.GAP(gap, "GAP3", labels, cols, "GAP status 3", "GAP3")
mapping.GAP(gap, "GAP4", labels, cols, "GAP status 4", "GAP4")

gap <- readOGR(dsn=inpath, layer="mpb10km_fire_protection", stringsAsFactors = FALSE)
labels <- c("Not available", "Disturbance events allowed", "Disturbance events suppressed", "Subject to extractive or OHV use", "No known mandate for protection")
cols <- c('lightgrey','#e41a1c','#377eb8','#4daf4a','#984ea3')
mapping.GAP(gap, "GAPs", labels, cols, "GAP status", "GAPs")
