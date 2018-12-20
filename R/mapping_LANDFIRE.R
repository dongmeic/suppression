library(rasterVis)
library(colorRamps)
source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
LFpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/LANDFIRE.gdb"
vcc <- readOGR(dsn=paste0(LFpath), layer="VCC", stringsAsFactors = FALSE)
head(vcc)
table(vcc$RASTERVALU); table(vcc$NEW_DESCRI)
length(na.omit(unique(vcc$RASTERVALU)))

labels <- c("Very Low, Vegetation Departure 0-16%",
						"Low to Moderate, Vegetation Departure 17-33%", 
						"Moderate to Low, Vegetation Departure 34-50%",
						"Moderate to High, Vegetation Departure 51-66%",
						"High, Vegetation Departure 67-83%",
						"Very High, Vegetation Departure 84-100%",
						"Water",
						"Snow / Ice",
						"Non burnable Urban",
						"Burnable Urban",
						"Barren",
						"Sparsely Vegetated",
						"Non burnable Agriculture",
						"Burnable Agriculture")
cols <- c(brewer.pal(7,'BuGn')[-1],'#6baed6','#deebf7','#fff7bc','#662506','#737373','#ccebc5','#fec44f','#cc4c02')

mapping.LF <- function(shp, labels, cols, title, outnm){
	r <- rasterized(shp, "RASTERVALU", mean)
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
mapping.LF(vcc, labels, cols, "Vegetation condition class", "VCC")
mapping.LF(vcc.forest, labels, cols, "Vegetation condition class", "VCC_forest")

mfri <- readOGR(dsn=paste0(LFpath), layer="MFRI", stringsAsFactors = FALSE)
head(mfri)
table(mfri$RASTERVALU); table(mfri$LABEL)
length(unique(mfri$RASTERVALU));length(na.omit(unique(mfri$RASTERVALU)))
length(na.omit(unique(mfri$RASTERVALU)))
labels <- c("0-5 Years", "6-10 Years", "11-15 Years", "16-20 Years", "21-25 Years", "26-30 Years",
						"31-35 Years", "36-40 Years", "41-45 Years", "46-50 Years", "51-60 Years", "61-70 Years",
						"71-80 Years", "81-90 Years", "91-100 Years", "101-125 Years", "126-150 Years", "151-200 Years",
						"201-300 Years", "301-500 Years", "501-1000 Years", ">1000 Years", "Water", "Snow / Ice",
						"Barren", "Sparsely Vegetated", "Indeterminate Fire Regime Characteristics")
cols <- c(rev(matlab.like(22)),'#6baed6','#deebf7','#737373','#ccebc5','#bdbdbd')
mapping.LF(mfri, labels, cols, "Mean fire return interval", "MFRI")
length(na.omit(unique(mfri.forest$RASTERVALU)))
mapping.LF(mfri.forest, labels, cols, "Mean fire return interval", "MFRI_forest")

pms <- readOGR(dsn=paste0(LFpath), layer="PMS", stringsAsFactors = FALSE)
head(pms)
table(pms$RASTERVALU); table(pms$LABEL)
length(unique(pms$RASTERVALU));length(na.omit(unique(pms$RASTERVALU)))
length(na.omit(unique(pms$RASTERVALU)))
labels <- c("0-5%", "6-10%", "11-15%", "16-20%", "21-25%", "26-30%",
						"31-35%", "36-40%", "41-45%", "46-50%", "51-55%", "56-60%",
						"61-65%", "66-70%", "71-75%", "76-80%", "81-85%", "86-90%", "91-95%",
						"96-100%", "Water", "Snow / Ice", "Barren", "Sparsely Vegetated")
cols <- c(matlab.like(20),'#6baed6','#deebf7','#737373','#ccebc5')
mapping.LF(pms, labels, cols, "Percent of mixed-severity fires", "PMS")
length(na.omit(unique(pms.forest$RASTERVALU)))
mapping.LF(pms.forest, labels, cols, "Percent of mixed-severity fires", "PMS_forest")

prs <- readOGR(dsn=paste0(LFpath), layer="PRS", stringsAsFactors = FALSE)
head(prs)
table(prs$RASTERVALU); table(prs$LABEL)
length(unique(prs$RASTERVALU));length(na.omit(unique(prs$RASTERVALU)))
length(na.omit(unique(prs$RASTERVALU)))
labels <- c("0-5%", "6-10%", "11-15%", "16-20%", "21-25%", "26-30%",
						"31-35%", "36-40%", "41-45%", "46-50%", "51-55%", "56-60%",
						"61-65%", "66-70%", "71-75%", "76-80%", "81-85%", "86-90%",
						"96-100%", "Water", "Snow / Ice", "Barren", "Sparsely Vegetated")
cols <- c(matlab.like(20),'#6baed6','#deebf7','#737373','#ccebc5')
mapping.LF(prs, labels, cols, "Percent of replacement-severity fires", "PRS")
length(na.omit(unique(prs.forest$RASTERVALU)))
mapping.LF(prs.forest, labels, cols, "Percent of replacement-severity fires", "PRS_forest")

pls <- readOGR(dsn=paste0(LFpath), layer="PLS", stringsAsFactors = FALSE)
head(pls)
table(pls$RASTERVALU); table(pls$LABEL)
length(unique(pls$RASTERVALU));length(na.omit(unique(pls$RASTERVALU)))
length(na.omit(unique(pls$RASTERVALU)))
labels <- c("0-5%", "6-10%", "11-15%", "16-20%", "21-25%", "26-30%",
						"31-35%", "36-40%", "41-45%", "46-50%", "51-55%", "56-60%",
						"61-65%", "66-70%", "71-75%", "76-80%", "81-85%", "86-90%", "91-95%",
						"96-100%", "Water", "Snow / Ice", "Barren", "Sparsely Vegetated")
cols <- c(matlab.like(20),'#6baed6','#deebf7','#737373','#ccebc5')
mapping.LF(pls, labels, cols, "Percent of low-severity fires", "PLS")
length(na.omit(unique(pls.forest$RASTERVALU)))
mapping.LF(pls.forest, labels, cols, "Percent of low-severity fires", "PLS_forest")

landcover <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer="LandCover_mpb10km", 
										 stringsAsFactors = FALSE)
head(landcover)
#landcover$forest <- ifelse(1, 0, landcover$RASTERVALU %in% c("41", "42", "43"))

vcc$forest <- landcover$forest
mfri$forest <- landcover$forest
pms$forest <- landcover$forest
prs$forest <- landcover$forest
pls$forest <- landcover$forest

vcc.forest <- vcc[vcc$forest==1,]
mfri.forest <- mfri[mfri$forest==1,]
pms.forest <- pms[pms$forest==1,]
prs.forest <- prs[prs$forest==1,]
pls.forest <- pls[pls$forest==1,]

table(vcc.forest$RASTERVALU); table(vcc.forest$NEW_DESCRI)
table(mfri.forest$RASTERVALU); table(mfri.forest$LABEL)
table(pms.forest$RASTERVALU); table(pms.forest$LABEL)
table(prs.forest$RASTERVALU); table(prs.forest$LABEL)
table(pls.forest$RASTERVALU); table(pls.forest$LABEL)

