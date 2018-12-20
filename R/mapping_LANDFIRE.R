library(rasterVis)
source("/gpfs/projects/gavingrp/dongmeic/suppression/R/data_summary_functions.R")
LFpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/LANDFIRE.gdb"
vcc <- readOGR(dsn=paste0(LFpath), layer="VCC", stringsAsFactors = FALSE)
head(vcc)
table(vcc$RASTERVALU); table(vcc$NEW_DESCRI)
length(na.omit(unique(vcc$RASTERVALU)))
vcc_r <- rasterized(vcc, "RASTERVALU", mean)
vcc_r <- as.factor(vcc_r)
rat <- levels(vcc_r)[[1]]
rat[["vcc"]] <- c("Very Low, Vegetation Departure 0-16%",
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
levels(vcc_r) <- rat
cols <- c(brewer.pal(7,'BuGn')[-1],'#6baed6','#deebf7','#fff7bc','#662506','#737373','#ccebc5','#fec44f','#cc4c02')

p <- levelplot(vcc_r, col.regions=cols, xlab="", ylab="",par.settings = list(axis.line = list(col = "transparent")), 
					scales = list(draw = FALSE), margin=F, main="Vegetation Condition Class")
p <- p + latticeExtra::layer(sp.polygons(mpb10km, lwd=0.5, col=alpha("black", alpha = 0.6)))
png(paste0(out,"VCC.png"), width=8, height=6, units="in", res=300)
par(mfrow=c(1,1), xpd=FALSE, mar=rep(0.5,4))
print(p)
dev.off()

mfri <- readOGR(dsn=paste0(LFpath), layer="MFRI", stringsAsFactors = FALSE)
head(mfri)
table(mfri$RASTERVALU); table(mfri$LABEL)
length(unique(mfri$RASTERVALU));length(na.omit(unique(mfri$RASTERVALU)))
mfri_r <- rasterized(mfri, "RASTERVALU", mean)
mfri_r <- as.factor(mfri_r)
rat <- levels(mfri_r)[[1]]
rat[["mfri"]] <- c("0-5 Years",
									"6-10 Years", 
									"11-15 Years",
									"16-20 Years",
									"21-25 Years",
									"26-30 Years",
									"31-35 Years",
									"36-40 Years",
									"41-45 Years",
									"46-50 Years",
									"51-60 Years",
									"61-70 Years",
									"71-80 Years",
									"81-90 Years",
									"91-100 Years",
									"101-125 Years",
									"126-150 Years",
									"151-200 Years",
									"201-300 Years",
									"301-500 Years",
									"501-1000 Years",
									">1000 Years",
									"Water",
									"Snow / Ice",
									"Barren",
									"Sparsely Vegetated",
									"Indeterminate Fire Regime Characteristics")
levels(mfri_r) <- rat
cols <- c(coolwarm(22)brewer.pal(7,'BuGn')[-1],'#6baed6','#deebf7','#737373','#ccebc5','#bdbdbd')

pms <- readOGR(dsn=paste0(LFpath), layer="PMS", stringsAsFactors = FALSE)
prs <- readOGR(dsn=paste0(LFpath), layer="PRS", stringsAsFactors = FALSE)
pls <- readOGR(dsn=paste0(LFpath), layer="PLS", stringsAsFactors = FALSE)

