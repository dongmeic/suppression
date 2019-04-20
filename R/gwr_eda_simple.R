library(spgwr)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

vars <- c('mStdAge', 'density', 'prs', 'mfri', 'vpd', 'summerP0', 'cwd', 'Tvar',
					'wd', 'AugTmax', 'Acs', 'PPT', 'Mar20', 'ddAugJul')
					
ptm <- proc.time()
for(v in vars){	
	df <- data[, c('beetleAcres', v)]
	colnames(df)[2] <- 'var'
	GWRbandwidth <- gwr.sel(log(beetleAcres) ~ var, data=df, coords=cbind(xy$x,xy$y), adapt=T)
	gwr.model <- gwr(log(beetleAcres) ~ var, data=df, coords=cbind(xy$x, xy$y), 
									 adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
	
	sink(sprintf('%s/gwr_model_results_%s.txt', ROOT, v))
	gwr.model
	sink()
	results <- as.data.frame(gwr.model$SDF)
	write.csv(results, sprintf('%s/gwr_results_wo_FIA_%s.csv', ROOT, v), row.names=FALSE)
}
proc.time() - ptm