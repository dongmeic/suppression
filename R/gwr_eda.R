library(spgwr)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_scaled.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

ptm <- proc.time()
GWRbandwidth <- gwr.sel(beetleAcres ~ ., data=data, coords=cbind(xy$x,xy$y), adapt=T)
proc.time() - ptm

ptm <- proc.time()
gwr.model <- gwr(beetleAcres ~ ., data=data, coords=cbind(xy$x, xy$y), adapt=GWRbandwidth, 
                hatmatrix=TRUE, se.fit=TRUE)
proc.time() - ptm

results <- as.data.frame(gwr.model$SDF)
write.csv(results, sprintf('%s/gwr_results_scaled.csv', ROOT), row.names=FALSE)