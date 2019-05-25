library(spgwr)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_scaled.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

ptm <- proc.time()
GWRbandwidth <- gwr.sel(beetleAcres ~  etopo1 + mStdAge + density + 
    prs + GAP1 + GAP3 + vpd + wd + summerP0 + Tmean + winterMin + Acs, 
    data=data, coords=cbind(xy$x,xy$y), adapt=T)
proc.time() - ptm

ptm <- proc.time()
gwr.model <- gwr(beetleAcres ~ etopo1 + mStdAge + density + 
    prs + GAP1 + GAP3 + vpd + wd + summerP0 + Tmean + winterMin + Acs, 
    data=data, coords=cbind(xy$x, xy$y), 
    adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
proc.time() - ptm

sink(sprintf('%s/gwr_model_results_wo_FIA_scaled_s.txt', ROOT))
gwr.model
sink()

results <- as.data.frame(gwr.model$SDF)
write.csv(results, sprintf('%s/gwr_results_wo_FIA_scaled_s.csv', ROOT), row.names=FALSE)
