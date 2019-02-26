library(spgwr)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

ptm <- proc.time()
vars <- c()
GWRbandwidth <- gwr.sel(log(beetleAcres) ~ lon + lat + etopo1 + mStdAge + density + 
    prs + mfri + GAP1 + GAP2 + GAP3 + vpd + summerP0 + Tmean + cwd + Tvar + 
    wd + AugTmax + Acs + PPT + Mar20, data=data, coords=cbind(xy$x,xy$y), adapt=T)
proc.time() - ptm

ptm <- proc.time()
gwr.model <- gwr(log(beetleAcres) ~ lon + lat + etopo1 + mStdAge + density + 
    prs + mfri + GAP1 + GAP2 + GAP3 + vpd + summerP0 + Tmean + cwd + Tvar + 
    wd + AugTmax + Acs + PPT + Mar20, data=data, coords=cbind(xy$x, xy$y), 
    bandwidth=GWRbandwidth, adapt=T, hatmatrix=TRUE, se.fit=TRUE)
proc.time() - ptm

sink(sprintf('%s/gwr_model_results.txt', ROOT))
gwr.model
sink()

results <- as.data.frame(gwr.model$SDF)
write.csv(results, sprintf('%s/gwr_results_wo_FIA_20v.csv', ROOT), row.names=FALSE)