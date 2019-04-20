library(spgwr)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

ptm <- proc.time()
vars <- c()
GWRbandwidth <- gwr.sel(beetleAcres^0.05 ~ lon + I(lon^2) + lat + I(lat^2) + 
    etopo1 + I(etopo1^2) + mStdAge + I(mStdAge^2) + density + 
    I(density^2) + log(vcc) + mfri + log(mfri) + prs + I(prs^2) + 
    GAP1 + GAP3 + vpd + I(vpd^2) + cwd + I(cwd^2) + I(maxAugT^2) + 
    I(summerP0^2) + summerP0 + Tmean + I(Tmean^2) + exp(Tmean) + 
    mi + I(mi^2) + Tvar + I(Tvar^2) + wd + I(wd^2) + AugTmean + 
    OctTmin + I(OctTmin^2) + I(AugMaxT^2) + AugTmax + Acs + I(Acs^2) + 
    MarMin + ddAugJun + I(ddAugJun^2) + ddAugJul + I(ddAugJul^2) + 
    PPT + I(PPT^2) + summerP2 + I(summerP2^2) + TMarAug + exp(TMarAug) + 
    Mar20 + log(Mar20 + 1) + fallTmean + exp(fallTmean) + MarTmin + 
    maxT + Tmin + winterMin + summerTmean + Pmean + I(Pmean^2) + 
    I(summerP1^2) + minT + I(JanMin^2) + TOctSep + Jan20 + I(Jan20^2) + 
    PcumOctSep + I(PcumOctSep^2), data=data, coords=cbind(xy$x,xy$y), adapt=T)
proc.time() - ptm

ptm <- proc.time()
gwr.model <- gwr(beetleAcres^0.05 ~ lon + I(lon^2) + lat + I(lat^2) + 
    etopo1 + I(etopo1^2) + mStdAge + I(mStdAge^2) + density + 
    I(density^2) + log(vcc) + mfri + log(mfri) + prs + I(prs^2) + 
    GAP1 + GAP3 + vpd + I(vpd^2) + cwd + I(cwd^2) + I(maxAugT^2) + 
    I(summerP0^2) + summerP0 + Tmean + I(Tmean^2) + exp(Tmean) + 
    mi + I(mi^2) + Tvar + I(Tvar^2) + wd + I(wd^2) + AugTmean + 
    OctTmin + I(OctTmin^2) + I(AugMaxT^2) + AugTmax + Acs + I(Acs^2) + 
    MarMin + ddAugJun + I(ddAugJun^2) + ddAugJul + I(ddAugJul^2) + 
    PPT + I(PPT^2) + summerP2 + I(summerP2^2) + TMarAug + exp(TMarAug) + 
    Mar20 + log(Mar20 + 1) + fallTmean + exp(fallTmean) + MarTmin + 
    maxT + Tmin + winterMin + summerTmean + Pmean + I(Pmean^2) + 
    I(summerP1^2) + minT + I(JanMin^2) + TOctSep + Jan20 + I(Jan20^2) + 
    PcumOctSep + I(PcumOctSep^2), data=data, coords=cbind(xy$x, xy$y), 
    adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
proc.time() - ptm

sink(sprintf('%s/gwr_model_results_wo_FIA_01.txt', ROOT))
gwr.model
sink()

results <- as.data.frame(gwr.model$SDF)
write.csv(results, sprintf('%s/gwr_results_wo_FIA_01.csv', ROOT), row.names=FALSE)