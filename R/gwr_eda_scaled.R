library(spgwr)
library(spdep)

ROOT <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
data <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_scaled.csv', ROOT))
xy <- read.csv(sprintf('%s/mpb10km_data_wo_FIA_xy.csv', ROOT))

ptm <- proc.time()
GWRbandwidth <- gwr.sel(beetleAcres ~ lon + lat + etopo1 + mStdAge + density + 
    mfri + prs + GAP1 + GAP3 + vpd + cwd + summerP0 + Tmean + mi + Tvar + wd + AugTmean + 
    OctTmin + AugTmax + Acs + MarMin + ddAugJun + ddAugJul + PPT + summerP2 + TMarAug + 
    Mar20 + fallTmean + MarTmin + Tmin + winterMin + summerTmean + 
    Pmean + minT + TOctSep + Jan20 + PcumOctSep + lon_sq + lat_sq + 
    etopo1_sq + mStdAge_sq + density_sq + prs_sq + vpd_sq + cwd_sq + 
    maxAugT_sq + summerP0_sq + Tmean_sq + mi_sq + Tvar_sq + wd_sq + 
    OctTmin_sq + AugMaxT_sq + Acs_sq + ddAugJun_sq + ddAugJul_sq + 
    PPT_sq + summerP2_sq + Pmean_sq + summerP1_sq + JanMin_sq + 
    Jan20_sq + PcumOctSep_sq + Tmean_exp + TMarAug_exp + fallTmean_exp + 
    vcc_log + mfri_log + Mar20_logp1, data=data, coords=cbind(xy$x,xy$y), adapt=T)
proc.time() - ptm

ptm <- proc.time()
gwr.model <- gwr(beetleAcres ~ lon + lat + etopo1 + mStdAge + density + 
    mfri + prs + GAP1 + GAP3 + vpd + cwd + summerP0 + Tmean + mi + Tvar + wd + AugTmean + 
    OctTmin + AugTmax + Acs + MarMin + ddAugJun + ddAugJul + PPT + summerP2 + TMarAug + 
    Mar20 + fallTmean + MarTmin + Tmin + winterMin + summerTmean + 
    Pmean + minT + TOctSep + Jan20 + PcumOctSep + lon_sq + lat_sq + 
    etopo1_sq + mStdAge_sq + density_sq + prs_sq + vpd_sq + cwd_sq + 
    maxAugT_sq + summerP0_sq + Tmean_sq + mi_sq + Tvar_sq + wd_sq + 
    OctTmin_sq + AugMaxT_sq + Acs_sq + ddAugJun_sq + ddAugJul_sq + 
    PPT_sq + summerP2_sq + Pmean_sq + summerP1_sq + JanMin_sq + 
    Jan20_sq + PcumOctSep_sq + Tmean_exp + TMarAug_exp + fallTmean_exp + 
    vcc_log + mfri_log + Mar20_logp1, data=data, coords=cbind(xy$x, xy$y), 
    adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
proc.time() - ptm

sink(sprintf('%s/gwr_model_results_wo_FIA_scaled.txt', ROOT))
gwr.model
sink()

results <- as.data.frame(gwr.model$SDF)
write.csv(results, sprintf('%s/gwr_results_wo_FIA_scaled.csv', ROOT), row.names=FALSE)

mpb10km.gal.nb <- read.gal('/gpfs/projects/gavingrp/dongmeic/beetle/nb/mpb10km_fishnet_selected.gal')
mpb10km.gal.w <- nb2listw(mpb10km.gal.nb, zero.policy=TRUE)
sink(sprintf('%s/gwr_morantest.txt', ROOT))
gwr.morantest(gwr.model, mpb10km.gal.w, zero.policy = TRUE)
sink()

print("all done!")