# explore models

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath, "mpb10km_nonclimate.csv"))
bioclm <- read.csv(paste0(csvpath, "mpb10km_bioclm/mpb10km_bioclm_mean.csv"))
clm.predictors <- c('vpd', 'cwd', 'maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd', 
										'AugTmean', 'OctTmin', 'AugMaxT', 'AugTmax', 'Acs', 'MarMin', 
										'ddAugJun', 'ddAugJul', 'JanTmin', 'PPT', 'summerP2', 'TMarAug', 
										'Mar20', 'fallTmean', 'MarTmin', 'maxT', 'Tmin', 'winterMin', 
										'summerTmean', 'Pmean', 'summerP1', 'minT', 'JanMin', 'TOctSep', 
										'Jan20', 'PcumOctSep')
nonclm.predictors <- c("lon", "lat", "etopo1", "x", "y", "allyears", 
												"vegetation", "forest", "mStdAge", "density", "PctLarge", "PctOld",
												"vcc", "mfri", "prs", "pms", "pls", "GAP", "GAP2", "GAP3")
ndf <- cbind(df[,nonclm.predictors], bioclm[,clm.predictors])

path <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/test5/'
coeffs <- read.csv(paste0(path, 'coefficients.csv'))
interactions <- grep(':', coeffs$predictor, value=TRUE)
selected <- c('vpd', 'cwd','maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd', 'AugTmean',
    'OctTmin', 'AugMaxT', 'AugTmax', 'MarMin', 'ddAugJun', 'ddAugJul',
    'JanTmin', 'PPT', 'summerP2', 'TMarAug', 'Mar20', 'fallTmean',
    'MarTmin', 'maxT', 'Tmin', 'summerTmean', 'Pmean', 'minT', 'JanMin',
    'TOctSep', 'Jan20', 'PcumOctSep')
vars <- sapply(interactions, function(x) unlist(strsplit(x, ":", fixed = TRUE))[2])
preds.interact <- interactions[vars %in% selected]

ndf$GAP2 <- ifelse(ndf$GAP2 == 2, 1, 0)
ndf$GAP3 <- ifelse(ndf$GAP3 == 3, 1, 0)
colnames(ndf)[which(colnames(ndf)=='allyears')] <- 'beetleAcres'
colnames(ndf)[which(colnames(ndf)=='vegetation')] <- 'host'
colnames(ndf)[which(colnames(ndf)=='GAP')] <- 'GAP1'

vars <- c('host', 'forest', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3')
for (var in vars){
	ndf[,var] <- as.factor(ndf[,var])
}

indata <- subset(ndf, !is.na(beetleAcres) & forest == '1' & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
										 !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
										 !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))

df <- indata[, -which(names(indata) %in% c('forest', 'x','y','host'))]							 
for(var in preds.interact){
	var1 <- unlist(strsplit(var, ":", fixed = TRUE))[1]
	var2 <- unlist(strsplit(var, ":", fixed = TRUE))[2]
	df[,var] <- df[,var1] * df[,var2]
	print(paste('Added interaction term:', var))
}

df1 <- as.data.frame(scale(df[,-which(names(df) %in% c('beetleAcres', vars))]))
df2 <- cbind(df[,c('beetleAcres', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3')], df1)
mod <- lm(beetleAcres^0.07 ~ ., data=df2)
summary(mod)
#mod <- step(mod)

mod <- lm(
		beetleAcres^0.07 ~ vcc + pms + pls + GAP1 + GAP3 +
    etopo1 + density + vpd + cwd + summerP0 + Tmean + Tvar +
    wd + AugTmean + AugMaxT + AugTmax + Acs + MarMin + ddAugJun +
    ddAugJul + JanTmin + PPT + summerP2 + TMarAug + Mar20 + fallTmean +
    maxT + Tmin + winterMin + summerTmean + Pmean + minT + JanMin +
    TOctSep + PcumOctSep + `lon:vpd` + `lat:Tvar` + `lat:maxAugT` +
    `lat:summerP0` + `lat:mi` + `lon:OctTmin` + `lon:AugMaxT` +
    `etopo1:AugTmax` + `etopo1:cwd` + `lon:Tmean` + `lon:mi` +
    `lon:AugTmean` + `lat:MarMin` + `lon:wd` + `lat:summerP2` +
    `lat:TMarAug` + `lat:Mar20` + `lat:fallTmean` + `etopo1:TMarAug` +
    `lat:MarTmin` + `lat:vpd` + `etopo1:vpd` + `lat:AugMaxT` +
    `lon:summerP2` + `lat:summerTmean` + `etopo1:wd` + `lat:Pmean` +
    `lon:maxAugT` + `lon:summerTmean` + `lon:Jan20` + `lon:PcumOctSep` +
    `lon:JanTmin` + `lon:minT` + `lat:AugTmax` + `lon:Pmean` +
    `etopo1:AugTmean` + `etopo1:PcumOctSep` + `etopo1:summerTmean` +
    `etopo1:Tvar` + `etopo1:PPT` + `lat:TOctSep` + `etopo1:TOctSep` +
    `lat:wd` + `etopo1:minT` + `lon:MarMin` + `lon:TOctSep` +
    `lat:minT` + `lat:Tmin` + `etopo1:JanTmin` + `lon:JanMin` +
    `lon:Tvar` + `etopo1:JanMin` + `etopo1:Pmean` + `lon:fallTmean` +
    `etopo1:OctTmin` + `lat:ddAugJun` + `etopo1:Tmin` + `lon:MarTmin` +
    `lat:ddAugJul` + `etopo1:summerP0` + `etopo1:Jan20` + `lat:PPT` +
    `lon:ddAugJul` + `etopo1:MarTmin` + `etopo1:Mar20` + `lon:TMarAug` +
    `etopo1:MarMin` + `lat:JanTmin` + `etopo1:fallTmean` + `lon:AugTmax` +
    `etopo1:summerP2`, data=df2)

summary(mod)

ptm <- proc.time()
mod <- lm(
		beetleAcres^0.07 ~ (lon + lat + etopo1 + density + PctLarge +
    vcc + mfri + prs + pms + pls + GAP1 + GAP3 + vpd + cwd +
    maxAugT + summerP0 + Tmean + mi + Tvar + wd + AugTmean +
    OctTmin + AugMaxT + AugTmax + MarMin + ddAugJun + ddAugJul +
    JanTmin + PPT + summerP2 + TMarAug + Mar20 + fallTmean +
    MarTmin + maxT + Tmin + summerTmean + Pmean + minT + JanMin +
    TOctSep + Jan20 + PcumOctSep)^2, data=df)
proc.time() - ptm

summary(mod)
ptm <- proc.time()
mod <- step(mod)
proc.time() - ptm
summary(mod)

