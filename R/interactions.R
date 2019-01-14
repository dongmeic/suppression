
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
ndf$GAP2 <- ifelse(ndf$GAP2 == 2, 1, 0)
ndf$GAP3 <- ifelse(ndf$GAP3 == 3, 1, 0)
colnames(ndf)[which(colnames(ndf)=='allyears')] <- 'beetleAcres'
colnames(ndf)[which(colnames(ndf)=='vegetation')] <- 'host'
colnames(ndf)[which(colnames(ndf)=='GAP')] <- 'GAP1'
vars <- c('host', 'forest', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3')
for (var in vars){
	ndf[,var] <- as.factor(ndf[,var])
}
# write.csv(ndf, paste0(csvpath, "mpb10km_input_data.csv"), row.names=FALSE)
 
indata <- subset(ndf, !is.na(beetleAcres) & forest == '1' & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
										 !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
										 !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))

df <- indata[, -which(names(indata) %in% c('forest', 'x','y','host'))]			
mod <- lm(beetleAcres^0.07 ~ ., data=df)
summary(mod)
#mod <- step(mod)
df1 <- df[complete.cases(df), ]
mod <- lm(beetleAcres^0.07 ~ lon + lat + etopo1 + density + PctLarge +
    vcc + mfri + prs + pms + pls + GAP1 + GAP3 + vpd + cwd +
    maxAugT + summerP0 + Tmean + mi + Tvar + wd + AugTmean +
    OctTmin + AugMaxT + AugTmax + MarMin + ddAugJun + ddAugJul +
    JanTmin + PPT + summerP2 + TMarAug + Mar20 + fallTmean +
    MarTmin + maxT + Tmin + summerTmean + Pmean + minT + JanMin +
    TOctSep + Jan20 + PcumOctSep, data=df1)
bestModel <- mod
bestAIC <- AIC(mod)
vars <- c('beetleAcres','lon', 'lat', 'etopo1', 'density', 'PctLarge',
    'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP3',
    'vpd', 'cwd','maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd', 'AugTmean',
    'OctTmin', 'AugMaxT', 'AugTmax', 'MarMin', 'ddAugJun', 'ddAugJul',
    'JanTmin', 'PPT', 'summerP2', 'TMarAug', 'Mar20', 'fallTmean',
    'MarTmin', 'maxT', 'Tmin', 'summerTmean', 'Pmean', 'minT', 'JanMin',
    'TOctSep', 'Jan20', 'PcumOctSep')
cols <- vars[-1]
df2 <- df1[,vars]
out <- data.frame(f1=c(), f2=c(), p=c())
options(warn=-1)

for (f1 in 2:(ncol(df2) - 2)) {
  for (f2 in (f1 + 1):(ncol(df2) - 1)) {
  	mod.string <- paste0('lm(beetleAcres^0.07 ~ ', cols[f1], ':', cols[f2],', data=df2)')
    mod <- eval(parse(text=mod.string))
    interaction.p <- summary(mod)$coef[2, 4]
    if (interaction.p < 0.001) {
      row <- data.frame(
          f1=names(df2)[f1], f2=names(df2)[f2], p=interaction.p) 
      out <- rbind(out, row)
      print(paste('Selected interaction term:', paste0(cols[f1], ':', cols[f2])))
    }
  }
}

options(warn=0)
interactions <- out[order(out$p), ]
dim(interactions)
head(interactions)
hist(interactions$p, col=4)
rug(interactions$p)
par(mfrow=c(3, 3))
for (i in 1:nrow(interactions)) {
  f1 <- as.character(interactions[i, 1])
  f2 <- as.character(interactions[i, 2])
  has.data <- !(is.na(df2[, f1]) | is.na(df2[, f2]))
  plot(df2[has.data, f1] ~ df2[has.data, f2], 
       pch=16, 
       col=rgb(0, 0, 0, 0.01),
       ylab=f1,
       xlab=f2)
  lines(lowess(df2[has.data, f1] ~ df2[has.data, f2]), col=2)
}

interactions$f1f2 <- paste0(interactions$f1, ':', interactions$f2)

for(iter in 1:100){
	print(paste('Iteration', iter))
	print(paste('Best AIC:', bestAIC))
	print(paste('Best model:', bestModel$call[2]))
  terms <- sample(interactions$f1f2, 20)
  var.string <- function(){
    for (term in terms){
  		cat(sprintf('+ %s ', term))
  	}
  }
  if(iter > 1){
  	fixed <- as.character(mod$call)[2]
  	fixed <- gsub('\n ', '', fixed)
  	strings <- unlist(strsplit(unlist(strsplit(fixed, " ~ ", fixed = TRUE))[2], ' + ', fixed = TRUE))
  	repeated <- grep(':', strings, value=TRUE)
  	terms <- terms[!(terms %in% repeated)]
  	inters <- capture.output(var.string())
  	mod.string <- paste0('lm(', fixed, inters, ', data=df2)') 
  }else{
    fixed <- 'lon + lat + etopo1 + density + PctLarge +vcc + mfri + prs + 
  					pms + pls + GAP1 + GAP3 + vpd + cwd +maxAugT + summerP0 + Tmean + 
  					mi + Tvar + wd + AugTmean + OctTmin + AugMaxT + AugTmax + MarMin +
   					ddAugJun + ddAugJul +JanTmin + PPT + summerP2 + TMarAug + Mar20 + 
  					fallTmean + MarTmin + maxT + Tmin + summerTmean + Pmean + minT + 
  					JanMin + TOctSep + Jan20 + PcumOctSep'
  	inters <- capture.output(var.string())
		mod.string <- paste0('lm(beetleAcres^0.07 ~ ', fixed, inters, ', data=df2)')
	}
	mod <- eval(parse(text=mod.string))
	mod <- step(mod)
	if(AIC(mod) < bestAIC){
		bestModel <- mod
		bestAIC <- AIC(mod)
	}
	print(paste('Best AIC:', bestAIC))
	print(paste('Best model:', gsub('\n  ', '', mod$call[2])))
	print(paste('R squared', summary(mod)$r.squared))
	print(paste('Adjusted R squared', summary(mod)$adj.r.squared))
}

print('All done')
