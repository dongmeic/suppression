library(mgcv)
# data cleaning

inpath <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
file <- 'mpb10km_data.csv'
data <- read.csv(paste0(inpath, "/", file))

drop <- c('x', 'y', 'SprsCosts', 'SprsAcres')
data <- data[, -which(names(data) %in% drop)]
# vcc values > 6 indicate areas of no vegetation, replace with 0s
data$vcc[data$vcc > 6] <- NA
data$prs[data$prs > 20] <- NA
data$pms[data$pms > 20] <- NA
data$pls[data$pls > 20] <- NA

# Mean fire return interval where there are no trees is effectively 
# infinite--set to 22 (> 1000 years)
data$mfri[data$mfri > 22] <- NA

comp <- data[complete.cases(data), ]
comp <- comp[is.finite(comp$SprsCPA), ]
data <- comp

data$SprsCPA <- log(data$SprsCPA + 1)
names(data)[which(names(data) == 'SprsCPA')] <- 'logSprsCPA.p1'

sq_terms <- c('lon', 'lat', 'etopo1', 'mStdAge', 'density', 'PctLarge', 
							'prs', 'vpd', 'maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd',
							'AugMaxT', 'Acs', 'ddAugJul', 'PPT', 'summerP2', 'maxT', 
							'Pmean', 'summerP1', 'JanMin', 'PcumOctSep', 'logSprsCPA.p1', 'PctSprs')
exp_terms <- c('Tmean', 'OctTmin', 'fallTmean', 'TOctSep')
logp1_terms <- c('vcc', 'mfri', 'Mar20', 'OutDays')

# add transformation
for(var in sq_terms){
	varnm <- paste0(var, '_sq')
	data[,varnm] <- (data[,var])^2
	cat(sprintf('Calculated %s in a squared term...\n', var))
}

for(var in exp_terms){
	varnm <- paste0(var, '_exp')
	data[,varnm] <- exp(data[,var])
	cat(sprintf('Calculated %s in a exponential term...\n', var))
}

for(var in logp1_terms){
	varnm <- paste0(var, '_logp1')
	data[,varnm] <- log(data[,var]+1)
	cat(sprintf('Calculated %s in a logarithm term...\n', var))
}

data$beetleAcres <- log(data$beetleAcres)
drop <- "beetleAcres"
predictors <- data[ , !(names(data) %in% drop)]
predictors <- scale(predictors)
df <- data.frame(beetleAcres=data[,drop], predictors)

lmodel <- lm(beetleAcres ~ ., data=df)
mod <- step(lmodel, trace=0)
lboot <- lm.boot(lmodel, R = 1000)

