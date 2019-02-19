library(mgcv)
# data cleaning

inpath <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables'
file <- 'mpb10km_data.csv'
data <- read.csv(paste0(inpath, "/", file))

# data cleaning method 1 & 2
drop <- c('x', 'y', 'SprsCosts', 'SprsAcres')
data <- data[, -which(names(data) %in% drop)]
# data cleaning method 3
sprs.vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 'SprsAcre', 'SprsDays', 'OutDays')
drops <- c('x', 'y', sprs.vars)
data <- data[, -which(names(data) %in% drops)]
head(data)

# data cleaning method 1
# vcc values > 6 indicate areas of no vegetation, replace with 0s
data$vcc[data$vcc > 6] <- 0
data$prs[data$prs > 20] <- 0
data$pms[data$pms > 20] <- 0
data$pls[data$pls > 20] <- 0

# Mean fire return interval where there are no trees is effectively 
# infinite--set to 22 (> 1000 years)
data$mfri[data$mfri > 22] <- 0

data$beetleAcres[is.na(data$beetleAcres)] <- 0
data$mStdAge[is.na(data$mStdAge) & data$forest == 0] <- 0
data$density[is.na(data$density) & data$forest == 0] <- 0
data$PctLarge[is.na(data$PctLarge) & data$forest == 0] <- 0
data$PctOld[is.na(data$PctOld) & data$forest == 0] <- 0

data$SprsCPA[is.na(data$SprsCPA) & data$forest == 0] <- 0
data$SprsFires[is.na(data$SprsFires) & data$forest == 0] <- 0
data$PctSprs[is.na(data$PctSprs) & data$forest == 0] <- 0
data$SprsAcre[is.na(data$SprsAcre) & data$forest == 0] <- 0
data$SprsDays[is.na(data$SprsDays) & data$forest == 0] <- 0
data$OutDays[is.na(data$OutDays) & data$forest == 0] <- 0

comp <- data[complete.cases(data), ]

for (field in names(data)) {
  cat(sprintf('%20s: %d\n', field, sum(is.na(data[, field]))))
}

comp <- comp[is.finite(comp$SprsCPA), ]
dim(comp)
write.csv(comp, sprintf('%s/mpb10km_data_clean.csv', inpath), row.names=FALSE)

# data cleaning method 2
data <- subset(data, !is.na(beetleAcres) & SprsCPA != Inf & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
         !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
         !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
comp <- data[complete.cases(data), ]
dim(comp)
write.csv(comp, sprintf('%s/mpb10km_data_clean_2.csv', inpath), row.names=FALSE)

# data cleaning method 3
data <- subset(data, !is.na(beetleAcres) & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
         !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
         !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
comp <- data[complete.cases(data), ]
dim(comp)
write.csv(comp, sprintf('%s/mpb10km_data_clean_3.csv', inpath), row.names=FALSE)

# exploratory data analysis
data <- read.csv(sprintf('%s/mpb10km_data_clean.csv', inpath))
head(data)

data <- subset(data, beetleAcres > 0)
head(data)
par(mfrow=c(2, 2))
hist(data$beetleAcres)
hist(log(data$beetleAcres))
hist(1 / data$beetleAcres)
hist(sqrt(data$beetleAcres))

summary(data$SprsCPA)
par(mfrow=c(1, 1))
hist(log(data$SprsCPA + 1))
data$SprsCPA <- log(data$SprsCPA + 1)
names(data)[which(names(data) == 'SprsCPA')] <- 'logSprsCPA.p1'

for (field in names(data)) {
  if (field != 'beetleAcres') {
    cat(sprintf('te(%s) + ', field))   
  }
}

mod <- gam(
  log(beetleAcres) ~ te(lon) + te(lat) + te(etopo1) + host + te(mStdAge)
    + te(density) + te(PctLarge) + PctOld + te(vcc) + te(mfri) 
    + te(prs) + GAP1 + te(vpd) + te(cwd) + te(maxAugT) + te(summerP0) 
    + te(Tmean) + te(mi) + te(Tvar) + te(wd) + te(AugTmean) + te(OctTmin) 
    + te(AugMaxT) + te(Acs) + te(MarMin) + te(ddAugJul) + te(JanTmin) 
    + te(PPT) + te(summerP2) + te(TMarAug) + te(Mar20) + te(fallTmean) 
    + te(MarTmin) + te(maxT) + te(Tmin) + te(winterMin) + te(summerTmean) 
    + te(Pmean) + te(summerP1) + te(minT) + te(JanMin) + te(TOctSep) 
    + te(PcumOctSep) + te(logSprsCPA.p1) + te(SprsFires) + te(PctSprs) 
    + SprsAcre + te(SprsDays) + te(OutDays),
  data=data)

smoothed <- c(
  'lon', 'lat', 'etopo1', 'mStdAge', 'density', 'PctLarge', 'vcc', 'mfri',
  'prs', 'vpd', 'cwd', 'maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd',
  'AugTmean', 'OctTmin', 'AugMaxT', 'Acs', 'MarMin', 'ddAugJul', 'JanTmin',
  'PPT', 'summerP2', 'TMarAug', 'Mar20', 'fallTmean', 'MarTmin', 'maxT', 
  'Tmin', 'winterMin', 'summerTmean', 'Pmean', 'summerP1', 'minT', 
  'JanMin', 'TOctSep', 'PcumOctSep', 'logSprsCPA.p1', 'SprsFires', 
  'PctSprs', 'SprsDays', 'OutDays')

out <- '/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/'
pdf(paste0(out, "suppression_plots_smoothed.pdf"))  
par(mfrow=c(3, 3))
for (field in smoothed) {
  plot(log(data$beetleAcres) ~ data[, field], 
       pch=16, cex=0.25,
       col=rgb(0, 0, 0, 0.2),
       xlab=field, ylab='log(beetleAcres)')
  lines(lowess(log(data$beetleAcres) ~ data[, field]), col=2, lwd=2)
}
dev.off()

for (field in names(data)) {
  if (field != 'beetleAcres') {
    cat(sprintf('%s + ', field))
  }
}

indata <- data
sq_terms <- c('lon', 'lat', 'etopo1', 'mStdAge', 'density', 'PctLarge', 
							'prs', 'vpd', 'maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd',
							'AugMaxT', 'Acs', 'ddAugJul', 'PPT', 'summerP2', 'maxT', 
							'Pmean', 'summerP1', 'JanMin', 'PcumOctSep', 'logSprsCPA.p1', 'PctSprs')
exp_terms <- c('Tmean', 'OctTmin', 'fallTmean', 'TOctSep')
logp1_terms <- c('vcc', 'mfri', 'Mar20', 'OutDays')

# without scaling predictors
mod <- lm(
  log(beetleAcres) ~ lon + I(lon^2) + lat + I(lat^2) + etopo1 
    + I(etopo1^2) + host + forest + mStdAge + I(mStdAge^2) + density 
    + I(density^2) + PctLarge + I(PctLarge^2) + PctOld + vcc + log(vcc + 1)
    + mfri + log(mfri+1) + prs + I(prs^2) + pms + pls + GAP1 + GAP2 + GAP3 
    + vpd + I(vpd^2) + cwd + maxAugT + I(maxAugT^2) + summerP0 
    + I(summerP0^2) + Tmean + I(Tmean^2) + exp(Tmean) + mi + I(mi^2) + Tvar 
    + I(Tvar^2) + wd + I(wd^2) + AugTmean + OctTmin + exp(OctTmin) 
    + AugMaxT + I(AugMaxT^2) + AugTmax + Acs + I(Acs^2) + MarMin + ddAugJun
    + ddAugJul + I(ddAugJul^2) + JanTmin + PPT + I(PPT^2) + summerP2 
    + I(summerP2^2) + TMarAug + exp(TMarAug) + Mar20 + log(Mar20 + 1) 
    + fallTmean + exp(fallTmean) + MarTmin + maxT + I(maxT^2) + Tmin 
    + winterMin + summerTmean + Pmean + I(Pmean^2) + summerP1 
    + I(summerP1^2) + minT + JanMin + I(JanMin^2) + TOctSep + exp(TOctSep)
    + Jan20 + PcumOctSep + I(PcumOctSep^2) + logSprsCPA.p1 
    + I(logSprsCPA.p1^2) + SprsFires + PctSprs + I(PctSprs^2) + SprsAcre 
    + SprsDays + OutDays + log(OutDays + 1),
  data=data)

for(var in sq_terms){
	varnm <- paste0(var, '_sq')
	indata[,varnm] <- (indata[,var])^2
	cat(sprintf('Calculated %s in a squared term...\n', var))
}

for(var in exp_terms){
	varnm <- paste0(var, '_exp')
	indata[,varnm] <- exp(indata[,var])
	cat(sprintf('Calculated %s in a exponential term...\n', var))
}

for(var in logp1_terms){
	varnm <- paste0(var, '_logp1')
	indata[,varnm] <- log(indata[,var]+1)
	cat(sprintf('Calculated %s in a logarithm term...\n', var))
}

drop <- "beetleAcres"
predictors <- indata[ , !(names(indata) %in% drop)]
predictors <- scale(predictors)
df <- data.frame(beetleAcres=data[,drop], predictors)
#indata$beetleAcres <- log(indata$beetleAcres)
#df <- data.frame(scale(indata))
df$beetleAcres <- log(df$beetleAcres)

#mod <- lm(log(beetleAcres) ~ ., data=df)
mod <- lm(beetleAcres ~ ., data=df)

# modeling
mod.r <- step(mod, trace=0)
summary(mod.r)

par(mfrow=c(2, 2))
plot(mod.r)
which(rownames(data) == 6118)
mod2 <- update(mod, . ~ ., data=data[-1404, ])
#mod2 <- update(mod, . ~ ., data=df[-1404, ])
mod2.r <- step(mod2, trace=0)

summary(mod2.r)
par(mfrow=c(2, 2))
plot(mod2.r)

data <- data[-1404, ]
#data <- df[-1404, ]
data$preds <- mod2.r$fitted

pdf(paste0(out, "suppression_var_plots.pdf"))
par(mfrow=c(3, 3))
for (field in names(data)) {
  if (field != 'beetleAcres') {
    plot(data$preds ~ data[, field], 
         pch=16, cex=0.25,
         col=rgb(0, 0, 0, 0.2), 
         xlab=field,
         ylab='log(Predicted Beetle Acres)')
    lines(lowess(data$preds ~ data[, field]), col=2, lwd=2)
  }
}
dev.off()

med.df <- data.frame(t(apply(data, 2, median)))
n.steps <- 100
med.df <- med.df[rep(1, n.steps), ]

pdf(paste0(out, "suppression_2D_plots_log.pdf"))
par(mfrow=c(3, 3))
for (field in names(med.df)[!(names(med.df) %in% grep('_', names(med.df), value=T))]) {
  if (!(field %in% c('preds', 'beetleAcres'))) {
    test.df <- med.df
    xmin <- quantile(data[, field], probs=0.025)
    xmax <- quantile(data[, field], probs=0.975)
    test.df[, field] <- seq(xmin, xmax, length=100)
    preds <- predict(mod2.r, newdata=test.df)
    plot(preds ~ test.df[, field], 
         type='l', 
         xlab=field, 
         ylab='log(beetleAcre) predicted')
  }
}
dev.off()

pdf(paste0(out, "suppression_2D_plots.pdf"))
par(mfrow=c(3, 3))
for (field in names(med.df)[!(names(med.df) %in% grep('_', names(med.df), value=T))]) {
  if (!(field %in% c('preds', 'beetleAcres'))) {
    test.df <- med.df
    xmin <- quantile(data[, field], probs=0.025)
    xmax <- quantile(data[, field], probs=0.975)
    test.df[, field] <- seq(xmin, xmax, length=100)
    preds <- exp(predict(mod2.r, newdata=test.df))
    plot(preds ~ test.df[, field], 
         type='l', 
         xlab=field, 
         ylab='beetleAcre predicted')
  }
}
dev.off()











