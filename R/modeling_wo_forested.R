
# input data
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ndf <- read.csv(paste0(csvpath, "mpb10km_input_data.csv"))
vars <- c('host', 'forest', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3')
for (var in vars){
	ndf[,var] <- as.factor(ndf[,var])
}
indata <- subset(ndf, !is.na(beetleAcres) & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
										 !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
										 !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
df <- indata[, -which(names(indata) %in% c('x','y'))]
df1 <- df[complete.cases(df), ]
mod <- lm(beetleAcres^0.07 ~ ., data=df1)
summary(mod)
mod <- step(mod)	
mod <- lm(
		beetleAcres^0.07 ~ lon + etopo1 + forest + density + PctOld +
    vcc + mfri + prs + pms + pls + GAP1 + GAP3 + vpd + cwd +
    maxAugT + summerP0 + Tmean + mi + Tvar + wd + AugTmean +
    OctTmin + AugMaxT + AugTmax + Acs + MarMin + ddAugJun + ddAugJul +
    JanTmin + PPT + TMarAug + Mar20 + fallTmean + MarTmin + maxT +
    Tmin + summerTmean + Pmean + minT + JanMin + TOctSep + Jan20 +
    PcumOctSep, data=df1)
summary(mod)
fit <- glm(beetleAcres^0.07 ~ lon + etopo1 + forest + density + PctOld +
    vcc + mfri + prs + pms + pls + GAP1 + GAP3 + vpd + cwd +
    maxAugT + summerP0 + Tmean + mi + Tvar + wd + AugTmean +
    OctTmin + AugMaxT + AugTmax + Acs + MarMin + ddAugJun + ddAugJul +
    JanTmin + PPT + TMarAug + Mar20 + fallTmean + MarTmin + maxT +
    Tmin + summerTmean + Pmean + minT + JanMin + TOctSep + Jan20 +
    PcumOctSep,data=df1,family=gaussian())
    
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
 