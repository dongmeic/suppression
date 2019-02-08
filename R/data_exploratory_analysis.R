# data distribution, correlation, linear regression
library(rcompanion)
library(MASS)
library(mgcv)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath, "mpb10km_nonclimate.csv"))
df.s <- subset(df, !is.na(allyears) & forest > 0)
#df.s <- subset(df, forest > 0)

# transformation of y
y = df.s$allyears
y.log = log(y)
hist(y.log)
y.tuk <- transformTukey(y,plotit=FALSE) # sample(y,5000)
y.lbda <- y ^ 0.05

Box <- boxcox(y[y>0] ~ 1, lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1, y[y>0]
Cox <- data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 <- Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest
lambda <- Cox2[1, "Box.x"]                 # Extract that lambda
y.box <- (y ^ lambda - 1)/lambda   # Transform the original data

test <- y.lbda
hist(test)
shapiro.test(test)
plotNormalHistogram(test)
qqnorm(test)
qqline(test)

x = df.s$density # df.s$mStdAge # df.s$PctOld # df.s$PctLarge 
cor.test(test, x)

# mean values of bioclimate data
bioclm <- read.csv(paste0(csvpath, "mpb10km_bioclm/mpb10km_bioclm_mean.csv"))
clm.predictors <- c('vpd', 'cwd', 'maxAugT', 'summerP0', 'Tmean', 'mi', 'Tvar', 'wd', 
										'AugTmean', 'OctTmin', 'AugMaxT', 'AugTmax', 'Acs', 'MarMin', 
										'ddAugJun', 'ddAugJul', 'JanTmin', 'PPT', 'summerP2', 'TMarAug', 
										'Mar20', 'fallTmean', 'MarTmin', 'maxT', 'Tmin', 'winterMin', 
										'summerTmean', 'Pmean', 'summerP1', 'minT', 'JanMin', 'TOctSep', 
										'Jan20', 'PcumOctSep')

# combine both dataset
nonclm.predictors <- c("lon", "lat", "etopo1", "x", "y", "allyears", 
												"vegetation", "forest", "mStdAge", "density", "PctLarge", "PctOld",
												"vcc", "mfri", "prs", "pms", "pls", "GAP", "GAP2", "GAP3")
fires <- read.csv(paste0(csvpath,"suppressed_fires.csv"))

ndf <- cbind(df[,nonclm.predictors], fires, bioclm[,clm.predictors])
ndf$GAP2 <- ifelse(ndf$GAP2 == 2, 1, 0)
ndf$GAP3 <- ifelse(ndf$GAP3 == 3, 1, 0)
colnames(ndf)[which(colnames(ndf)=='allyears')] <- 'beetleAcres'
colnames(ndf)[which(colnames(ndf)=='vegetation')] <- 'host'
colnames(ndf)[which(colnames(ndf)=='GAP')] <- 'GAP1'
write.csv(ndf, paste0(csvpath, "mpb10km_input_data.csv"), row.names=FALSE)
ndf.s <- subset(ndf, !is.na(beetleAcres) & forest > 0 & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
										 !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
										 !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
write.csv(ndf.s, paste0(csvpath, "mpb10km_input_data_cleaned.csv"), row.names=FALSE)

# modeling
indata <- read.csv(paste0(csvpath, "mpb10km_input_data_cleaned.csv"))
vars <- c('host', 'forest', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3', 'SprsSize')
for (var in vars){
	indata[,var] <- as.factor(indata[,var])
}

df <- indata[, -which(names(indata) %in% c('forest', 'x','y','host'))]
length(na.omit(df$SprsFires))

df1 <- df[complete.cases(df), ]
df2 <- as.data.frame(scale(df1[,-which(names(df1) %in% c('beetleAcres', vars))]))
df2 <- cbind(df1[,c('beetleAcres', 'vcc', 'mfri', 'prs', 'pms', 'pls', 'GAP1', 'GAP2', 'GAP3', 'SprsSize')], df2)

mod <- lm(beetleAcres^0.07 ~ ., data=df2)
summary(mod)
mod <- step(mod)

mod <- lm(
		beetleAcres^0.07 ~ (vcc + mfri + pms + pls + GAP1 + lat + etopo1 +
    density + PctLarge + SprsFires + PctSprs + vpd + cwd + Tmean +
    mi + wd + AugTmean + OctTmin + AugMaxT + AugTmax + MarMin +
    ddAugJun + ddAugJul + JanTmin + PPT + summerP2 + TMarAug +
    Mar20 + fallTmean + MarTmin + maxT + Tmin + summerTmean +
    Pmean + minT + JanMin + TOctSep + Jan20 + PcumOctSep)^2, data=df2)
    
gam.mod <- gam(beetleAcres ~ vcc + mfri + pms + pls + GAP1 + te(lat) + te(etopo1) +
    te(density) + te(PctLarge) + te(SprsFires) + te(PctSprs) + te(vpd) + te(cwd) + te(Tmean) +
    te(mi) + te(wd) + te(AugTmean) + te(OctTmin) + te(AugMaxT) + te(AugTmax) + te(MarMin) +
    te(ddAugJun) + te(ddAugJul) + te(JanTmin) + te(PPT) + te(summerP2) + te(TMarAug) +
    Mar20 + te(fallTmean) + te(MarTmin) + te(maxT) + te(Tmin) + te(summerTmean) +
    te(Pmean) + te(minT) + te(JanMin) + te(TOctSep) + Jan20 + te(PcumOctSep), data=df2)

par(mfrow=c(3, 3))
plot(gam.mod)
  
gam.mod <- gam(beetleAcres ~ vcc + mfri + pms + pls + GAP1 + s(lat) + s(etopo1) +
    s(density) + s(PctLarge) + s(SprsFires) + s(PctSprs) + s(vpd) + s(cwd) + s(Tmean) +
    s(mi) + s(wd) + s(AugTmean) + s(OctTmin) + s(AugMaxT) + s(AugTmax) + s(MarMin) +
    s(ddAugJun) + s(ddAugJul) + s(JanTmin) + s(PPT) + s(summerP2) + s(TMarAug) +
    Mar20 + s(fallTmean) + s(MarTmin) + s(maxT) + s(Tmin) + s(summerTmean) +
    s(Pmean) + s(minT) + s(JanMin) + s(TOctSep) + Jan20 + s(PcumOctSep), data=df2)    

mod <- lm(
		beetleAcres^0.07 ~ (lon + lat + etopo1 + density + PctLarge +
    vcc + mfri + prs + pms + pls + GAP1 + GAP3 + vpd + cwd +
    maxAugT + summerP0 + Tmean + mi + Tvar + wd + AugTmean +
    OctTmin + AugMaxT + AugTmax + MarMin + ddAugJun + ddAugJul +
    JanTmin + PPT + summerP2 + TMarAug + Mar20 + fallTmean +
    MarTmin + maxT + Tmin + summerTmean + Pmean + minT + JanMin +
    TOctSep + Jan20 + PcumOctSep)^2, data=df)

anova(mod)
TukeyHSD(aov(mod))
