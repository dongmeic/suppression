# data distribution, correlation, linear regression
library(rcompanion)
library(MASS)

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

Box <- boxcox(y ~ 1, lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1, y[y>0]
Cox <- data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 <- Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest
lambda <- Cox2[1, "Box.x"]                 # Extract that lambda
y.box <- (y ^ lambda - 1)/lambda   # Transform the original data

test <- y.lbda
hist(test)
shapiro.test(test)
plotNormalHistogram(test)
de

x = df.s$density # df.s$mStdAge # df.s$PctOld # df.s$PctLarge 
cor.test(test, x)

# average the beetle affected acres
df$allyears <- rowMeans(df[,6:25], na.rm=TRUE)
write.csv(df[,6:26], paste0(csvpath, "mpb10km_mpb_acres.csv"), row.names=FALSE)
write.csv(df, paste0(csvpath, "mpb10km_nonclimate.csv"), row.names=FALSE)

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
ndf <- cbind(df[,nonclm.predictors], bioclm[,clm.predictors])
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

