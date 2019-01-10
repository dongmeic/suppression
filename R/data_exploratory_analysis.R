# data distribution, correlation, linear regression
library(rcompanion)
library(MASS)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
df <- read.csv(paste0(csvpath, "mpb10km_nonclimate.csv"))
df.s <- subset(df, allyears > 0 & forest > 0)

# transformation of y
y = df.s$allyears
y.log = log(df.s$allyears)
hist(y)
y.tuk <- transformTukey(y,plotit=FALSE)
y.lbda <- y ^ 0.1

Box <- boxcox(y ~ 1, lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
Cox <- data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 <- Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest
lambda <- Cox2[1, "Box.x"]                 # Extract that lambda
y.box <- (y ^ lambda - 1)/lambda   # Transform the original data

test <- y.lbda
hist(test)
shapiro.test(test)
plotNormalHistogram(test)
qqnorm(test,ylab="Sample Quantiles for y")
qqline(test,col="red")

x = df.s$density # df.s$mStdAge # df.s$PctOld # df.s$PctLarge 
cor.test(test, x)

