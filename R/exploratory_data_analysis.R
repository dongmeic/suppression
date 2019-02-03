library(rcompanion)
library(MASS)
library(mgcv)
library(fmsb)

csvpath <- "/Users/dongmeichen/Documents/data/ABM/"
df <- read.csv(paste0(csvpath, "mpb10km_input_data.csv"))
#df.s <- subset(df, !is.na(beetleAcres) & forest > 0)
df.s <- subset(df, !is.na(beetleAcres) & !is.na(density) & mStdAge > 0)

# transformation of y
y = df.s$beetleAcres
y.log = log(y)
hist(y.log)
y.tuk <- transformTukey(sample(y,5000),plotit=FALSE) # sample(y,5000)
y.lbda <- y ^ 0.05

test <- y.lbda
hist(test)
shapiro.test(sample(test,5000)) #sample(test,5000)
plotNormalHistogram(test)
qqnorm(test)
qqline(test)

x = df.s$density # df.s$mStdAge # df.s$PctOld # df.s$PctLarge 
cor.test(test, x)

mod <- glm(beetleAcres^0.05 ~ mStdAge + density + Tmean + Pmean, data=df, family = gaussian())
NagelkerkeR2(mod)
summary(mod)
ypred <- predict(mod, df, type="response")
ndf <- data.frame(x=df[,'Pmean'], y=ypred)
ndf <- ndf[order(ndf$x),]
plot(ndf$x, ndf$y, pch=16, cex=0.25, col=rgb(0,0,0,0.5), main='Pmean',xlab='',ylab='')
