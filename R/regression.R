library(ggplot2)
library(plyr)
library(gvlma)
library(MASS)
require(car)
library(DataCombine)
require(pscl)

inpath <- "/Volumes/dongmeic-18/beetle/data/text/"
df.jurisd <- read.csv(paste0(inpath, "jurisditional_scaling_df.csv"))
df.raster <- read.csv(paste0(inpath, "raster_scaling_df.csv"))
df.firevt <- read.csv(paste0(inpath, "fire_event_scaling_df.csv"))

head(df.jurisd)
head(df.raster)
head(df.firevt)

# check jurisdictional levels
df.coef.jurisd <- ddply( df.jurisd[ df.jurisd$Jurisd %in% c('Ecoregions','Land ownerships','National forests','Sub-ecoregions'), ], 
       "Jurisd", function(df) coefficients(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDens+LogDist, data=df)))
# check rasterize scaling
df.coef.raster <- ddply( df.raster[ df.raster$CellSize %in% c(10000,25000,50000,100000,200000), ], 
                         "CellSize", function(df) coefficients(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDens+LogDist, data=df)))
# check fire event scales
df.coef.firevt <- ddply( df.firevt[ df.firevt$DistClass %in% c("1-30m","2-300m","3-3km","4-30km","5-300km"), ], 
                         "DistClass", function(df) coefficients(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDist, data=df)))

# ddply( df.jurisd[ df.jurisd$Jurisd %in% c('Ecoregions','Land ownerships','National Forests','Sub-ecoregions'), ], 
#                         "Jurisd", function(df) summary(gvlma(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDens+LogDist, data=df))))

# ddply( df.raster[ df.raster$CellSize %in% c(10000,25000,50000,100000,200000), ], 
#        "CellSize", function(df) summary(gvlma(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDens+LogDist, data=df))))

# ddply( df.firevt[ df.firevt$DistClass %in% c("1-30m","2-300m","3-3km","4-30km","5-300km"), ],
#        "DistClass", function(df) summary(gvlma(lm(LogMPB~LogCost+LogBA+LogSize+Duration+LogDist, data=df))))

## Regression diagnostics
data <- subset(df.jurisd, Jurisd=="National forests")
data <- data[-c(6,14,15),] # remove outliers in national forests
data <- subset(df.raster, CellSize==200000)
#data <- data[-c(5,777,883,28,353,1118,317,371,1098,129,219,560),]
#data <- data[-c(2264,2705,2795),]
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/boxcox.html
boxcox(MPBAcre ~ LogCost+LogSize+LogBA+Duration+LogDens+LogDist, data = data,
       lambda = seq(-0.25, 0.25, length = 10))
summary(a <- powerTransform(MPBAcre ~ LogCost+LogSize+LogBA+Duration+LogDens+LogDist, data = data, family="bcPower"))
boxcox(MPBAcre^0.1291 ~ LogCost+LogSize+LogBA+Duration+LogDens+LogDist, data = data)
fit <- lm(MPBAcre^0.1291 ~ LogCost+LogSize+LogBA+Duration+LogDens+LogDist, data=data)

# 1yr  3yr  5yr  7yr  pre 
# 2490 2110 1722 1498 2654 

# 1-30m  2-300m   3-3km  4-30km 5-300km 
# 365     447    1016    2727    5919 
data <- subset(df.firevt, DistClass=="1-30m")
data <- subset(df.firevt, YearClass!="pre" & DistClass=="5-300km")
#data <- data[data$MPBAcre!=0,]
#data <- data[-c(531,533,724),]
m1 <- lm(MPBAcre ~ LogCost+LogSize+LogBA+Duration+LogDist, data = data)
boxcox(MPBAcre ~ LogCost+LogSize+LogBA+Duration+LogDist, data = data, lambda = seq(-0.25, 0.25, length = 10))
summary(a <- powerTransform(MPBAcre ~ LogCost+LogSize+LogBA+Duration+LogDist, data = data, family="bcPower"))
fit <- lm(MPBAcre^0.1984 ~ LogCost+LogSize+LogBA+Duration+LogDist, data=data)
# data_1 <- data.frame(data, resid_mod=fit$residuals)
# data_2 <- slide(data_1, Var="resid_mod", NewVar = "lag", slideBy = -1)
# data_3 <- na.omit(data_2)
gvmodel <- gvlma(fit)
summary(gvmodel)
layout(matrix(c(1,2,3,4),2,2))
par(mar = rep(2, 4))
plot(fit)
dev.off()
dev.new()
anova(fit)
vcov(fit)
step <- stepAIC(fit, direction="both")
step$anova
# http://www.statmethods.net/stats/rdiagnostics.html
# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots
# Influential Observations
# added variable plots 
av.plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)
# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?
# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)
# Test for Autocorrelated Errors
durbinWatsonTest(fit)

