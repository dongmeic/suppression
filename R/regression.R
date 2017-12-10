library(ggplot2)
library(plyr)
library(lmtest)
library(caret)
library(gvlma)
library(MASS)
require(car)
library(DataCombine)
require(pscl)

# read data
# inpath <- "/Volumes/dongmeic/beetle/data/text/"
inpath <- "/Users/dongmeichen/Documents/writing/fire suppression/data/"
outpath <- "/Users/dongmeichen/Documents/writing/fire suppression/output/"
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
#data <- data[-c(6,14,15),] # remove outliers in national forests
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

############################################# remove area burned and fire size ##################################################
## check coefficients
# check jurisdictional levels
df.coef.jurisd <- ddply( df.jurisd[ df.jurisd$Jurisd %in% c('National forests','Ecoregions','Sub-ecoregions','Land ownerships'), ], 
                         "Jurisd", function(df) coefficients(lm(LogMPB~LogCost+Duration+LogDens+LogDist, data=df)))
# check rasterize scaling
df.coef.raster <- ddply( df.raster[ df.raster$CellSize %in% c(10000,25000,50000,100000,200000), ], 
                         "CellSize", function(df) coefficients(lm(LogMPB~LogCost+Duration+LogDens+LogDist, data=df)))
# check fire event scales
df.coef.firevt <- ddply( df.firevt[ df.firevt$DistClass %in% c("1-30m","2-300m","3-3km","4-30km","5-300km"), ], 
                         "DistClass", function(df) coefficients(lm(LogMPB~LogCost+Duration+LogDist, data=df)))

## Regression diagnostics
# repeat mannually for the results at different scales
data <- subset(df.jurisd, Jurisd=="Sub-ecoregions")
data <- subset(df.raster, CellSize==10000)
data <- subset(df.firevt, YearClass == "1yr" & DistClass=="5-300km")

# http://rstatistics.net/how-to-test-a-regression-model-for-heteroscedasticity-and-if-present-how-to-correct-it/
# method 1
lambda <- BoxCoxTrans(data$MPBAcre)
data <- cbind(data, mpbAcreNew=predict(lambda,data$MPBAcre))
fit <- lm(mpbAcreNew ~ LogCost+Duration+LogDens+LogDist, data=data)

# method 2 (selected)
summary(a <- powerTransform(MPBAcre ~ LogCost+Duration+LogDens+LogDist, data = data, family="bcPower"))
boxcox(MPBAcre^0.0485 ~ LogCost+Duration+LogDens+LogDist, data = data)
# jurisdictional 0.1402, 0.1227, 0.0796, 0.0931
# raster 0.0485,0.085,0.097,0.1038,0.1097
fit <- lm(MPBAcre^0.0485 ~ LogCost+Duration+LogDens+LogDist, data=data)

# fire-event scale
# method 1
lambda <- BoxCoxTrans(data$MPBAcre)
data <- cbind(data, mpbAcreNew=predict(lambda,data$MPBAcre))
boxcox(mpbAcreNew ~ LogCost+Duration+LogDist, data = data)
fit <- lm(mpbAcreNew ~ LogCost+Duration+LogDist, data=data)

# method 2 (selected)
summary(a <- powerTransform(MPBAcre ~ LogCost+Duration+LogDist, data = data, family="bcPower"))
boxcox(MPBAcre^0.2034 ~ LogCost+Duration+LogDist, data = data)
# fire-event 0.0652
fit <- lm(MPBAcre^0.2034 ~ LogCost+Duration+LogDist, data=data)

bptest(fit)
gvmodel <- gvlma(fit)
summary(gvmodel)
layout(matrix(c(1,2,3,4),2,2))
par(mar = rep(2, 4))
plot(fit)
dev.off()
dev.new()
par(mfrow=c(1,1))

########################################## fire-event scale #####################################
png(paste0(outpath, "event_ggplot_cost_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = LogCost, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Fire suppression cost (log)',
       y = 'MPB affected area (log)')
dev.off()

png(paste0(outpath, "event_ggplot_dura_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = Duration, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Containment duration (days)',
       y = 'MPB affected area (log)')
dev.off()

png(paste0(outpath, "event_ggplot_size_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = LogSize, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Fire size (log)',
       y = 'Containment duration (days)')
dev.off()

png(paste0(outpath, "event_ggplot_size_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = LogSize, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Fire size (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "event_ggplot_dist_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = LogDist, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Distance to WUI (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "event_ggplot_dist_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = subset(df.firevt, YearClass != "pre"),
       aes(x = LogDist, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_grid(DistClass ~ YearClass) +
  labs(x = 'Distance to WUI (log)',
       y = 'Containment duration (days)')
dev.off()

####################################### rasterized grid ############################################
png(paste0(outpath, "raster_ggplot_dens_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = LogDens, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Fire density (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "raster_ggplot_dens_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = LogDens, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Fire density (log)',
       y = 'Containment duration (days)')
dev.off()

png(paste0(outpath, "raster_ggplot_dist_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = LogDist, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Distance to WUI (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "raster_ggplot_dist_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = LogDist, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Distance to WUI (log)',
       y = 'Containment duration (days)')
dev.off()

png(paste0(outpath, "raster_ggplot_cost_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = LogCost, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Fire suppression cost (log)',
       y = 'MPB affected area (log)')
dev.off()

png(paste0(outpath, "raster_ggplot_dura_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.raster,
       aes(x = Duration, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ CellSize) +
  labs(x = 'Containment duration (days)',
       y = 'MPB affected area (log)')
dev.off()

####################################### jurisdictional ############################################
png(paste0(outpath, "jurisd_ggplot_dens_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = LogDens, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Fire density (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "jurisd_ggplot_dens_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = LogDens, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Fire density (log)',
       y = 'Containment duration (days)')
dev.off()

png(paste0(outpath, "jurisd_ggplot_dist_cost.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = LogDist, y=LogCost)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Distance to WUI (log)',
       y = 'Fire suppression cost (log)')
dev.off()

png(paste0(outpath, "jurisd_ggplot_dist_dura.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = LogDist, y=Duration)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Distance to WUI (log)',
       y = 'Containment duration (days)')
dev.off()

png(paste0(outpath, "jurisd_ggplot_cost_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = LogCost, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Fire suppression cost (log)',
       y = 'MPB affected area (log)')
dev.off()

png(paste0(outpath, "jurisd_ggplot_dura_btl.png"), width=8, height=6, units="in", res=300)
ggplot(data = df.jurisd,
       aes(x = Duration, y=LogMPB)) +
  geom_point(color="grey70") + geom_smooth(se = FALSE, method = "loess") + 
  facet_wrap( ~ Jurisd) +
  labs(x = 'Containment duration (days)',
       y = 'MPB affected area (log)')
dev.off()

####################################### conceptual models ##########################################
data <- subset(df.jurisd, Jurisd=="Ecoregions")
data <- subset(df.raster, CellSize==10000)
data <- subset(df.firevt, YearClass == "1yr" & DistClass=="5-300km")

plot(data$LogCost, data$LogMPB)
size.cost <- locator(n = 512, type = "n")
size.dura <- locator(n = 512, type = "n")
dens.cost <- locator(n = 512, type = "n")
dens.dura <- locator(n = 512, type = "n")
dist.cost <- locator(n = 512, type = "n")
dist.dura <- locator(n = 512, type = "n")
cost.mpb <- locator(n = 512, type = "n")
dura.mpb <- locator(n = 512, type = "n")
force.l <- locator(n = 512, type = "n")
force.s <- locator(n = 512, type = "n")

png(paste0(outpath,"conceptual_models.png"), width = 9, height = 6, units = "in", res = 300)
par(mfrow=c(3,3),mar=c(2,2,2,1))

plot(spline(size.cost$x, size.cost$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Fire size", line=0.5, cex.lab=1.2)
title(ylab="Containment cost", line=0.5, cex.lab=1.2)
mtext("a")

plot(spline(size.dura$x, size.dura$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Fire size", line=0.5, cex.lab=1.2)
title(ylab="Containment duration", line=0.5, cex.lab=1.2)
mtext("b")

plot(spline(cost.mpb$x, cost.mpb$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Containment cost", line=0.5, cex.lab=1.2)
title(ylab="Beetle outbreaks", line=0.5, cex.lab=1.2)
mtext("c")

plot(spline(dens.cost$x, dens.cost$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Suppression density", line=0.5, cex.lab=1.2)
title(ylab="Containment cost", line=0.5, cex.lab=1.2)
mtext("d")

plot(spline(dens.dura$x, dens.dura$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Suppression density", line=0.5, cex.lab=1.2)
title(ylab="Containment duration", line=0.5, cex.lab=1.2)
mtext("e")

plot(spline(dura.mpb$x, dura.mpb$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Containment duration", line=0.5, cex.lab=1.2)
title(ylab="Beetle outbreaks", line=0.5, cex.lab=1.2)
mtext("f")

plot(spline(dist.cost$x, dist.cost$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Distance to WUI", line=0.5, cex.lab=1.2)
title(ylab="Containment cost", line=0.5, cex.lab=1.2)
mtext("g")

plot(spline(dist.dura$x, dist.dura$y), axes = FALSE, type ='l', ann = F, lwd=2)
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Distance to WUI", line=0.5, cex.lab=1.2)
title(ylab="Containment duration", line=0.5, cex.lab=1.2)
mtext("h")

plot(spline(force.l$x, force.l$y), axes = FALSE, type ='l', ann = F, lwd=2)
lines(force.s$x, force.s$y, lwd=2, col="grey")
legend(16,6,c("Large fires","Small fires"),cex=1.1,lty=c(1,1),lwd=c(2,2),col=c("black","grey"),bty = "n")
u <- par("usr") 
arrows(u[1], u[3], u[2], u[3], length = 0.1, code = 2, xpd = TRUE) 
arrows(u[1], u[3], u[1], u[4], length = 0.1, code = 2, xpd = TRUE)
title(xlab="Containment cost/duration", line=0.5, cex.lab=1.2)
title(ylab="Suppression force", line=0.5, cex.lab=1.2)
mtext("i")
dev.off()

####################################### simple linear regression ####################################
# test results; since results are similar to multiple linear regression, I decided not to use SLR
data <- subset(df.jurisd, Jurisd=="Ecoregions")
# Ecoregions/Land ownerships/National forests/Sub-ecoregions
data <- subset(df.raster, CellSize==10000)
# 10000 25000 50000 100000 200000
# check line 122 for more details
summary(a <- powerTransform(MPBAcre ~ LogCost, data = data, family="bcPower"))
boxcox(MPBAcre^0.0469 ~ LogCost, data = data)
# jurisdictional 0.1407
# raster 0.0469
fit <- lm(MPBAcre^0.0469 ~ LogCost, data=data)

bptest(fit)
gvmodel <- gvlma(fit)
summary(gvmodel)
layout(matrix(c(1,2,3,4),2,2))
par(mar = rep(2, 4))
plot(fit)
dev.off()
dev.new()
par(mfrow=c(1,1))
