library(rcompanion)
library(MASS)
library(mgcv)
library(fmsb)
library(classInt)
library(RColorBrewer)
library(rgdal)
library(ggplot2)
library(pvclust)
library(mclust)
library(cluster)
library(fpc)
library(spgwr)
library(maptools)
library(GWmodel)
library(e1071)

# csvpath <- "/Users/dongmeichen/Documents/data/ABM/"
csvpath <- "/Users/dongmeichen/Documents/beetle/data/"
df <- read.csv(paste0(csvpath, "mpb10km_data.csv"))

data <- read.csv(paste0(csvpath, "mpb10km_data.csv"))
drop <- c('x', 'y', 'SprsCosts', 'SprsAcres')
#drop <- c('SprsCosts', 'SprsAcres') # run again to get x y
data <- data[, -which(names(data) %in% drop)]
head(data)

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
comp <- comp[is.finite(comp$SprsCPA), ]
dim(comp)
# comp <- subset(comp, beetleAcres > 0)
# xy <- comp[,c("x", "y")]

mydata.raw <- subset(comp, beetleAcres > 0)
mydata.raw.xy <- cbind(xy, mydata.raw)
mydata <- data.frame(scale(mydata.raw))
head(mydata)

# check beetle affected acres
df.forest <- subset(df, forest==1)
df.beetle <- subset(df,!is.na(beetleAcres))
par(mfrow=c(1,2),mar=c(3,3,3,0))
hist(df.forest$beetleAcres)
hist(df.beetle$beetleAcres)
#df.s <- subset(df, !is.na(beetleAcres) & forest > 0)
#df.s <- subset(df, !is.na(beetleAcres) & !is.na(density) & mStdAge > 0)

# transformation of y
y = df.beetle$beetleAcres
y.log = log(y)
hist(y.log)
y.tuk <- transformTukey(sample(y,5000),plotit=FALSE) # sample(y,5000)
y.lbda <- y ^ 0.025

test <- y.lbda
hist(test)
shapiro.test(sample(test,5000)) #sample(test,5000)
plotNormalHistogram(test)
qqnorm(test)
qqline(test)

x = df.beetle$density # df.beetle$mStdAge # df.beetle$PctOld # df.beetle$PctLarge 
cor.test(test, x)

mod <- glm(beetleAcres^0.025 ~ mStdAge + density + Tmean + Pmean, data=df.beetle, family = gaussian())
NagelkerkeR2(mod)
summary(mod)
ypred <- predict(mod, df, type="response")
ndf <- data.frame(x=df[,'Pmean'], y=ypred)
ndf <- ndf[order(ndf$x),]
plot(ndf$x, ndf$y, pch=16, cex=0.25, col=rgb(0,0,0,0.5), main='Pmean',xlab='',ylab='')

# cluster analysis using all available variables
drops <- c("lon", "lat", "etopo1", "x", "y", "host", "forest", "GAP1", "GAP2", "GAP3") #, "beetleAcres"
#df$beetleAcres <- log(df$beetleAcres)
mydata <- df[,!(names(df) %in% drops)]
mydata <- mydata1
mydata <- subset(mydata, !is.na(beetleAcres) & SprsCPA != Inf & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
         !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
         !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
mydata.raw <- na.omit(mydata)
mydata.raw.xy <- mydata.raw
mydata <- data.frame(scale(mydata.raw))

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
par(mfrow=c(1,1),mar=c(5,4.5,2,1))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# cluster analysis after removing the fire suppression data
drops <- c("lon", "lat", "etopo1", "x", "y", "host", "forest", "GAP1", "GAP2", "GAP3")
sprs.vars <- c('SprsCosts', 'SprsAcres', 'SprsCPA', 'SprsFires', 'PctSprs', 'SprsAcre', 'SprsDays', 'OutDays')
drops.more <- c(drops, sprs.vars)
mydata <- df[,!(names(df) %in% drops.more)]
mydata <- subset(mydata, !is.na(beetleAcres) & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
                   !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
                   !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
mydata.raw <- na.omit(mydata)
mydata <- data.frame(scale(mydata.raw))

# cluster analysis with only fire suppression data
vgt.vars <- c("mStdAge", "density", "PctLarge", "PctOld", "vcc", "mfri", "prs", "pms", "pls")
mydata <- df[,(names(df) %in% c('beetleAcres', vgt.vars,sprs.vars))]
mydata1 <- df[,(names(df) %in% c('x','y','beetleAcres', vgt.vars,sprs.vars))]

# K-Means Cluster Analysis
ncluster <- 2
fit <- kmeans(mydata, ncluster) # cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata.upated <- data.frame(mydata.raw, fit$cluster) 
head(mydata.upated)
# show the fire suppression variables
nclr <- ncluster
plotclr <- brewer.pal(nclr,"Set1") # "#E41A1C" "#377EB8" "#4DAF4A"
plotclr <- c("#E41A1C", "#4DAF4A")
#plotclr <- plotclr[nclr:1] # reorder colors
ylabs <- c('Suppression costs', 'Suppression acres', 'Costs per acre', 'No. fires suppressed',
            'Percent of fires suppressed', 'Fire size suppressed', 'Containment duration', 'Fire out duration')
for (var in sprs.vars){
   ggplot(mydata.upated, aes(x=as.factor(fit.cluster), y=PctSprs, fill=as.factor(fit.cluster)))+
    scale_fill_manual(values = plotclr) +
    geom_boxplot()+labs(title='', x="Cluster", y = ylabs[which(vars==var)])+
    theme_classic() + theme(legend.position="none")
}
# without fire suppression variables
ggplot(mydata.upated, aes(x=as.factor(fit.cluster), y=PctOld, fill=as.factor(fit.cluster)))+
  scale_fill_manual(values = plotclr) +
  geom_boxplot()+labs(title='', x="Cluster", y = 'PctOld')+
  theme_classic() + theme(legend.position="none")

# get coordinate information
# with fire suppression variables
data <- subset(df, !is.na(beetleAcres) & SprsCPA != Inf & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
                   !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
                   !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))

# without fire suppression variables
sdf <- df[,!(names(df) %in% sprs.vars)]
data <- subset(sdf, !is.na(beetleAcres) & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
                 !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
                 !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))

data <- na.omit(data)
data <- data.frame(fit$cluster, data[,c("lon", "lat", "etopo1", "x", "y")])
head(data)

plotvar <- data$fit.cluster
plotvar <- fit$cluster
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

mpb10km_us_line <- readOGR(dsn='/Users/dongmeichen/Documents/beetle/shp', 
                           layer='mpb10km_us_line')
mpb_projstr <- "+proj=laea +lon_0=-112.5 +lat_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj4string(mpb10km_us_line) <- mpb_projstr

par(mfrow=c(1,1),mar=c(0,0,0,0))
plot(mpb10km_us_line)
points(data$x, data$y, pch=16, col=colcode, cex=0.3)
points(mydata.raw.xy$x, mydata.raw.xy$y, pch=16, col=colcode, cex=0.3)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
par(mfrow=c(1,1),mar=c(1,0,2,0))
plot(fit) # display dendogram
groups <- cutree(fit, k=ncluster) # cut tree into n clusters
# draw dendogram with red borders around the n clusters
rect.hclust(fit, k=ncluster, border="red") 

# Ward Hierarchical Clustering with Bootstrapped p values
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 

# Model Based Clustering
fit <- Mclust(mydata)
#plot(fit) # plot results
summary(fit) # display the best model 

# K-Means Clustering with n clusters
fit <- kmeans(mydata, ncluster)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
par(mfrow=c(1,1),mar=c(5,4.5,2,1))
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(mydata, fit$cluster)

# geographically weighted regression
model1 <- glm(beetleAcres^0.07 ~ mStdAge+density+PctLarge+PctOld+vcc+mfri+prs+pms+pls, data=mydata.raw, family=gaussian())
summary(model1)
plot(model1, which=3)
resids<-residuals(model1)
colours <- brewer.pal(4,"Set1")
map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(data$x,data$y)) 
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=0.3) 

# +density+PctLarge+PctOld+vcc+mfri+prs+pms+pls
GWRbandwidth <- gwr.sel(beetleAcres^0.07 ~ mStdAge, 
                        data=mydata.raw, coords=cbind(data$x,data$y),adapt=T)
gwr.model = gwr(beetleAcres^0.07 ~ mStdAge, 
                data=mydata.raw, coords=cbind(data$x,data$y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
gwr.model
mydata.gwr <- cbind(mydata.raw, data[,c('x', 'y')])
results <- as.data.frame(gwr.model$SDF)
head(results)
head(mydata.gwr)
  
# mpb10km US lines
mpb10km_poly <- readShapePoly("/Users/dongmeichen/Documents/beetle/shp/mpb10km.shp")
mpb10km_lines <- fortify(mpb10km_poly, region="SP_ID")

gwr.point1 <- ggplot(mydata.gwr, aes(x=x,y=y))+geom_point(aes(colour=results$PctOld), size=0.05)+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", 
                         na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1+geom_path(data=mpb10km_lines,aes(long, lat, group=id), colour="grey")+coord_equal()

# using multiple variables
GWRbandwidth <- gwr.sel(beetleAcres^0.07 ~ mStdAge+density+PctLarge+PctOld+vcc+mfri+prs+pms+pls+
                          vpd+summerP0+wd+AugTmean+Tvar+JanTmin+ddAugJul, 
                        data=mydata.raw, coords=cbind(data$x,data$y),adapt=T)
gwr.model = gwr(beetleAcres^0.07 ~ mStdAge+density+PctLarge+PctOld+vcc+mfri+prs+pms+pls+
                              vpd+summerP0+wd+AugTmean+Tvar+JanTmin+ddAugJul, 
                data=as.data.frame(mydata.raw), coords=cbind(data$x,data$y), adapt=GWRbandwidth, 
                hatmatrix=TRUE, se.fit=TRUE)

# a different method for GWR - from An introduction to R for spatial analysis and mapping
mydata <- data.frame(mydata.raw, data[,c("lon", "lat", "etopo1", "x", "y")])
spdf <- SpatialPointsDataFrame(mydata[,c("x", "y")], mydata)
localstats1 <- gwss(spdf, vars = c("beetleAcres", "mStdAge"), bw=50000)
head(data.frame(localstats1$SDF))

quick.map <- function(spdf,var,legend.title,main.title) {
  x <- spdf@data[,var]
  cut.vals <- pretty(x)
  x.cut <- cut(x,cut.vals)
  cut.levels <- levels(x.cut)
  cut.band <- match(x.cut,cut.levels)
  colors <- brewer.pal(length(cut.levels),'Reds')
  par(mar=c(1,1,1,1))
  plot(mpb10km_us_line,col='grey85')
  title(main.title)
  plot(spdf,add=TRUE,col=colors[cut.band],pch=16)
  legend('topleft',cut.levels,col=colors,pch=16,bty='n',title=legend.title)
}

quick.map(localstats1$SDF,"beetleAcres_LM",
          "Beetle affected acres","Geographically Weighted Mean")

par(mfrow=c(1,1))
quick.map(localstats1$SDF,"Corr_beetleAcres.mStdAge",
          expression(rho),"Geographically Weighted Pearson Correlation")

localstats2 <- gwss(spdf, vars = c("beetleAcres", "SprsCPA"), bw=50000, quantile = TRUE)

quick.map(localstats2$SDF,"beetleAcres_Median",
          "Beetle affected acres","Geographically Weighted Median Beetle-affected Acres")

par(mfrow=c(1,2))
quick.map(localstats2$SDF,"beetleAcres_IQR",
          "Beetle affected acres","Geographically Weighted Interquartile Range")
quick.map(localstats2$SDF,"beetleAcres_QI",
          "Beetle affected acres","Geographically Weighted Quantile Imbalance")

gwr.res <- gwr.basic(beetleAcres~SprsCPA, data=spdf, bw=50000, kernel = 'gaussian')
gwr.res

# try GLM
mod <- glm(beetleAcres ~ ., family = gaussian(), data=mydata)
summary(mod)
sort(abs(mod$coefficients))
