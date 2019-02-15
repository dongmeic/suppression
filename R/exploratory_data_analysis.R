library(rcompanion)
library(MASS)
library(mgcv)
library(fmsb)
library(classInt)
library(RColorBrewer)
library(rgdal)

# csvpath <- "/Users/dongmeichen/Documents/data/ABM/"
csvpath <- "/Users/dongmeichen/Documents/beetle/data/"

df <- read.csv(paste0(csvpath, "mpb10km_data.csv"))
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

# cluster analysis
drops <- c("lon", "lat", "etopo1", "x", "y", "host", "forest", "GAP1", "GAP2", "GAP3") #, "beetleAcres"
mydata <- df[,!(names(df) %in% drops)]
mydata <- subset(mydata, !is.na(beetleAcres) & SprsCPA != Inf & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
         !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
         !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
mydata <- na.omit(mydata)
mydata <- scale(mydata)
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 

# get coordinate information
data <- subset(df, !is.na(beetleAcres) & SprsCPA != Inf & !(vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)) &
                   !(mfri %in% c(111, 112, 131, 132, 133)) & !(prs %in% c(111, 112, 131, 132)) & 
                   !(pms %in% c(111, 112, 131, 132)) & !(pls %in% c(111, 112, 131, 132)))
data <- na.omit(data)
data <- cbind(mydata, data[,c("lon", "lat", "etopo1", "x", "y")])
head(data)
plotvar <- data$fit.cluster
nclr <- 5
plotclr <- brewer.pal(nclr,"Set1")
#plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

mpb10km_us_line <- readOGR(dsn='/Users/dongmeichen/Documents/beetle/shp', 
                           layer='mpb10km_us_line')
mpb_projstr <- "+proj=laea +lon_0=-112.5 +lat_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj4string(mpb10km_us_line) <- mpb_projstr

par(mfrow=c(1,1),mar=c(0,0,0,0))
plot(mpb10km_us_line)
points(data$x, data$y, pch=16, col=colcode, cex=0.8)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
par(mfrow=c(1,1),mar=c(1,0,2,0))
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results
summary(fit) # display the best model 

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
par(mfrow=c(1,1),mar=c(5,4.5,2,1))
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster) 

