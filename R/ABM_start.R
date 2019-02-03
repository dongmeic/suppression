library(dplyr) # for left_join, transmute
library(tidyr) # for replace_na
library(raster) 
library(RColorBrewer)
library(BAMMtools)
# input data
setwd('/Users/dongmeichen/Documents/data/ABM')
indata <- read.csv('mpb10km_input_data.csv')
costs.df <- read.csv('suppressed_costs.csv')
fires.df <- read.csv('suppressed_fires.csv')
crs <- "+proj=laea +lat_0=50 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
indata <- cbind(indata, costs.df, fires.df)
#write.csv(indata, 'mpb10km_data.csv', row.names = FALSE)
df <- indata[!is.na(indata$SprsCosts) & !is.na(indata$SprsFires) 
             & !is.na(indata$SprsDays) & !is.na(indata$beetleAcres) 
             & !(indata$vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)), ]

setup <- function(df=indata, xdim=3, ydim=3){
  #i = 0
  start <- df[sample(nrow(df), 1),c('x', 'y')]
  xstart <- start$x
  ystart <- start$y
  xy <- data.frame(x=rep(seq(xstart, xstart + (xdim-1)*10000, 10000), times=ydim),
                   y=rep(seq(ystart, ystart + (ydim-1)*10000, 10000), times=xdim))
  xy <- xy %>%
    left_join(df %>% transmute(x, y, check = 'yes')) %>%
    replace_na(list(check = 'no'))
  while(any(xy$check == 'no') & xstart <= max(df$x) & xstart >= min(df$x) &
        ystart <= max(df$y) & ystart >= min(df$y)){
    start <- df[sample(nrow(df), 1),c('x', 'y')]
    xstart <- start$x
    ystart <- start$y
    xy <- data.frame(x=rep(seq(xstart, xstart + (xdim-1)*10000, 10000), each=xdim),
                     y=rep(seq(ystart, ystart + (ydim-1)*10000, 10000), times=ydim))
    xy <- xy %>%
      left_join(df %>% transmute(x, y, check = 'yes')) %>%
      replace_na(list(check = 'no'))
    #i = i + 1
    #print(i)
  }
  ind <- subset(df, x <= max(xy$x) & x >= min(xy$x) & y <= max(xy$y) & y >= min(xy$y))
  ind <- ind[,1:5]
  n <- dim(ind)[1]
  no.trees=sample(100:200000,n)
  no.young.tree=unlist(lapply(no.trees, function(x) sample(1:x, 1)))
  no.mid_aged.tree=unlist(lapply(no.trees - no.young.tree, function(x) sample(1:x, 1)))
  no.old.tree=no.trees - no.young.tree - no.mid_aged.tree
  FakeDF <- data.frame(no.trees=no.trees, no.young.tree=no.young.tree, 
                       no.mid_aged.tree=no.mid_aged.tree,no.old.tree=no.old.tree)
  ind <- cbind(ind, FakeDF)
  return(ind)
}

ind <- setup()

pal <- colorRampPalette(c("white","black"))
map <- function(var='etopo1', k=9){
  r <- rasterFromXYZ(ind[, c("x", "y", var)])
  plot(r, axes=FALSE, box=FALSE,legend=FALSE, col = pal(k), main=var)
  text(r, col='red')
}

map(var = 'no.trees')

# yearly change in tree age-class
forest.aging <- function(mortality.rate=0.01, 
                         regenerate.rate=0.02, 
                         growth.rate.young=0.05, 
                         growth.rate.old=0.06,
                         disturbance=F, 
                         disturbance.intensity=NULL){
  
  no.old.tree = no.old.tree + no.mid_aged.tree * growth.rate.old
  no.mid_aged.tree = no.mid_aged.tree + no.young.tree * growth.rate.young
  no.young.tree = no.young.tree + no.trees * regenerate.rate
  no.trees = - no.trees * mortality.rate + no.old.tree + no.mid_aged.tree + no.young.tree
  
  if(disturbance){
      no.trees = no.trees - no.trees * disturbance.intensity
      no.young.tree = sample(1:no.trees, 1)
      no.mid_aged.tree = sample(1:(no.trees - no.young.tree), 1)
      no.old.tree = no.trees - no.young.tree - no.mid_aged.tree
  }
}

