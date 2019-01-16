# explore the different conditions for fire suppression
# correlate beetle affected acres with density and age, and fire severity in different GAP status

library(ggplot2)
library(ggpubr)
#inpath <- 'H:/14.winter_2019/data/'
inpath <- '/Volumes/dongmeic/14.winter_2019/data/'
indata <- read.csv(paste0(inpath, 'mpb10km_nonclimate.csv'))
head(indata)
fire_sprs <- read.csv(paste0(inpath, 'suppressed_fires.csv'))
sprs_costs <- read.csv(paste0(inpath, 'suppressed_costs.csv'))
df <- cbind(indata[,c('allyears', 'forest', 'age', 'density', 'PctLarge', 'PctOld', 
                      'GAP', 'GAP2', 'GAP3', 'GAPs', 'vcc', 'mfri', 'prs',
                      'pms', 'pls', 'wilderness')], fire_sprs, sprs_costs)
colnames(df)[which(colnames(df)=='GAP')] <- 'GAP1'
df[,c('GAP1', 'GAP2', 'GAP3')] <- apply(df[,c('GAP1', 'GAP2', 'GAP3')], 
                                        2, function(x) ifelse(x != 0, 1,0))
vars <- c('forest','GAP1', 'GAP2', 'GAP3', 'GAPs', 'vcc', 'mfri', 'prs', 'pms', 'pls','wilderness')
df[,vars] <- apply(df[,vars], 2, function(x) as.factor(x))
df$GAPs <- ifelse(df$GAP1=='1' & df$GAP2=='1', '2',
                    ifelse(df$GAP3=='1' & df$GAP2=='1', '2',
                           ifelse(df$GAP1=='1' & df$GAP3=='1', '3',
                                  ifelse(df$GAP1=='1' & df$GAP2=='1'& df$GAP3=='1', '2', df$GAPs))))
head(df)

p <- ggplot(df[!is.na(df$allyears),], aes(x=age, y=log(allyears)))
p + geom_point()
p + geom_point(aes(colour = GAP1), alpha = 1/5)+
  geom_smooth(aes(color = GAP1), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB","#FC4E07"))+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~GAP1)

p + geom_point(aes(colour = GAP2), alpha = 1/5)+
  geom_smooth(aes(color = GAP2), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB","#FC4E07"))+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~GAP2)

p + geom_point(aes(colour = GAP3), alpha = 1/5)+
  geom_smooth(aes(color = GAP3), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB","#FC4E07"))+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~GAP3)

p + geom_point(aes(colour = GAPs), alpha = 1/5)+
  geom_smooth(aes(color = GAPs), method = lm, 
              se = FALSE, fullrange = TRUE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~GAPs)

df.s <- df[!is.na(df$allyears) & !(df$vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181))
            & df$GAPs %in% c(1, 2, 3),]
#df.s <- df.s[df.s$GAPs %in% c('1', '2', '3'),]
p <- ggplot(df.s, aes(x=vcc, y=log(allyears)))
p + geom_boxplot(aes(colour = GAPs))
p + geom_boxplot(aes(colour = GAPs))+
  facet_wrap(~GAPs)

df.s <- df[!is.na(df$allyears) & !(df$mfri %in% c(111, 112, 131, 132, 133))
           & df$GAPs %in% c(1, 2, 3),]
df.s$mfri <- ifelse(df.s$mfri %in% c('1', '2', '3', '4', '5', '6'),'1',
                    ifelse(df.s$mfri %in% c('7', '8', '9', '10', '11', '12'), '2', 
                           ifelse(df.s$mfri %in% c('13', '14', '15', '16', '17', '18'), '3', '4')))
p <- ggplot(df.s, aes(x=GAPs, y=log(allyears)))
p + geom_boxplot(aes(colour = mfri))

df.s <- df[!is.na(df$allyears) & !(df$prs %in% c(111, 112, 131, 132))
           & df$GAPs %in% c(1, 2, 3),]
df.s$prs <- ifelse(df.s$prs %in% c('1', '2', '3', '4', '5'),'1',
                    ifelse(df.s$prs %in% c('6','7', '8', '9', '10'), '2', 
                           ifelse(df.s$prs %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=prs, y=log(allyears)))
p + geom_boxplot(aes(colour = GAPs))

df.s <- df[!is.na(df$allyears) & !(df$pms %in% c(111, 112, 131, 132))
           & df$GAPs %in% c(1, 2, 3),]
df.s$pms <- ifelse(df.s$pms %in% c('1', '2', '3', '4', '5'),'1',
                   ifelse(df.s$pms %in% c('6','7', '8', '9', '10'), '2', 
                          ifelse(df.s$pms %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=GAPs, y=log(allyears)))
p + geom_boxplot(aes(colour=pms))

df.s <- df[!is.na(df$allyears) & !(df$pls %in% c(111, 112, 131, 132))
           & df$GAPs %in% c(1, 2, 3),]
df.s$pls <- ifelse(df.s$pls %in% c('1', '2', '3', '4', '5'),'1',
                   ifelse(df.s$pls %in% c('6','7', '8', '9', '10'), '2', 
                          ifelse(df.s$pls %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=GAPs, y=log(allyears)))
p + geom_boxplot(aes(colour=pls))

# correlate beetle affected acres with suppressed fires in different fire severtiy
df.s <- df[!is.na(df$allyears) & !(df$mfri %in% c(111, 112, 131, 132, 133)),]
df.s$mfri <- ifelse(df.s$mfri %in% c('1', '2', '3', '4', '5', '6'),'1',
                    ifelse(df.s$mfri %in% c('7', '8', '9', '10', '11', '12'), '2', 
                           ifelse(df.s$mfri %in% c('13', '14', '15', '16', '17', '18'), '3', '4')))
p <- ggplot(df.s, aes(x=SprsFires, y=log(allyears)))
p + geom_point(aes(colour = mfri), alpha = 1/5)+
  geom_smooth(aes(color = mfri), method = lm, 
              se = FALSE, fullrange = TRUE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~mfri)

df.s <- df[!is.na(df$allyears) & !(df$vcc %in% c(111, 112, 120, 121, 131, 132, 180, 181)),]
p <- ggplot(df.s, aes(x=SprsFires, y=log(allyears)))
p + geom_point(aes(colour = vcc), alpha = 1/5)+
  geom_smooth(aes(color = vcc), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~vcc)

df.s <- df[!is.na(df$allyears) & !(df$prs %in% c(111, 112, 131, 132)),]
df.s$prs <- ifelse(df.s$prs %in% c('1', '2', '3', '4', '5'),'1',
                   ifelse(df.s$prs %in% c('6','7', '8', '9', '10'), '2', 
                          ifelse(df.s$prs %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=SprsFires, y=log(allyears)))
p + geom_point(aes(colour = prs), alpha = 1/5)+
  geom_smooth(aes(color = prs), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~prs)

df.s <- df[!is.na(df$allyears) & !(df$pms %in% c(111, 112, 131, 132)),]
df.s$pms <- ifelse(df.s$pms %in% c('1', '2', '3', '4', '5'),'1',
                   ifelse(df.s$pms %in% c('6','7', '8', '9', '10'), '2', 
                          ifelse(df.s$pms %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=SprsFires, y=log(allyears)))
p + geom_point(aes(colour = pms), alpha = 1/5)+
  geom_smooth(aes(color = pms), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~pms)

df.s <- df[!is.na(df$allyears) & !(df$pls %in% c(111, 112, 131, 132)),]
df.s$pls <- ifelse(df.s$pls %in% c('1', '2', '3', '4', '5'),'1',
                   ifelse(df.s$pls %in% c('6','7', '8', '9', '10'), '2', 
                          ifelse(df.s$pls %in% c('11', '12', '13', '14', '15'), '3', '4')))
p <- ggplot(df.s, aes(x=PctSprs, y=log(allyears)))
p + geom_point(aes(colour = pls), alpha = 1/5)+
  geom_smooth(aes(color = pls), method = lm, 
              se = FALSE, fullrange = FALSE)+
  ggpubr::stat_cor(label.x = 0)+
  facet_wrap(~pls)

plot(df$SprsFires, log(df$allyears))
plot(df$PctSprs, log(df$allyears))
plot(df$SprsDays, log(df$allyears))

# percent of beetle affected grids in GAP1, 2, 3
n <- length(df$allyears[!is.na(df$allyears)])
n1 <- length(df$allyears[!is.na(df$allyears) & df$GAPs == '1'])
n2 <- length(df$allyears[!is.na(df$allyears) & df$GAPs == '2'])
n3 <- length(df$allyears[!is.na(df$allyears) & df$GAPs == '3'])
n1/n
n2/n
n3/n
1-(n1+n2+n3)/n

# beetle affected acres and suppressed fires without GAP1 
df.s <- df[!is.na(df$allyears) & df$GAPs == '2',]
plot(df.s$SprsFires, log(df.s$allyears))
plot(df.s$PctSprs, log(df.s$allyears))
plot(df.s$SprsDays, log(df.s$allyears))

# wilderness
df.s <- df[!is.na(df$allyears) & df$wilderness == '0',]
cor.test(df.s$SprsFires, log(df.s$allyears))
cor.test(df.s$PctSprs, log(df.s$allyears))
cor.test(df.s$SprsDays, log(df.s$allyears))

# forest
df.s <- df[!is.na(df$allyears) & df$forest== '1',]
cor.test(df.s$SprsFires, log(df.s$allyears))
cor.test(df.s$PctSprs, log(df.s$allyears))
cor.test(df.s$SprsDays, log(df.s$allyears))

df.s <- df[!is.na(df$allyears) & df$forest== '1' & !is.na(df$SprsCPA) &
             df$SprsCPA != Inf,]
cor.test(df.s$SprsCosts, log(df.s$allyears))
cor.test(df.s$SprsAcres, log(df.s$allyears))
cor.test(df.s$SprsCPA, log(df.s$allyears))

df.s <- df[!is.na(df$allyears) & !is.na(df$SprsCPA) &
             df$SprsCPA != Inf,]
cor.test(df.s$SprsCosts, log(df.s$allyears))
cor.test(df.s$SprsAcres, log(df.s$allyears))
cor.test(df.s$SprsCPA, log(df.s$allyears))

