
library(lubridate)
library(scales)

setwd("~/Documents/R/Github/redtide_fei")
setwd("C:/Users/brendan.turley/Downloads/data/data")
getwd()
setwd(paste0(getwd(),'/data'))

sedar <- read.csv('rt_fei_sedar.csv')
sedar$begin <- mdy(sedar$begin)
sedar$end <- mdy(sedar$end)
names(sedar)[1:2] <- c('name','species')
sedar$name <- paste('SEDAR',sedar$name)

amendments <- read.csv('rt_fei_amendments.csv')
amendments$date.initiated <- mdy(amendments$date.initiated)
amendments$date.effective <- mdy(amendments$date.effective)
names(amendments)[c(1,4:5)] <- c('name','begin','end')

events <- rbind(sedar[,1:4],amendments[c(1:2,4:5)])

landings <- read.csv('rg_landings.csv')
rg_landings_61 <- read.csv('rg_landings_sedar61.csv')
rg_quota <- read.csv('rg_quota.csv')
rg_quota$quota <- rg_quota$quota*1e6
data_agg <- aggregate(landings$live.lbs,by=list(landings$year,landings$species),sum,na.rm=T)
names(data_agg) <- c('year','species','landings')
conversion <- 1.18 # 1.2 from Framework Action to the Fishery Management Plan for the Reef Fish Resources of the Gulf of Mexico to Adjust Red Grouper Allowable Harvest; also from SEDAR61 1.18, 1.2
data_agg$landings <- data_agg$landings*(1/conversion)

timeline <- seq(as.Date('2000-01-01'),as.Date('2023-01-01'),by='day')


yplt <- which(yday(timeline)==1)[seq(1,24,5)]

par(mar=c(3,10,1,1))
plot(timeline, rep(1, length(timeline)),
     typ = 'l', yaxt = 'n', xlab = 'date', ylab = '',
     ylim = c(-10, 3), lwd = 3, lend = 2)
text(timeline[yplt], rep(0, length(yplt)), seq(2000,2023,5))
points(timeline[yplt], rep(1, length(yplt)), pch = '|', lwd = 3, cex = 1.25)

segments(sedar$begin[which(sedar$species=='gag')], 2, 
         sedar$end[which(sedar$species=='gag')], 2,
         lwd = 10, col = 4, lend=2)

segments(sedar$begin[which(sedar$species=='red grouper')], 3, 
         sedar$end[which(sedar$species=='red grouper')], 3,
         lwd = 10, col = 2, lend=2)

ind <- c(1,3:5,7:12)
cols <- as.numeric(factor(amendments$species[ind],levels=c('red grouper', 'reef fish', 'gag')))+1
cols[c(1,4)] <- 3
for(i in 1:10){
  segments(amendments$begin[ind[i]], -i, 
           amendments$end[ind[i]], -i,
           lwd = 10, col = cols[i], lend=2)
}

axis(2,c(3,2,-(1:10)),
     c('Red Grouper','Gag','Amendment 30B','RG Regulatory 2010','RG Regulatory 2011','Amendment 32',
       'Gag Framework 2016','RG Framework 2016','Amendment 44','RG Modification 2019','Amendment 53','RG Modification 2022'),
     las=2)

polygon(c(as.Date('2005-01-01'),as.Date('2005-12-31'),as.Date('2005-12-31'),as.Date('2005-01-01')),
        c(10,10,-20,-20),border=alpha('gray50',.2),col=alpha('gray50',.5))

polygon(c(as.Date('2014-07-01'),as.Date('2014-09-01'),as.Date('2014-09-01'),as.Date('2014-07-01')),
        c(10,10,-20,-20),border=alpha('gray50',.2),col=alpha('gray50',.5))

polygon(c(as.Date('2018-01-01'),as.Date('2018-12-31'),as.Date('2018-12-31'),as.Date('2018-01-01')),
        c(10,10,-20,-20),border=alpha('gray50',.2),col=alpha('gray50',.5))

polygon(c(as.Date('2021-05-01'),as.Date('2021-10-31'),as.Date('2021-10-31'),as.Date('2021-05-01')),
        c(10,10,-20,-20),border=alpha('gray50',.2),col=alpha('gray50',.5))




b <- barplot(data_agg$landings[which(data_agg$species=='GROUPER RED')],
        ylim=c(0,max(rg_quota$quota)+1e6), las=2, 
        names.arg = data_agg$year[which(data_agg$species=='GROUPER RED')])
points(b[(23-19+1):23],rg_quota$quota,typ='o',pch=16)


plot(rg_quota$year,rg_quota$quota,typ='h',col=4,lwd=7,lend=2,
     ylim=c(0,max(rg_quota$quota)))
points(data_agg$year[which(data_agg$species=='GROUPER RED')],
       data_agg$landings[which(data_agg$species=='GROUPER RED')],
       typ='o',lwd=2)
points(rg_landings_61$year,rg_landings_61$total,typ='o')



##### red grouper

rg <- events[which(events$species!='gag'),]
rg <- rg[order(rg$begin),]
cols <- rep(NA,13)
cols[c(4,5,8,11,12)] <- 1
cols[c(2,6,9,13)] <- 4
cols[c(1,3,7,10)] <- 2

plot(timeline, rep(1, length(timeline)),
     typ = 'n', yaxt = 'n', xlab = 'date', ylab = '',
     ylim = c(0,13), lwd = 3, lend = 2)
for(i in 1:13){
    segments(rg$begin[i], i, 
             rg$end[i], i,
             lwd = 10, col = cols[i], lend=2)
}
axis(2,1:13,
     c('SEDAR12','Amendment 30B','SEDAR 12 update','RG Regulatory 2010','RG Regulatory 2011','Amendment 32',
       'SEDAR42','RG Framework 2016','Amendment 44','SEDAR61','RG Modification 2019','RG Modification 2022','Amendment 53'),
     las=2)




plot(rep(0,15),seq(as.Date('2009-01-01'),as.Date('2023-01-01'),by='year'),xlim=c(0,10),las=1,xaxt='n',xlab='',ylab='',typ='n')
segments(rep(0,3),amendments$end[which(amendments$species=='gag')],
         rep(2.5,3),amendments$end[which(amendments$species=='gag')])
segments(rep(0,3),amendments$end[which(amendments$species=='red grouper')],
         rep(7.5,3),amendments$end[which(amendments$species=='red grouper')])
