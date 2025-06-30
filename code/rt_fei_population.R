library(cmocean)
library(grid)
library(magick)

setwd("/Users/Brendan/Desktop/professional/projects/Postdoc_FL/images")
gag <- image_read("640x427-Grouper-Gag-NOAAFisheries.png")
rgr <- image_read("640x427-Grouper-Red-NOAAFisheries.png")

setwd("/Users/Brendan/Desktop/professional/projects/Postdoc_FL/data")
grouper <- read.csv('Grouper_ssb_rec_trajectories.csv')


col <- c('gray50','purple3','gold3')
cols <- c(1,'forestgreen','dodgerblue','orange2')



setwd("/Users/Brendan/Desktop/professional/projects/Postdoc_FL/figures")
png('fei_populations.png',width=10,height=7,units='in',res=300)
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(grouper$Year, grouper$GAG_SSB_SEDAR72,
     typ='n', las=1,
     xlim=c(1990,2020),
     ylim=range(grouper[,2:5],na.rm=T),
     xlab='',ylab='Gag SSB')
abline(v=c(2005,2014,2018),col='gray80',lwd=10)
for(i in 2:5){
  points(grouper$Year, grouper[,i], typ='o', lwd=2, col=cols[i-1], pch=16, cex=.8)
}
legend('topleft',c('SEDAR72','SEADR33U','SEDAR33','SEDAR10U'),pch=16,col=cols,bty='n')
mtext('a)',adj=-.1,line=.5)


plot(grouper$Year, grouper$GAG_R_SEDAR72,
     typ='n', las=1,
     xlim=c(1990,2020),
     ylim=range(grouper[,6:9],na.rm=T),
     xlab='',ylab='Gag Recruitment')
abline(v=c(2005,2014,2018),col='gray80',lwd=10)
for(i in 6:9){
  points(grouper$Year, grouper[,i], typ='o', lwd=2, col=cols[i-5], pch=16, cex=.8)
}
mtext('b)',adj=-.1,line=.5)
# rasterImage(gag, 2009, 2, 2021, 3)
grid.raster(gag,width=.15,x=unit(.89, "npc"),y=unit(.88, "npc"))

plot(grouper$Year, grouper$RGR_SSB_SEDAR61,
     typ='n', las=1,
     xlim=c(1990,2020),
     ylim=range(grouper[,10:12],na.rm=T),
     xlab='',ylab='Red grouper SSB')
abline(v=c(2005,2014,2018),col='gray80',lwd=10)
for(i in 10:12){
  points(grouper$Year, grouper[,i], typ='o', lwd=2, col=col[i-9], pch=16, cex=.8)
}
legend('topleft',c('SEDAR61','SEADR42','SEDAR12U'),pch=16,col=col,bty='n')
mtext('c)',adj=-.1,line=.5)

plot(grouper$Year, grouper$RGR_R_SEDAR61,
     typ='n', las=1,
     xlim=c(1990,2020),
     ylim=range(grouper[,13:15],na.rm=T),
     xlab='',ylab='Red grouper Recruitment')
abline(v=c(2005,2014,2018),col='gray80',lwd=10)
for(i in 13:15){
  points(grouper$Year, grouper[,i], typ='o', lwd=2, col=col[i-12], pch=16, cex=.8)
}
mtext('d)',adj=-.1,line=.5)
grid.raster(rgr,width=.15,x=unit(.89, "npc"),y=unit(.38, "npc"))
dev.off()







library(stocksmart)

sort(unique(stockAssessmentData$StockName))


### red grouper
rg_sum <- stockAssessmentSummary[which(stockAssessmentSummary$`Stock Name`=='Red grouper - Gulf of Mexico'),]
rg <- stockAssessmentData[which(stockAssessmentData$StockName=='Red grouper - Gulf of Mexico'),]

rg1 <- rg[which(rg$Assessmentid==10982),]

rg1_abun <- rg1[which(rg1$Metric=='Abundance'),]
plot(rg1_abun$Year,rg1_abun$Value,typ='l',ylab='Thousand eggs',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

rg1_catch <- rg1[which(rg1$Metric=='Catch'),]
plot(rg1_catch$Year,rg1_catch$Value,typ='l',ylab='Pounds - gutted weight',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

rg_F <- rg1[which(rg1$Metric=='Fmort'),]
plot(rg_F$Year,rg_F$Value,typ='l',ylab='Exploitation rate',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

rg_r <- rg1[which(rg1$Metric=='Recruitment'),]
plot(rg_r$Year,rg_r$Value,typ='l',ylab='Thousand recruits',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)


rg2 <- rg[which(rg$Assessmentid==9342),]
rg2_abun <- rg2[which(rg2$Metric=='Abundance'),]

rg3 <- rg[which(rg$Assessmentid==6232),]
rg3_abun <- rg3[which(rg3$Metric=='Abundance'),]

plot(rg1_abun$Year,rg1_abun$Value,typ='l')
points(rg2_abun$Year,rg2_abun$Value,typ='l')
points(rg3_abun$Year,rg3_abun$Value,typ='l')

rg1_abun$Units
rg2_abun$Units
rg3_abun$Units


### gag
gag_sum <- stockAssessmentSummary[which(stockAssessmentSummary$`Stock Name`=='Gag - Gulf of Mexico'),]
gag <- stockAssessmentData[which(stockAssessmentData$StockName=='Gag - Gulf of Mexico'),]

gag1 <- gag[which(gag$Assessmentid==11676),]

gag_abun <- gag1[which(gag1$Metric=='Abundance'),]
gag_abun <- gag_abun[which(gag_abun$Year>1985),]
plot(gag_abun$Year,gag_abun$Value,typ='l',ylab='Metric tons',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

gag_catch <- gag1[which(ga1g$Metric=='Catch'),]
plot(gag_catch$Year,gag_catch$Value,typ='l',ylab='Metric tons',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

gag_F <- gag1[which(gag1$Metric=='Fmort'),]
plot(gag_F$Year,gag_F$Value,typ='l',ylab='Exploitation rate',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

gag_r <- gag1[which(gag1$Metric=='Recruitment'),]
plot(gag_r$Year,gag_r$Value,typ='l',ylab='Thousand recruits',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)


gag2 <- gag[which(gag$Assessmentid==4996),]
gag2_abun <- gag2[which(gag2$Metric=='Abundance'),]

gag3 <- gag[which(gag$Assessmentid==3009),]
gag3_abun <- gag3[which(gag3$Metric=='Abundance'),]


plot(gag_abun$Year,gag_abun$Value,typ='l')
points(gag2_abun$Year,gag2_abun$Value,typ='l')
points(gag3_abun$Year,gag3_abun$Value,typ='l')


png('fei_populations.png',width=11,height=5.5,units='in',res=300)
par(mfrow=c(2,3))
plot(rg1_abun$Year,rg1_abun$Value,typ='l',ylab='Thousand eggs',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)
mtext('Abundance')

plot(rg1_catch$Year,rg1_catch$Value,typ='l',ylab='Pounds - gutted weight',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)
mtext('Catch')

plot(rg_r$Year,rg_r$Value,typ='l',ylab='Thousand recruits',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)
mtext('Recruits')
mtext('Red grouper',4,line=.5)

plot(gag_abun$Year,gag_abun$Value,typ='l',ylab='Metric tons',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

plot(gag_catch$Year,gag_catch$Value,typ='l',ylab='Metric tons',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)

plot(gag_r$Year,gag_r$Value,typ='l',ylab='Thousand recruits',xlim=c(1990,2020),lwd=3)
abline(v=c(2005,2014,2018),lty=5,col=2,lwd=3,lend=2)
mtext('Gag',4,line=.5)
dev.off()
