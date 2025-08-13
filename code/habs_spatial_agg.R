
library(fields)
library(lubridate)
library(MASS)
library(ncdf4)
library(rgdal)
library(scales)

### load map
# setwd("C:/Users/brendan.turley/Desktop/FL_habs/ne_10m_admin_0_countries")
setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

setwd("~/Desktop/professional/biblioteca/data")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

setwd("C:/Users/brendan.turley/Documents/data/habs")
# original data requested from https://habsos.noaa.gov/about
# habs1 <- read.csv('habsos_20220225.csv')
# habs1 <- read.csv('habsos_20230714.csv')
habs1 <- read.csv('habsos_20240430_fix.csv')
# habs1$date <- dmy_hm(paste(substr(habs1$SAMPLE_DATE,1,15),substr(habs1$SAMPLE_DATE,30,31)))
habs1$date <- mdy(habs1$SAMPLE_DATE)
habs <- habs1[which(year(habs1$date)<2024 & year(habs1$date)>1999 & habs1$STATE_ID=='FL' ),]

### duplication
# names(habs)[c(3:4,6,26)]
# dups1 <- which(duplicated(habs[c(3:4,6,26)]))
# dups2 <- which(duplicated(habs[c(3:4,6,26)],fromLast = T))

# new <- matrix(NA,length(dups1),1)
# for(i in 1:length(dups1)){
#   # new[i] <- mean(data$Karenia.brevis.abundance..cells.L.[c(dups1[i],dups2[i])],na.rm=T)
#   new[i,] <- mean(habs[c(dups1[i],dups2[i]),10],na.rm=T)
# }
# habs[dups1,10] <- new
# habs <- habs[-dups2,]
### end, no dups

lonbox_e <- -80.6 ### Florida Bay
lonbox_w <- -88 ### mouth of Mississippi River
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.3 ### southern edge of Ket West
ind <- which(habs$LATITUDE<latbox_n & habs$LATITUDE>latbox_s & habs$LONGITUDE<lonbox_e & habs$LONGITUDE>lonbox_w)
habs <- habs[ind,]
ind2 <- which(habs$LONGITUDE>-82 & habs$LATITUDE>27)
habs <- habs[-ind2,]
# plot(habs$LONGITUDE,habs$LATITUDE,col=year(habs$date),asp=1)

# lons <- seq(lonbox_w,lonbox_e,by=.04)
# lats <- seq(latbox_s,latbox_n,by=.04)

lons <- seq(lonbox_w,lonbox_e,by=.05)
lats <- seq(latbox_s,latbox_n,by=.05)

# lons <- seq(lonbox_w,lonbox_e,by=.1)
# lats <- seq(latbox_s,latbox_n,by=.1)
# lons <- seq(lonbox_w-.1,lonbox_e+.1,by=.0417)
# lats <- seq(latbox_s-.1,latbox_n+.1,by=.0417)

lats_c <- cut(habs$LATITUDE,lats)
lons_c <- cut(habs$LONGITUDE,lons)
lonlat <- expand.grid(lon=levels(lons_c),lat=levels(lats_c))
h_agg <- aggregate(habs$CELLCOUNT,by=list(lon=lons_c,lat=lats_c),mean,na.rm=T)
hab_agg <- merge(lonlat,h_agg,by=c('lon','lat'),all=T)
hab_agg_m <- t(matrix(hab_agg$x,length(levels(lats_c)),length(levels(lons_c))))
# imagePlot(log10(hab_agg_m),asp=1)
ind_na <- which(hab_agg_m==0)
hab_agg_m[which(hab_agg_m==0)] <- NA
habs_mp <- log10(hab_agg_m)
zeros <- matrix(NA,dim(habs_mp)[1],dim(habs_mp)[2])
zeros[ind_na] <- 0

fxn <- function(x) {
  # mean(x[which(x>quantile(x,.75,na.rm=T))],na.rm=T)
  # length(which(x>quantile(x,.5,na.rm=T)))
  # c(length(which(x==0))/length(x),
  #   length(which(x>=1e1 & x<1e2))/length(x),
  #   length(which(x>=1e2 & x<1e3))/length(x),
  #   length(which(x>=1e3 & x<1e4))/length(x),
  #   length(which(x>=1e4 & x<1e5))/length(x),
  #   length(which(x>=1e5))/length(x))
  length(which(x>=1e5))/length(x)
  # length(which(x>1e5))/length(which(x>0))
  # length(which(x>1e5))
}


x <- aggregate(habs$CELLCOUNT,by=list(month(habs$date)),fxn)
# barplot(t(x$x[,ncol(x$x):1]),names.arg = month.abb[1:12],las=2)


setwd('~/Desktop/professional/projects/Postdoc_FL/figures')
# png('rt_fei_habs.png',width=10,height=4,units='in',res=300)
# par(mar=c(4.5,4.5,2,2),mfrow=c(1,2))
png('rt_fei_habs2.png',width=5,height=7,units='in',res=300)
par(mar=c(4.5,4.5,1,2),mfrow=c(2,1))

barplot(x$x,names.arg = month.abb[1:12],las=2,yaxt='n')
axis(2,seq(0,.14,.02),paste0(seq(0,14,2),'%'),las=2)
mtext('Percentage of water samples',2,line=3)
mtext('a)',adj=0,font=2)

imagePlot(lons[1:(length(lons-1))],
          lats[1:(length(lats)-1)],
          habs_mp,
          asp=1,col=rocket(60),nlevel=59,las=1,
          xlab='Longitude',ylab='Latitude',
          legend.lab = expression(paste('Karenia brevis (log'[10],' cells l'^-1,')')),
          legend.mar=5) #legend.mar=10 for horizontal
image(lons[1:(length(lons-1))],
      lats[1:(length(lats)-1)],
      zeros,add=T,breaks=c(-1,1),col='gray20')
plot(world,add=T,col='gray70')
contour(topo_lon,topo_lat,topo,add=T,
        levels=c(-100),col='gray50',lwd=.75)
mtext('b)',adj=0,font=2)
dev.off()



habs_h <- aggregate(habs$CELLCOUNT,by=list(lon=lons_c,lat=lats_c),length)
hab_h <- merge(lonlat,habs_h,by=c('lon','lat'),all=T)
hab_h_m <- t(matrix(hab_h$x,length(levels(lats_c)),length(levels(lons_c))))
hab_h_m[which(hab_h_m==0)] <- NA
habs_hp <- log10(hab_h_m)

png('rt_fei_map2.png',width=7,height=6,units='in',res=300)
par(mar=c(4.5,4.5,1,1))
imagePlot(lons[1:(length(lons-1))],
          lats[1:(length(lats)-1)],
          habs_hp,
          asp=1,col=mako(60),nlevel=59,
          xlab='Longitude',ylab='Latitude',
          legend.lab = expression(paste('Total number of samples (log'[10],')')))
plot(world,add=T,col='gray70')
contour(topo_lon,topo_lat,topo,add=T,
        levels=c(-200),col='gray50',lwd=.75)
dev.off()



pdf('habs_agg_meanmth.pdf',width=6,height=5,pointsize=10,useDingbats=T)
par(mfrow=c(2,2),mar=c(4,4,1,1))
for(i in 1:12){
  hab_t <- habs[which(month(habs$date)==i),]
  lat_c <- cut(hab_t$LATITUDE,lats)
  lon_c <- cut(hab_t$LONGITUDE,lons)
  h_agg <- aggregate(hab_t$CELLCOUNT,by=list(lon=lon_c,lat=lat_c),mean,na.rm=T)
  hab_agg <- merge(lonlat,h_agg,by=c('lon','lat'),all=T)
  hab_agg_m <- t(matrix(hab_agg$x,length(levels(lats_c)),length(levels(lons_c))))
  # imagePlot(log10(hab_agg_m),asp=1)
  ind_na <- which(hab_agg_m==0)
  hab_agg_m[which(hab_agg_m==0)] <- NA
  habs_mp <- log10(hab_agg_m)
  zeros <- matrix(NA,dim(habs_mp)[1],dim(habs_mp)[2])
  zeros[ind_na] <- 0
  imagePlot(lons[1:(length(lons-1))],
            lats[1:(length(lats)-1)],
            habs_mp,
            xlab='',ylab='',las=1,
            asp=1,col=plasma(60),nlevel=59)
  plot(world,add=T,col='gray70')
  imagePlot(lons[1:(length(lons-1))],
            lats[1:(length(lats)-1)],
            habs_mp,
            asp=1,col=plasma(60),nlevel=59,add=T)
  image(lons[1:(length(lons-1))],
        lats[1:(length(lats)-1)],
        zeros,add=T,breaks=c(-1,1),col='gray20')
  mtext(month.abb[i])
}
dev.off()

pdf('habs_agg_histmth.pdf',width=6,height=5,pointsize=10,useDingbats=T)
par(mfrow=c(2,2),mar=c(4,4,1,1))
for(i in 1:12){
  hab_t <- habs[which(month(habs$date)==i),]
  lat_c <- cut(hab_t$LATITUDE,lats)
  lon_c <- cut(hab_t$LONGITUDE,lons)
  habs_h <- aggregate(hab_t$CELLCOUNT,by=list(lon=lon_c,lat=lat_c),length)
  hab_h <- merge(lonlat,habs_h,by=c('lon','lat'),all=T)
  hab_h_m <- t(matrix(hab_h$x,length(levels(lats_c)),length(levels(lons_c))))
  hab_h_m[which(hab_h_m==0)] <- NA
  habs_hp <- log10(hab_h_m)
  imagePlot(lons[1:(length(lons-1))],
            lats[1:(length(lats)-1)],
            habs_hp,
            xlab='',ylab='',las=1,
            asp=1,col=mako(60),nlevel=59)
  plot(world,add=T,col='gray70')
  imagePlot(lons[1:(length(lons-1))],
            lats[1:(length(lats)-1)],
            habs_hp,
            asp=1,col=mako(60),nlevel=59,add=T)
  mtext(month.abb[i])
}
dev.off()


hab_kde <- kde2d(habs$LONGITUDE,habs$LATITUDE,n=100)
z <- hab_kde$z
z[z<quantile(z,.95,na.rm=T)] <- NA

png('rt_fei_map3.png',width=7,height=6,units='in',res=300)
par(mar=c(4.5,4.5,1,1))
imagePlot(hab_kde$x,hab_kde$y,log10(z),
          asp=1,col=mako(60),nlevel=59,
          xlab='Longitude',ylab='Latitude',
          legend.lab = expression(paste('Kernal density (log'[10],')')))
plot(world,add=T,col='gray70')
contour(topo_lon,topo_lat,topo,add=T,
        levels=c(-200),col='gray50',lwd=.75)
dev.off()

# lats_c <- cut(habs$LATITUDE,lats)
# lats_merge <- data.frame(lats=sort(unique(lats_c)))
# lons_c <- cut(habs$LONGITUDE,lons)
# 
# habs_h <- habs_m <- matrix(NA,length(lons)-1,length(lats)-1)
# n <- 1
# for(i in levels(lons_c)){
#   tmp <- habs[which(lons_c==i),]
#   habs_h[n,] <- hist(habs$LATITUDE[which(lons_c==i)],breaks = lats,plot=F)$counts
#   if(nrow(tmp)>0){
#     h_agg <- aggregate(habs$CELLCOUNT[which(lons_c==i)],by=list(lats_c[which(lons_c==i)]),mean,na.rm=T)
#     names(h_agg) <- c('lats','cells')
#     h_aggm <- merge(lats_merge,h_agg,by='lats',all=T)
#     habs_m[n,] <- h_aggm$cells
#   }
#   n <- n + 1
# }
# habs_h[which(habs_h==0)] <- NA
# habs_hp <- log10(habs_h)
# imagePlot(lons[1:(length(lons-1))],
#           lats[1:(length(lats)-1)],
#           habs_hp,
#           asp=1,col=mako(60),nlevel=59)
# 
# ind_na <- which(habs_m==0)
# habs_m[which(habs_m==0)] <- NA
# habs_mp <- log10(habs_m)
# zeros <- matrix(NA,dim(habs_mp)[1],dim(habs_mp)[2])
# zeros[ind_na] <- 0
# imagePlot(lons[1:(length(lons-1))],
#           lats[1:(length(lats)-1)],
#           habs_mp,
#           asp=1,col=plasma(60),nlevel=59)
# image(lons[1:(length(lons-1))],
#       lats[1:(length(lats)-1)],
#       zeros,add=T,breaks=c(-1,1),col='gray20')



habs_q <- matrix(NA,12,3)
for(i in 1:12){
  tmp <- habs[which(month(habs$date)==i),]
  habs_q[i,] <- quantile(tmp$CELLCOUNT,c(.5,.75,.9),na.rm=T)
}

barplot(habs_q[,1],names=month.abb[1:12])
barplot(habs_q[,2],names=month.abb[1:12])
barplot(habs_q[,3],names=month.abb[1:12])
barplot(t(habs_q))



x <- aggregate(habs$CELLCOUNT,by=list(month(habs$date)),mean,na.rm=T)

setwd('~/Desktop/professional/projects/Postdoc_FL/figures')
png('rt_fei_box.png',width=7,height=6,units='in',res=300)
par(mar=c(4,5,1,1))
boxplot(habs$CELLCOUNT[which(habs$CELLCOUNT>0)]~month(habs$date[which(habs$CELLCOUNT>0)]),
        log='y',outline=T,outcol=alpha('gray20',.1),pch=16,
        lty=1,staplewex=0,lwd=2,
        xaxt='n',yaxt='n',las=2,xlab='', ylab='',varwidth=T)
mtext(expression(paste('Karenia brevis (cells l'^-1,')')),2,line=3.5)
axis(1,1:12,month.abb[1:12],las=2)
axis(2,10^seq(0,8,1),c(0,10,100,1000,1e4,1e5,1e6,1e7,1e8),las=1)
abline(h=1e5,lty=5,lwd=2)
points(x$Group.1,x$x,pch=16,col='red',typ='o')
dev.off()



fxn <- function (x,y) {
  y[which.max(x)]
}

habs_h <- aggregate(c(habs$CELLCOUNT),
                    by=list(lon=lons_c,lat=lats_c),
                    which.max)

habs_h <- aggregate(habs$CELLCOUNT,
                    by=list(lon=lons_c,lat=lats_c,month=month(habs$date)),
                    max,na.rm=T)

habs_h$lonlats <- paste(habs_h$lon,habs_h$lat)

new <- as.data.frame(matrix(NA,nrow(habs_mp),4))
n <- 1
for(i in unique(habs_h$lonlats)){
  tmp <- habs_h[which(habs_h$lonlats==i),]
  ind <- which.max(tmp$x)
  new[n,] <- c(as.character(tmp[ind,1]),as.character(tmp[ind,2]),tmp[ind,3],max(tmp$x,na.rm=T))
  n <- n + 1
}
names(new) <- c('lon','lat','mth','max')
new$mth <- as.numeric(new$mth)
new$max <- as.numeric(new$max)
hab_agg <- merge(lonlat,new,by=c('lon','lat'),all.x=T)
hab_agg_m <- t(matrix(hab_agg$mth,length(levels(lats_c)),length(levels(lons_c))))

imagePlot(lons[1:(length(lons-1))],
          lats[1:(length(lats)-1)],
          hab_agg_m,breaks=seq(.0,12.5,1),col=magma(12))


lonlat2 <- expand.grid(lons=lons,lats=lats)
lonlat2$lon <- cut(lonlat2$lons,lons)
lonlat2$lat <- cut(lonlat2$lats,lats)
new2 <- merge(new,lonlat2,by=c('lon','lat'),all.x=T)

centr <- matrix(NA,12,2)
for(i in 1:12){
  ind <- which(new2$mth==i)
  # centr[i,] <- cbind(weighted.mean(new2$lons[ind],new2$max[ind],na.rm=T),
                     # weighted.mean(new2$lats[ind],new2$max[ind],na.rm=T))
  centr[i,] <- cbind(mean(new2$lons[ind],na.rm=T),mean(new2$lats[ind],na.rm=T))
}

points(centr,col=turbo(12),pch=paste(1:12),cex=2,lwd=3,typ='o')

plot(centr,typ='o',col=magma(12),pch=paste(1:12),cex=2,lwd=3)

# imagePlot(log10(hab_agg_m),asp=1)
ind_na <- which(hab_agg_m==0)
hab_agg_m[which(hab_agg_m==0)] <- NA
habs_mp <- log10(hab_agg_m)
zeros <- matrix(NA,dim(habs_mp)[1],dim(habs_mp)[2])
zeros[ind_na] <- 0
