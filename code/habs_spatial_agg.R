
library(fields)
library(lubridate)
library(MASS)
library(ncdf4)
# library(rgdal)
library(terra)
library(scales)

### load map
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ne_10m_admin_0_countries")
world <- vect('ne_10m_admin_0_countries.shp')

setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

setwd("C:/Users/brendan.turley/Documents/data/habs/0120767/8.8/data/0-data")
# original data requested from https://habsos.noaa.gov/about
habs1 <- read.csv('habsos_20240430.csv')
habs1 <- habs1[,-grep('X',names(habs1))]
habs1$date <- ymd(habs1$SAMPLE_DATE)
table(year(habs1$date), month(habs1$date))
habs <- habs1[which(year(habs1$date)<2023 & year(habs1$date)>1999 & habs1$STATE_ID=='FL' ),]
table(year(habs$date), month(habs$date))

### duplication
# names(habs)[c(3:4,6,26)]
# dups1 <- which(duplicated(habs[c(3:4,6,26)]))
# dups2 <- which(duplicated(habs[c(3:4,6,26)],fromLast = T))
# 
# new <- matrix(NA,length(dups1),1)
# for(i in 1:length(dups1)){
#   # new[i] <- mean(data$Karenia.brevis.abundance..cells.L.[c(dups1[i],dups2[i])],na.rm=T)
#   new[i,] <- mean(habs[c(dups1[i],dups2[i]),11],na.rm=T)
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
table(year(habs$date), month(habs$date))




### timeseries

kb_m <- aggregate(CELLCOUNT ~ year(date) + month(date), data = habs, quantile, .75, na.rm = T) |>
  setNames(c('year', 'month', 'kb_m'))
kb_m <- kb_m[order(kb_m$year, kb_m$month),]
kb_m$yr_m <- kb_m$year + kb_m$month/12

kb_q99 <- aggregate(CELLCOUNT ~ year(date) + month(date), data = habs, quantile, .95, na.rm = T) |>
  setNames(c('year', 'month', 'kb_qnt'))
kb_q99 <- kb_q99[order(kb_q99$year, kb_q99$month),]
kb_q99$yr_m <- kb_q99$year + kb_q99$month/12

cols <- colorRampPalette(c('gray50','azure','yellow','orange','red2'),alpha=T)(5)

szs <- c(1,1,1.5,1.5,2)*1.5


# setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
# png('rt_fei_hab_comp.png',width=12,height=12,units='in',res=300)
# par(mar = c(2.5,5,2,.5))
# layout(matrix(c(1,1,1,1,
#                 2,3,4,5,
#                 6,7,8,9,
#                 10,11,12,13),4,4,byrow=T))
# 
# plot(kb_qnt+1 ~ yr_m, data = kb_q99, typ = 'h', log = 'y', xaxt = 'n', las = 1, lwd = 2, lend = 2,
#      ylab = '', xlab='')
# mtext('K Brevis (cells/mL)',2, line = 3.2, cex = 1)
# abline(h = 1e5, lty = 5)
# with(subset(kb_q99, kb_qnt>=1e5),
#      points(kb_qnt+1 ~ yr_m, pch = 25, bg = 2))
# axis(1, seq(2000, 2025, 1)+(1/12), seq(2000, 2025, 1), las = 1)
# abline(v = seq(2000,2025,1)+(9/12), lty = 5, col = 2)
# mtext(paste0(letters[1],')'),adj=0)
# # points(kb_m+1 ~ yr_m, data = kb_m, typ = 'l', lwd = 2,col='gold')
# 
# # setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
# # png('rt_fei_hab_comp2.png',width=10,height=12,units='in',res=300)
# # par(mfrow = c(4,3), mar = c(3,3,1,1))
# for(j in c(2005)){
#   for(i in 1:12){
#     tmp <- subset(habs, year(date)==j & month(date)==i)
#     tmp$cuts <- cut(tmp$CELLCOUNT, c(-.01,1e3,1e4,1e5,1e6,1e10))
#     tmp <- tmp[order(tmp$CELLCOUNT),]
#     
#     plot(world,xlim=c(-87.5,-80.5),ylim=c(24,31), col = 'gray80')
#     points(tmp$LONGITUDE, tmp$LATITUDE, asp = 1, pch = 21,
#            cex  = szs[as.numeric(tmp$cuts)],
#            bg = cols[as.numeric(tmp$cuts)])
#     contour(topo_lon,topo_lat,topo,add=T,
#             levels=c(-100),col='gray50',lwd=.75)
#     mtext(paste(month.name[i],2005),adj=1)
#     mtext(paste0(letters[i+1],')'),adj=0)
#     # plot(world, add = T)
#     if(i==12){
#       legend('bottomleft',c('0-1000','1000-10,000','10,000-100,000','100,000-1,000,000','>1,000,000'),
#              pt.bg = cols[1:5], pch = 21, pt.cex = 1.75, cex = 1.2,
#              title = 'K. brevis (cells/mL)', bty = 'n', xpd = T)
#     }
#   } 
# }
# dev.off()


yrs <- c(2008, 2009, 2010, 2005, 2014, 2018)
mths <- 8:10

setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
png('rt_fei_hab_comp2-1.png',width=12,height=11.25,units='in',res=300)
par(mar = c(2.5,5,2,.5))
layout(matrix(c(1,1,1,
                2,3,4,
                5,6,7),3,3,byrow=T),
       heights = c(.7,1,1))

plot(kb_qnt+1 ~ yr_m, data = kb_q99, log = 'y',
     xaxt = 'n', las = 1, typ = 'h', lwd = 3, lend = 2,
     ylab = '', xlab='')
abline(v = seq(2000,2022,1)+(8.5/12), lty = 5, col = 4, lwd = 2, lend = 2)
abline(h = 1e5, lty = 2, lwd = 2, col = 'gray50', lend = 2)
with(subset(kb_q99, kb_qnt>=1e5),
     points(kb_qnt+1 ~ yr_m, pch = 25, bg = 'gold', cex = 2))
axis(1, seq(2000, 2023, 1)+(1/12), seq(2000, 2023, 1), las = 1)
mtext('K Brevis (cells/mL)',2, line = 3.2, cex = 1)
mtext(paste0(letters[1],')'),adj=0,font=2)
# points(kb_m+1 ~ yr_m, data = kb_m, typ = 'l', lwd = 2,col='gold')

n <- 2
for(j in yrs){
  tmp <- subset(habs, year(date)==j & is.element(month(date), mths))
  tmp$cuts <- cut(tmp$CELLCOUNT, c(-.01,1e3,1e4,1e5,1e6,1e10))
  tmp <- tmp[order(tmp$CELLCOUNT),]
  
  plot(world,xlim=c(-87.5,-80.5),ylim=c(24,31), col = 'gray80')
  points(tmp$LONGITUDE, tmp$LATITUDE, asp = 1, 
         pch = 21, col = 1,
         cex  = szs[as.numeric(tmp$cuts)],
         bg = cols[as.numeric(tmp$cuts)])
  contour(topo_lon,topo_lat,topo,add=T,
          levels=c(-100),col='gray50',lwd=.75)
  mtext(paste('Aug-Oct',j),adj=1)
  mtext(paste0(letters[n],')'),adj=0,font=2)
  if(j==2018){
    legend('bottomleft',c('0-999','1,000-9,999','10,000-99,999','100,000-999,999','>1,000,000'),
           pt.bg = cols[1:5], pch = 21, pt.cex = 1.75, cex = 1.5,
           title = 'K. brevis (cells/mL)', bty = 'n', xpd = T)
  }
  n <- n + 1
}
dev.off()



lons <- seq(lonbox_w,lonbox_e,by=.05)
lats <- seq(latbox_s,latbox_n,by=.05)

lats_c <- cut(habs$LATITUDE,lats)
lons_c <- cut(habs$LONGITUDE,lons)
lonlat <- expand.grid(lon=levels(lons_c),lat=levels(lats_c))
h_agg <- aggregate(habs$CELLCOUNT,by=list(lon=lons_c,lat=lats_c),mean,na.rm=T)
hab_agg <- merge(lonlat,h_agg,by=c('lon','lat'),all=T)
hab_agg_m <- t(matrix(hab_agg$x,length(levels(lats_c)),length(levels(lons_c))))
### display zeros
ind_na <- which(hab_agg_m==0)
hab_agg_m[which(hab_agg_m==0)] <- NA
habs_mp <- log10(hab_agg_m)
zeros <- matrix(NA,dim(habs_mp)[1],dim(habs_mp)[2])
zeros[ind_na] <- 0


fxn <- function(x) {
  # mean(x[which(x>quantile(x,.5,na.rm=T))],na.rm=T)
  # length(which(x>quantile(x,.5,na.rm=T)))
  # c(#length(which(x==0))/length(x),
  #   length(which(x>=1e1 & x<1e2))/length(x),
  #   length(which(x>=1e2 & x<1e3))/length(x),
  #   length(which(x>=1e3 & x<1e4))/length(x),
  #   length(which(x>=1e4 & x<1e5))/length(x),
  #   length(which(x>=1e5))/length(x))
  # length(which(x>1e5))/length(which(x>0))
  # length(which(x==0))
  # length(which(x>1e5))
  # length(which(x>=1e5))/length(x) ### PREFERRED
  c(#length(which(x==0))/length(x),
    #length(which(x>=0 & x<1e3))/length(x),
    length(which(x>=1e3 & x<1e4))/length(x),
    length(which(x>=1e4 & x<1e5))/length(x),
    length(which(x>=1e5 & x<1e6))/length(x),
    length(which(x>=1e6))/length(x))
}


cols <- colorRampPalette(c('gray','gold','orange2','orangered3','red4'))(4)
cols <- colorRampPalette(c('gold','orange2','orangered3','red4'))(4)

x <- aggregate(habs$CELLCOUNT,by=list(year(habs$date),month(habs$date)),fxn)
x <- x[order(x$Group.1,x$Group.2),]

b <- barplot(t(x$x[,ncol(x$x)]),las=2,col=rev(cols))
axis(1, at = b[seq(1,length(b),12)],unique(x$Group.1), las = 2)

b <- barplot(t(x$x[,ncol(x$x):1]),las=2,col=rev(cols))
axis(1, at = b[seq(1,length(b),12)],unique(x$Group.1), las = 2)

b <- barplot(t(x$x),las=2,col=(cols))
axis(1, at = b[seq(1,length(b),12)],unique(x$Group.1), las = 2)



x <- aggregate(habs$CELLCOUNT,by=list(month(habs$date)),
               function(x) length(which(x>=1e5))/length(x))

# setwd('~/Desktop/professional/projects/Postdoc_FL/figures')
setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
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



### timeseries

kb_m <- aggregate(CELLCOUNT ~ year(date) + month(date), data = habs, quantile, .75, na.rm = T) |>
  setNames(c('year', 'month', 'kb_m'))
kb_m <- kb_m[order(kb_m$year, kb_m$month),]
kb_m$yr_m <- kb_m$year + kb_m$month/12

kb_q99 <- aggregate(CELLCOUNT ~ year(date) + month(date), data = habs, quantile, .95, na.rm = T) |>
  setNames(c('year', 'month', 'kb_qnt'))
kb_q99 <- kb_q99[order(kb_q99$year, kb_q99$month),]
kb_q99$yr_m <- kb_q99$year + kb_q99$month/12

plot(kb_qnt+1 ~ yr_m, data = kb_q99, typ = 'h', log = 'y', xaxt = 'n', las = 1, lwd = 2, lend = 2)
abline(h = 1e5, lty = 5)
with(subset(kb_q99, kb_qnt>=1e5),
     points(kb_qnt+1 ~ yr_m, pch = 25, bg = 2))
axis(1, seq(2000, 2025, 1), las = 2)
abline(v = seq(2000,2025,1)+(9/12), lty = 5, col = 2)
# points(kb_m+1 ~ yr_m, data = kb_m, typ = 'l', lwd = 2)


lons <- seq(lonbox_w,lonbox_e,by=.2)
lats <- seq(latbox_s,latbox_n,by=.2)

lats_c <- cut(habs$LATITUDE,lats)
lons_c <- cut(habs$LONGITUDE,lons)
lonlat_yr_mth <- expand.grid(lon=levels(lons_c),lat=levels(lats_c),year=2000:2022,month=1:12)
h_agg <- aggregate(habs$CELLCOUNT,
                   by=list(lon=lons_c,lat=lats_c,year(habs$date),month(habs$date)),
                   mean,na.rm=T) |>
  setNames(c('lon','lat','year','month','cellcount'))

read_habs <- function(dat){
  out_m <- t(matrix(dat$cellcount,length(levels(lats_c)),length(levels(lons_c))))
  ind_na <- which(out_m==0)
  out_m[which(out_m==0)] <- NA
  out_m <- log10(out_m)
  zeros <- matrix(NA,dim(out_m)[1],dim(out_m)[2])
  zeros[ind_na] <- 0
  return(list(out_m,zeros))
}

yr <- 2018

par(mfrow=c(3,4))
for(i in 1:12){
  tmp <- subset(h_agg, year==yr & month==i)
  tmp_exp <- expand.grid(lon=levels(lons_c),lat=levels(lats_c),year=yr,month=i)
  hab_agg <- merge(tmp_exp,tmp,by=c('lon','lat','year','month'),all=T)
  res <- read_habs(hab_agg)
  
  image(lons[1:(length(lons-1))],
        lats[1:(length(lats)-1)],
        res[[1]])
  image(lons[1:(length(lons-1))],
        lats[1:(length(lats)-1)],
        res[[2]],col=1,add=T)
  plot(world,add=T,col='gray70')
  
}



lons <- seq(lonbox_w,lonbox_e,by=.5)
lats <- seq(latbox_s,latbox_n,by=.5)
lats_c <- cut(habs$LATITUDE,lats)
lons_c <- cut(habs$LONGITUDE,lons)
# lonlat <- expand.grid(lon=levels(lons_c),lat=levels(lats_c))
lonlat_yr_mth <- expand.grid(lon=levels(lons_c),lat=levels(lats_c),year=2000:2022,month=1:12)
h_agg <- aggregate(habs$CELLCOUNT,
                   by=list(lon=lons_c,lat=lats_c,year=year(habs$date),month=month(habs$date)),
                   mean,na.rm=T)
hab_agg <- merge(lonlat_yr_mth,h_agg,by=c('lon','lat','year','month'),all=T)
hab_agg$grid <- paste(hab_agg$lon,hab_agg$lat)

hab_agg2 <- aggregate(x ~ year + month + grid, data = hab_agg, mean, na.rm = T)

hab_agg3 <- aggregate(x ~ year + month, data = hab_agg, function(x) length(which(x>=1e5))/length(x))
hab_agg3 <- hab_agg3[order(hab_agg3$year, hab_agg3$month),]

b <- barplot(hab_agg3$x,las=2,col=2)
axis(1, at = b[seq(1,length(b),12)],unique(x$Group.1), las = 2)

b <- barplot(t(x$x[,ncol(x$x):1]),las=2,col=rev(cols))
axis(1, at = b[seq(1,length(b),12)],unique(x$Group.1), las = 2)

library(cmocean)
library(dplyr)
library(sf)
library(viridis)

tmp <- subset(habs, year(date)==2010 & month(date)==9)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
df <- st_as_sf(x = tmp,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = st_crs(world))
test <- st_make_grid(df, c(.2,.2), what = "polygons", square = FALSE)
# To sf and add grid ID
honeycomb_grid_sf = st_sf(test) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(test)))


hex_data <- st_join(honeycomb_grid_sf, df, join = st_intersects) %>%
  group_by(grid_id) %>%
  summarize(mean_value = mean(CELLCOUNT, na.rm = TRUE))
hex_data_zeros = filter(hex_data, mean_value > 0 | !is.na(mean_value))

hex_data_zeros <- hex_data_zeros |> mutate(mean_value = ifelse(mean_value==0,mean_value + 1, mean_value))

# plot(hex_data)
plot(hex_data_zeros['mean_value'],reset=F,logz=T, pal = cmocean('amp'))
plot(st_as_sf(world), col = 'gray', add = T)

plot(world,xlim=c(-88,-80),ylim=c(24,31))
plot(hex_data_zeros['mean_value'],add=T,logz=T)





# habs_h <- aggregate(habs$CELLCOUNT,by=list(lon=lons_c,lat=lats_c),length)
# hab_h <- merge(lonlat,habs_h,by=c('lon','lat'),all=T)
# hab_h_m <- t(matrix(hab_h$x,length(levels(lats_c)),length(levels(lons_c))))
# hab_h_m[which(hab_h_m==0)] <- NA
# habs_hp <- log10(hab_h_m)
# 
# png('rt_fei_map2.png',width=7,height=6,units='in',res=300)
# par(mar=c(4.5,4.5,1,1))
# imagePlot(lons[1:(length(lons-1))],
#           lats[1:(length(lats)-1)],
#           habs_hp,
#           asp=1,col=mako(60),nlevel=59,
#           xlab='Longitude',ylab='Latitude',
#           legend.lab = expression(paste('Total number of samples (log'[10],')')))
# plot(world,add=T,col='gray70')
# contour(topo_lon,topo_lat,topo,add=T,
#         levels=c(-200),col='gray50',lwd=.75)
# dev.off()



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
