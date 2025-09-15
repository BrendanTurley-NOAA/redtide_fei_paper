
library(fields)
library(lubridate)
library(MASS)
library(ncdf4)
# library(rgdal)
library(terra)
library(scales)


lonbox_e <- -79 ### Florida Bay
lonbox_w <- -88 ### mouth of Mississippi River
latbox_n <- 31.5 ### northern coast
latbox_s <- 24 ### southern edge of Ket West

### load map
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ne_10m_admin_0_countries")
world <- vect('ne_10m_admin_0_countries.shp') |>
  crop(ext(lonbox_w, lonbox_e, latbox_s, latbox_n))

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



yrs <- c(2005, 2014, 2017, 2018, 2019)
mths <- 1:12

cols <- colorRampPalette(c('gray50','azure','yellow','orange','red2'),alpha=T)(5)
szs <- c(1,1,1.5,1.5,2)*1.5

# setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
# png('rt_fei_hab_comp2-1.png',width=12,height=11.25,units='in',res=300)
# par(mar = c(2.5,5,2,.5))
# layout(matrix(c(1,1,1,
#                 2,3,4,
#                 5,6,7),3,3,byrow=T),
#        heights = c(.7,1,1))
# 
# plot(kb_qnt+1 ~ yr_m, data = kb_q99, log = 'y',
#      xaxt = 'n', las = 1, typ = 'h', lwd = 3, lend = 2,
#      ylab = '', xlab='')
# abline(v = seq(2000,2022,1)+(8.5/12), lty = 5, col = 4, lwd = 2, lend = 2)
# abline(h = 1e5, lty = 2, lwd = 2, col = 'gray50', lend = 2)
# with(subset(kb_q99, kb_qnt>=1e5),
#      points(kb_qnt+1 ~ yr_m, pch = 25, bg = 'gold', cex = 2))
# axis(1, seq(2000, 2023, 1)+(1/12), seq(2000, 2023, 1), las = 1)
# mtext('K Brevis (cells/mL)',2, line = 3.2, cex = 1)
# mtext(paste0(letters[1],')'),adj=0,font=2)
# points(kb_m+1 ~ yr_m, data = kb_m, typ = 'l', lwd = 2,col='gold')

setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
pdf('rt_spacetime.pdf', 12, 9, pointsize = 10)
par(mfrow = c(3,4))
# pdf('rt_spacetime-2.pdf', 5, 12, pointsize = 6)
# par(mfrow = c(5,12))
for(j in yrs){
  for(k in mths){
    tmp <- subset(habs, year(date)==j & month(date)==k)
    tmp$cuts <- cut(tmp$CELLCOUNT, c(-.01,1e3,1e4,1e5,1e6,1e10))
    tmp <- tmp[order(tmp$CELLCOUNT),]
    
    plot(world,xlim=c(-87.5,-80.5),ylim=c(24,31), col = 'gray80')
    points(tmp$LONGITUDE, tmp$LATITUDE, asp = 1, 
           pch = 21, col = 1,
           cex  = szs[as.numeric(tmp$cuts)],
           bg = cols[as.numeric(tmp$cuts)])
    contour(topo_lon,topo_lat,topo,add=T,
            levels=c(-100),col='gray50',lwd=.75)
    mtext(paste(month.abb[k],j),line=2)
    if(k==12){
      legend(-87.5, 27,c('0-999','1,000-9,999','10,000-99,999','100,000-999,999','>1,000,000'),
             pt.bg = cols[1:5], pch = 21, pt.cex = 1.2,
             title = 'K. brevis (cells/mL)', bty = 'n', xpd = T)
    }
  }
}
dev.off()


