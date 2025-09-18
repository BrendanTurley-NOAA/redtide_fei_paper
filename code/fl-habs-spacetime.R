
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
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cb_2019_us_county_500k")
counties <- vect('cb_2019_us_county_500k.shp')
FL <- subset(counties, counties$STATEFP=='12')

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



### spatial binning

lonbox_e <- -80.6 ### Florida Bay
lonbox_w <- -88 ### mouth of Mississippi River
latbox_n <- 30.6 ### northern coast
latbox_s <- 24.2 ### southern edge of Ket West
ind <- which(habs$LATITUDE<latbox_n & habs$LATITUDE>latbox_s & habs$LONGITUDE<lonbox_e & habs$LONGITUDE>lonbox_w)
habs <- habs[ind,]
ind2 <- which(habs$LONGITUDE>-82 & habs$LATITUDE>27)
habs <- habs[-ind2,]


# range(habs$LONGITUDE)
# lons <- seq(-88, -79.2, .2)
lons <- seq(lonbox_w, lonbox_e, .2)
# range(habs$LATITUDE)
# lats <- seq(24, 31, .2)
lats <- seq(latbox_s, latbox_n, .2)

lon_lat <- expand.grid(LONGITUDE = lons,
                       LATITUDE = lats)
lon_lat$lons <- cut(lon_lat$LONGITUDE, lons, include.lowest = T)
lon_lat$lats <- cut(lon_lat$LATITUDE, lats, include.lowest = T)
lon_lat$spat_bins <- paste(lon_lat$lons, lon_lat$lats)

habs$lons <- cut(habs$LONGITUDE, lons, include.lowest = T)
habs$lats <- cut(habs$LATITUDE, lats, include.lowest = T)
habs$spat_bins <- paste(habs$lons, habs$lats)

# hab_agg1 <- aggregate(CELLCOUNT ~ spat_bins, data = habs, max, na.rm = T)
hab_agg1 <- aggregate(CELLCOUNT ~ spat_bins, data = habs, quantile, .95, na.rm = T)

hab_agg1m <- merge(lon_lat, hab_agg1, by = c('spat_bins'), all = T)
hab_agg1m <- hab_agg1m[order(hab_agg1m$LATITUDE, hab_agg1m$LONGITUDE),]

hab_matrix <- matrix(hab_agg1m$CELLCOUNT, length(lons), length(lats))


imagePlot(lons, lats, log10(hab_matrix+1), asp = 1, col = magma(60))
plot(FL, add = T, border = 'gray40')

brks <- seq(0,8,1)
brks2 <- seq(0,12,2)

# par(mfrow = c(1,2))

setwd("C:/Users/brendan.turley/Documents/R_projects/redtide_fei_paper/figures")
pdf('rt_spacetime2.pdf', 8.5, 8, pointsize = 10)
par(mfrow = c(2,2), mar = c(3,3,2,1))
for(i in 2000:2022){
  # tmp <- subset(habs, year(date)==i) |>
       # aggregate(CELLCOUNT ~ spat_bins, quantile, .95, na.rm = T)
  
  ### cell density
  tmp <- subset(habs, year(date)==i) |>
    aggregate(CELLCOUNT ~ spat_bins + month(date), quantile, .95, na.rm = T) |>
    aggregate(CELLCOUNT ~ spat_bins, quantile, .95, na.rm = T)

  tmp2 <- merge(lon_lat, tmp, by = c('spat_bins'), all = T)
  tmp2 <- tmp2[order(tmp2$LATITUDE, tmp2$LONGITUDE),]
  
  tmp_m <- matrix(tmp2$CELLCOUNT, length(lons), length(lats))
  tot <- length(which(tmp_m>1e4))
  pct <- length(which(tmp_m>1e4))/length(which(!is.na(tmp_m)))
  # tmp_m[which(tmp_m>1e6)] <- 1e6
  
  ### bloom length
  tmp_l <- subset(habs, year(date)==i) |>
    aggregate(CELLCOUNT ~ spat_bins + month(date), quantile, .95, na.rm = T) |>
    aggregate(CELLCOUNT ~ spat_bins, function(x) length(which(x>1e4)))
  
  tmp_l2 <- merge(lon_lat, tmp_l, by = c('spat_bins'), all = T)
  tmp_l2 <- tmp_l2[order(tmp_l2$LATITUDE, tmp_l2$LONGITUDE),]
  
  tmp_lm <- matrix(tmp_l2$CELLCOUNT, length(lons), length(lats))
  max_mth <- max(tmp_lm, na.rm=T)
  
  ### plots
  imagePlot(lons, lats, log10(tmp_m+1), asp = 1, 
            col = magma(length(brks)-1), breaks = brks,
            xlab = '', ylab = '',
            xlim = c(-88, -80), ylim = c(24, 31))
  # plot(world,xlim=c(-87.5,-80.5),ylim=c(24,31), add = T, border = 'gray')
  plot(FL, add = T, border = 'gray40')
  mtext(adj = 1, 'K. brevis Log10(cells/mL)')
  mtext(adj = 0, paste ('year:',i))
  text(-87.9, 25.5, adj = 0, font = 2, 'Bins >1e4 cells/mL')
  text(-87.9, 25, adj = 0, paste('total:', tot))
  text(-87.9, 24.5, adj = 0, paste('proportion:', round(pct, digits = 2)))
  
  imagePlot(lons, lats, tmp_lm, asp = 1, 
            col = viridis(length(brks2)-1), breaks = brks2,
            xlab = '', ylab = '',
            xlim = c(-88, -80), ylim = c(24, 31))
  # plot(world,xlim=c(-87.5,-80.5),ylim=c(24,31), add = T, border = 'gray')
  plot(FL, add = T, border = 'gray40')
  mtext(adj = 1, '# of months (K. brevis >1e4 cells/mL)')
  text(-86, 25, adj = 1, paste('max:', max_mth))
}
dev.off()


brks <- seq(0,8,2)
brks2 <- seq(0,12,4)
par(mfrow = c(1,2))
for(i in 2000:2022){
  # tmp <- subset(habs, year(date)==i) |>
  # aggregate(CELLCOUNT ~ spat_bins, quantile, .95, na.rm = T)
  
  ### cell density
  tmp <- subset(habs, year(date)==i) |>
    aggregate(CELLCOUNT ~ spat_bins + month(date), quantile, .95, na.rm = T) |>
    aggregate(CELLCOUNT ~ spat_bins, quantile, .95, na.rm = T)
  
  ### bloom length
  tmp_l <- subset(habs, year(date)==i) |>
    aggregate(CELLCOUNT ~ spat_bins + month(date), quantile, .95, na.rm = T) |>
    aggregate(CELLCOUNT ~ spat_bins, function(x) length(which(x>1e4)))
  
  
  tmp2 <- merge(lon_lat, tmp, by = c('spat_bins'), all = T) |>
    merge(tmp_l, by = c('spat_bins'), all = T)
  
  tmp2$xq <- cut(log10(tmp2$CELLCOUNT.x+1), brks, include.lowest = T)
  tmp2$yq <- cut(tmp2$CELLCOUNT.y, brks2, include.lowest = T)
  
  tmp2$group <- paste(as.numeric(tmp2$xq), as.numeric(tmp2$yq), sep = '-')
  tmp3 <- merge(tmp2, bivariate_color_scale, by = 'group', all.x = T)
  tmp3$fill <- as.factor(tmp3$fill)
  
  tmp3 <- tmp3[order(tmp3$LATITUDE, tmp3$LONGITUDE),]
  
  tmp_m <- matrix(as.numeric(tmp3$fill), length(lons), length(lats))
  
  # cols <- bivariate_color_scale$fill
  cols <- levels(tmp3$fill)
  imagePlot(lons, lats, tmp_m, asp = 1, col = cols)
  
}

bi_pal(pal = "PurpleOr", dim = 4, flip_axes = F, preview = T)

library(biscale)
cols <- bi_pal(pal = "PurpleOr", dim = 4, flip_axes = F, preview = F)

bivariate_color_scale <- data.frame(group = names(cols),
                                    fill = cols)

# group    fill
# 1    4-4 '#d3d3d3'
# 2    4-3 '#d3af95'
# 3    4-2 '#d28753'
# 4    4-1 '#d25601'
# 5    3-4 '#a89db9'
# 6    3-3 '#a88283'
# 7    3-2 '#a86448'
# 8    3-1 '#a84001'
# 9    2-4 '#7e6a9f'
# 10   2-3 '#7e5771'
# 11   2-2 '#7e433e'
# 12   2-1 '#7e2b01'
# 13   1-4 '#563787'
# 14   1-3 '#562d5f'
# 15   1-2 '#552335'
# 16   1-1 '#551601'

# group_fill <- expand.grid(4:1,4:1) |>
#   arrange(desc(Var1), desc(Var2))
# bivariate_color_scale <- data.frame(group = paste(group_fill$Var1, group_fill$Var2, sep = '-'),
#                                     fill = cols)

colm <- factor(bivariate_color_scale$fill, levels = bivariate_color_scale$fill)
col_coord <- matrix(as.numeric(unlist(strsplit(bivariate_color_scale$group,'-'))),2,16)

image(1:4,1:4,
  matrix(as.numeric(colm),4,4), col = bivariate_color_scale$fill,
  asp = 1, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
text(col_coord[1,],col_coord[2,],bivariate_color_scale$fill)

