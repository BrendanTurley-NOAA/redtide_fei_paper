library(fields)
library(interp)
library(lubridate)
library(sf)
library(terra)


### loading bathymetry
setwd("~/Desktop/professional/biblioteca/data")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

### loading shape file for coastline
# setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
# world <- readOGR('ne_10m_admin_0_countries.shp')
setwd("~/Desktop/professional/biblioteca/data/shapefiles/Florida_Shoreline__1_to_40%2C000_Scale_-shp")
# FL <- readOGR('Florida_Shoreline__1_to_40%2C000_Scale_.shp')
FL <- vect('Florida_Shoreline__1_to_40%2C000_Scale_.shp')
FL <- st_as_sf(FL)


setwd('/Users/Brendan/Desktop/professional/projects/Postdoc_FL/data/FCWC/processed/v0')
files <- list.files()
ind_csv <-grep('interp',files)
### which dates do you want
date <- '2021-07'
ind <- grep(date,files[ind_csv])
ind1 <- ind[-c(1:2)]
date <- '2021-08'
ind2 <- grep(date,files[ind_csv])
ind <- c(ind1,ind2)
### create empty dataframe to store output
data_all <- read.csv(files[ind_csv][1],stringsAsFactors = F)
m <- 1
n <- 0
for(i in ind){
  csv <- files[ind_csv][i]
  data <- read.csv(csv,stringsAsFactors = F)
  n <- nrow(data) + n
  data_all[m:n,] <- data
  data_all$station[m:n] <- i
  m <- n + 1
}
data_all$dtime_utc <- ymd_hms(data_all$dtime_utc)
data_all <- na.omit(data_all)
data_all$depth_m <- -data_all$depth_m
data_all <- data_all[order(data_all$dtime_utc),]
data_all$station <- as.numeric(data_all$station)
data_all <- data_all[data_all$station!=54,]

plot(data_all$lon_dd,data_all$lat_dd,typ='n')
text(data_all$lon_dd,data_all$lat_dd,data_all$station)

data_in <- data_all[is.element(data_all$station,seq(42,52,2)),]
bottoms_in <- bottom_finder(data_in$lat_dd,data_in$depth_m)

data_out <- data_all[is.element(data_all$station,seq(41,53,2)),]
data_off <- data_all[is.element(data_all$station,52:60),]
# data_off <- data_off[data_off$station!=54,]
data_off2 <- data_all[is.element(data_all$station,61:70),]

seg1 <- cbind(unique(data_in$lon_dd),unique(data_in$lat_dd))
seg2 <- cbind(unique(data_out$lon_dd),unique(data_out$lat_dd))
seg3 <- cbind(unique(data_off$lon_dd),unique(data_off$lat_dd))
seg4 <- cbind(unique(data_off2$lon_dd),unique(data_off2$lat_dd))
seg4 <- seg4[order(seg4[,2]),]

### resolution for interpolation
z_res <- 100
x_res <- z_res*3
### nearshore
temp_in <- interp(data_in$lat_dd,data_in$depth_m,data_in$temp_c,
                  yo=seq(min(data_in$depth_m,na.rm=T), max(data_in$depth_m,na.rm=T), length = z_res),
                  xo=seq(min(data_in$lat_dd,na.rm=T), max(data_in$lat_dd,na.rm=T), length = x_res),
                  duplicate='mean')
sal_in <- interp(data_in$lat_dd,data_in$depth_m,data_in$sal_ppt,
                 yo=seq(min(data_in$depth_m,na.rm=T), max(data_in$depth_m,na.rm=T), length = z_res),
                 xo=seq(min(data_in$lat_dd,na.rm=T), max(data_in$lat_dd,na.rm=T), length = x_res),
                 duplicate='mean')
chl_in <- interp(data_in$lat_dd,data_in$depth_m,data_in$chl_ugl,
                 yo=seq(min(data_in$depth_m,na.rm=T), max(data_in$depth_m,na.rm=T), length = z_res),
                 xo=seq(min(data_in$lat_dd,na.rm=T), max(data_in$lat_dd,na.rm=T), length = x_res),
                 duplicate='mean')
do_in <- interp(data_in$lat_dd,data_in$depth_m,data_in$do_mgl,
                yo=seq(min(data_in$depth_m,na.rm=T), max(data_in$depth_m,na.rm=T), length = z_res),
                xo=seq(min(data_in$lat_dd,na.rm=T), max(data_in$lat_dd,na.rm=T), length = x_res),
                duplicate='mean')

### unique color ramps for plotting each variable
temp_col <- colorRampPalette(c(1,'purple','darkorange','gold'))
sal_col <- colorRampPalette(c('purple4','dodgerblue4','seagreen3','khaki1'))
chl_col <- colorRampPalette(c('honeydew2','darkseagreen3','forestgreen','darkslategrey'))
ox.col1 <- colorRampPalette(c(1,'firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('dodgerblue4','deepskyblue2','cadetblue1'))

par(mfrow=c(2,2))
hist(c(data_in$temp_c))
hist(c(data_in$sal_ppt))
hist(c(data_in$chl_ugl))
hist(c(data_in$do_mgl))

t_breaks <- breaks(data_in$temp_c,.1)
t_col <- temp_col(length(t_breaks)-1)
s_breaks <- seq(36,38,.2)
s_col <- sal_col(length(s_breaks)-1)
c_breaks <- seq(0,.5,.05)
c_col <- chl_col(length(c_breaks)-1)
o_breaks <- seq(5.8,6.3,.05)
ox_cols <- ox.col3(length(o_breaks)-1)

### inshore
sal_in$z[which(sal_in$z<36)] <- 36
sal_in$z[which(sal_in$z>38)] <- 38
chl_in$z[which(chl_in$z>1)] <- 1
do_in$z[which(do_in$z<5.8)] <- 5.8
do_in$z[which(do_in$z>6.3)] <- 6.3


setwd("~/Desktop/professional/projects/Postdoc_FL/figures/FCWC")
png(paste('FCWC_in_',paste(year(data_in$dtime_utc[1]),month.abb[month(data_in$dtime_utc[1])],sep='-'),'.png',sep=''), 
    width = 10, height = 10, units = 'in', res = 300)
par(mfrow=c(3,2),mar=c(5,5,2,2),oma=c(1,1,1,1))
# image(temp_in,breaks=t_breaks,col=t_col,las=1,xlab='',ylab='')
imagePlot(temp_in,breaks=t_breaks,col=t_col,las=1,xlab='',ylab='',lowerTriangle = F,upperTriangle = F,legend.shrink = 1)
contour(temp_in,add=T,levels=t_breaks)
points(data_in$lat_dd,data_in$depth_m,pch=20,cex=.25,col='chartreuse2')
polygon(c(bottoms_in[1,2],bottoms_in[,2],bottoms_in[nrow(bottoms_in),2]),c(bottoms_in[1,1]-100,bottoms_in[,1],bottoms_in[nrow(bottoms_in),1]-100),col='wheat4')
mtext(expression(paste('Latitude (',degree,'N)')),1,line=3)
mtext('Depth (m)',2,line=3)
mtext(expression(paste('Temperature (',degree,'C)')),3,line=0.2,adj=1)
mtext('a)',adj=-.1,line=.5)

# image(sal_in,breaks=s_breaks,col=s_col,las=1,xlab='',ylab='')
imagePlot(sal_in,breaks=s_breaks,col=s_col,las=1,xlab='',ylab='',lowerTriangle = T,upperTriangle = T,legend.shrink = 1)
contour(sal_in,add=T,levels=s_breaks)
points(data_in$lat_dd,data_in$depth_m,pch=20,cex=.25)
polygon(c(bottoms_in[1,2],bottoms_in[,2],bottoms_in[nrow(bottoms_in),2]),c(bottoms_in[1,1]-100,bottoms_in[,1],bottoms_in[nrow(bottoms_in),1]-100),col='wheat4')
mtext(expression(paste('Latitude (',degree,'N)')),1,line=3)
mtext('Depth (m)',2,line=3)
mtext('Salinity (PSU)',3,line=0.2,adj=1)
mtext('b)',adj=-.1,line=.5)

# image(chl_in,breaks=c_breaks,col=c_col,las=1,xlab='',ylab='')
imagePlot(chl_in,breaks=c_breaks,col=c_col,las=1,xlab='',ylab='',lowerTriangle = F,upperTriangle = T,legend.shrink = 1)
contour(chl_in,add=T,levels=c_breaks)
points(data_in$lat_dd,data_in$depth_m,pch=20,cex=.25)
polygon(c(bottoms_in[1,2],bottoms_in[,2],bottoms_in[nrow(bottoms_in),2]),c(bottoms_in[1,1]-100,bottoms_in[,1],bottoms_in[nrow(bottoms_in),1]-100),col='wheat4')
mtext(expression(paste('Latitude (',degree,'N)')),1,line=3)
mtext('Depth (m)',2,line=3)
mtext(expression(paste('Chlorophyll-a (',mu,'g l'^-1,')')),3,line=0.2,adj=1)
mtext('c)',adj=-.1,line=.5)

# image(do_in,breaks=o_breaks,col=ox_cols,las=1,xlab='',ylab='')
imagePlot(do_in,breaks=o_breaks,col=ox_cols,las=1,xlab='',ylab='',lowerTriangle = T,upperTriangle = F,legend.shrink = 1)
contour(do_in,add=T,levels=o_breaks,col='gray90')
points(data_in$lat_dd,data_in$depth_m,pch=20,cex=.25,col='orange')
polygon(c(bottoms_in[1,2],bottoms_in[,2],bottoms_in[nrow(bottoms_in),2]),c(bottoms_in[1,1]-100,bottoms_in[,1],bottoms_in[nrow(bottoms_in),1]-100),col='wheat4')
mtext(expression(paste('Latitude (',degree,'N)')),1,line=3)
mtext('Depth (m)',2,line=3)
mtext(expression(paste('Dissolved Oxygen (mg l'^-1,')')),3,line=0.2,adj=1)
mtext('d)',adj=-.1,line=.5)

# plot(FL,xlim=c(min(data_in$lon_dd)-.5,max(data_in$lon_dd)+.5),ylim=c(min(data_in$lat_dd)-.5,max(data_in$lat_dd)+.5),col='gray90',xlab='')
# contour(topo_lon,topo_lat,topo,add=T,levels=c(-100,-50,-25,-10))
# points(unique(data_in$lon_dd),unique(data_in$lat_dd),pch=21,bg='orange',cex=2)
# mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
# mtext(paste(year(data_in$dtime_utc[1]),month.abb[month(data_in$dtime_utc[1])],day(data_in$dtime_utc[1]),sep='-'),3,line=0.2,adj=1)
# axis(1,seq(floor(min(data_in$lon_dd)-.5),ceiling(max(data_in$lon_dd)+.5),.5),las=1)
# axis(2,seq(floor(min(data_in$lat_dd)-.5),ceiling(max(data_in$lat_dd)+.5),.5),las=1)
# box()
# grid()


# setwd("~/Desktop/professional/projects/Postdoc_FL/figures/FCWC")
# png(paste('FCWC_overview_',paste(year(data_all$dtime_utc[1]),month.abb[month(data_all$dtime_utc[1])],day(data_all$dtime_utc[1]),sep='-'),'.png',sep=''), width = 7.5, height = 7.5, units = 'in', res = 300)
# par(mfrow=c(1,1),mar=c(5,5,2,2),oma=c(1,1,1,1))
plot(1,1,
     xlim=c(min(data_all$lon_dd)-.5,max(data_all$lon_dd)+.5),
     ylim=c(min(data_all$lat_dd)-.5,max(data_all$lat_dd)+.5),
     xlab='',ylab='',las=1,asp=1)
plot(st_geometry(FL),add=T,col='gray90')
# plot(FL,xlim=c(min(data_all$lon_dd)-.5,max(data_all$lon_dd)+.5),ylim=c(min(data_all$lat_dd)-.5,max(data_all$lat_dd)+.5),col='gray90',xlab='')
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10))
points(unique(data_all$lon_dd),unique(data_all$lat_dd),pch=21,bg='orange',cex=2)
segments(seg1[1:(nrow(seg1)-1),1],seg1[1:(nrow(seg1)-1),2],
         seg1[2:nrow(seg1),1],seg1[2:nrow(seg1),2],col=1,lwd=4,lend=2)
segments(seg2[1:(nrow(seg2)-1),1],seg2[1:(nrow(seg2)-1),2],
         seg2[2:nrow(seg2),1],seg2[2:nrow(seg2),2],col=2,lwd=4,lend=2)
segments(seg3[1:(nrow(seg3)-1),1],seg3[1:(nrow(seg3)-1),2],
         seg3[2:nrow(seg3),1],seg3[2:nrow(seg3),2],col=3,lwd=4,lend=2)
segments(seg4[1:(nrow(seg4)-1),1],seg4[1:(nrow(seg4)-1),2],
         seg4[2:nrow(seg4),1],seg4[2:nrow(seg4),2],col=4,lwd=4,lend=2)
grid()
legend('bottomleft',paste('Segment',1:4,sep=' '),col=1:4,lwd=4,bty='n')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(paste(paste(year(data_all$dtime_utc[1]),month.abb[month(data_all$dtime_utc[1])],day(data_all$dtime_utc[1]),sep='-'),
            paste(month.abb[month(data_all$dtime_utc[nrow(data_all)])],day(data_all$dtime_utc[nrow(data_all)]),sep='-'),sep=' to '),
      3,line=0.2,adj=1)
axis(1,seq(floor(min(data_all$lon_dd)-.5),ceiling(max(data_all$lon_dd)+.5),.5),las=1)
axis(2,seq(floor(min(data_all$lat_dd)-.5),ceiling(max(data_all$lat_dd)+.5),.5),las=1)
box()
mtext('e)',adj=-.1,line=.5)
dev.off()

