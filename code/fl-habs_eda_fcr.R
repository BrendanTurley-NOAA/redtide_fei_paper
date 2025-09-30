
library(dplyr)
library(fields)
library(lubridate)
library(MASS)
library(ncdf4)
library(readxl)
library(terra)
library(scales)
library(sf)


lonbox_e <- -79 ### Florida Bay
lonbox_w <- -88 ### mouth of Mississippi River
latbox_n <- 31.5 ### northern coast
latbox_s <- 24 ### southern edge of Key West

### load map
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cb_2019_us_county_500k")
counties <- vect('cb_2019_us_county_500k.shp')
FL <- subset(counties, counties$STATEFP=='12')

### coastal counties
setwd("C:/Users/brendan.turley/Documents/data/")
coast_co <- read_excel('coastline-counties-list.xlsx', skip = 3)
fl_gulf <- subset(coast_co, `STATE NAME` == 'Florida' & `COASTLINE REGION` == 'Gulf of Mexico')
names(fl_gulf) <- c('state.county.fips', 'state.fips', 'COUNTYFP', 'county.name', 'state.name', 'coastline.region', '2016.population.est')

fl_gulfco <- merge(FL, fl_gulf, by = c('COUNTYFP'), all.y = T) |>
  st_as_sf()

x <- st_centroid(fl_gulfco) |> st_coordinates()
fl_gulfco <- fl_gulfco[order(x[,1],x[,2]), ]
fl_gulfco <- fl_gulfco[c(1:12,14:16,13,19,18,17,20:23), ]
fl_co_order <- fl_gulfco$NAME

# setwd("C:/Users/brendan.turley/Documents/data/bathy")
# bathy <- nc_open('etopo1.nc')
# topo <- ncvar_get(bathy, 'Band1')
# topo_lat <- ncvar_get(bathy, 'lat')
# topo_lon <- ncvar_get(bathy, 'lon')
# nc_close(bathy)

setwd("C:/Users/brendan.turley/Documents/data/habs/0120767/8.8/data/0-data")
# original data requested from https://habsos.noaa.gov/about
habs1 <- read.csv('habsos_20240430.csv')
habs1 <- habs1[,-grep('X',names(habs1))]
habs1$date <- ymd(habs1$SAMPLE_DATE)
habs <- habs1[which(year(habs1$date)<2023 & year(habs1$date)>1999 & habs1$STATE_ID=='FL' ),]

habs_sf <- st_as_sf(habs, coords = c('LONGITUDE','LATITUDE'), crs = crs(fl_gulfco))

hab_buf <- fl_gulfco |>
  st_buffer(dist = 10) |>
  st_join(habs_sf)

### define some bloom thresholds
overall <- sapply(c(1e3,1e4,1e5,1e6), function(x) 1-length(which(hab_buf$CELLCOUNT>=x))/nrow(hab_buf))
overall

per_county <- aggregate(CELLCOUNT ~ NAME, data = hab_buf, function(x) 
  sapply(c(1e3,1e4,1e5,1e6), function(y) 1-length(which(x>=y))/length(x)))
boxplot(per_county[,-1])

per_year <- aggregate(CELLCOUNT ~ year(date), data = hab_buf, function(x) 
  sapply(c(1e3,1e4,1e5,1e6), function(y) 1-length(which(x>=y))/length(x)))
boxplot(per_year[,-1])

per_county_year <- aggregate(CELLCOUNT ~ NAME + year(date), data = hab_buf, function(x) 
  sapply(c(1e3,1e4,1e5,1e6), function(y) 1-length(which(x>=y))/length(x)))
boxplot(per_county_year[,3])
abline(h = c(.9, .95,.99), lty = 5, lwd = 2, lend = 2, col = 'gray30')

boxplot(per_county[,-1],at = seq(1,8,2), xlim = c(0.5,8.5), ylim = c(.4,1), xaxt = 'n', varwidth = T)
boxplot(per_year[,-1],at = seq(2,8,2), add = T, col = 3, xaxt = 'n', varwidth = T)
axis(1, seq(1.5,7.5,2),c(1e3,1e4,1e5,1e6))
points(seq(1.5,7.5,2), overall, pch = 3, cex = 1.5, lwd = 3, col = 2, lend = 2)
abline(h = c(.9, .95), lty = 5, lwd = 2, lend = 2, col = 'gray30')
legend('bottomright', c('by county', 'by year','overall'), col = c(1,1,2),pt.bg=c('gray',3,NA),pch = c(22,22,3))

per_county_x <- aggregate(CELLCOUNT ~ NAME, data = hab_buf, quantile, c(.8,.9,.95,.99), na.rm = T)

aggregate(CELLCOUNT ~ year(date), data = hab_buf, function(x) length(x))

county_aggs <- data.frame(county = per_county$NAME,
           cells_1e5 = 1-per_county$CELLCOUNT[,3]) |>
  arrange(desc(cells_1e5))

aggregate(CELLCOUNT ~ NAME, data = hab_buf, quantile, .95, na.rm = T) |>
  arrange(desc(CELLCOUNT))  

q_thr <- .95
d_thr <- 1e5

m_out <- m_out2 <- m_out3 <- matrix(NA, 23, 24) |>
  as.data.frame()
names(m_out) <- c('county', paste(seq(2000, 2022)))
names(m_out2) <- c('county', paste(seq(2000, 2022)))
names(m_out3) <- c('county', paste(seq(2000, 2022)))
yrs <- data.frame(year = seq(2000, 2022))
# fl_coastco <- unique(fl_gulfco$NAME)
for(i in fl_co_order){
  # i <- fl_coastco[19]
  
  h_temp <- subset(fl_gulfco, fl_gulfco$NAME==i) |>
    st_buffer(dist = 10) |>
    st_join(habs_sf, left = T)
  
  if(any(h_temp$CELLCOUNT>d_thr)){
    t_agg_n <- aggregate(CELLCOUNT ~ year(SAMPLE_DATE) + month(SAMPLE_DATE), h_temp, 
                         quantile, q_thr, na.rm = T) |>
      setNames(c('year','month','cells')) 
    if(any(t_agg_n$cells>d_thr)){
      t_agg_n <- t_agg_n |>
        subset(cells>d_thr) |>
        aggregate(month ~ year, length) |>
        merge(yrs, all = T)
      
      m_out3[which(i==fl_co_order), ] <- c(i, t_agg_n$month)
    }
  }
  
  t_agg_p <- aggregate(CELLCOUNT ~ year(SAMPLE_DATE), h_temp, 
            function(x) length(which(x>d_thr))/length(x)) |>
    setNames(c('year','pro_bl')) |>
    merge(yrs, all = T)
  
  t_agg <- aggregate(CELLCOUNT ~ year(SAMPLE_DATE), h_temp, quantile, q_thr, na.rm = T) |>
    setNames(c('year','celldensity')) |>
    merge(yrs, all = T)
  
  m_out[which(i==fl_co_order), ] <- c(i, t_agg$celldensity)
  m_out2[which(i==fl_co_order), ] <- c(i, t_agg_p$pro_bl)
 
}
m_out <- type.convert(m_out)
m_out2 <- type.convert(m_out2)
m_out3 <- type.convert(m_out3)
m_out3$county <- fl_co_order
m_out3[is.na(m_out3)] <- 0

data.frame(county = m_out$county, 
           q.9 = apply(m_out[,-1],1,quantile, .95, na.rm = T)) |>
  arrange(desc(q.9))

### top 5 counties
# par(mfrow = c(2,2))
plot(2000:2022,m_out[1,-1], ylim = range(m_out[,-1]+1,na.rm = T), typ = 'n',log='y',
     xlab = 'year', ylab = '90%tile Kb denisty (cells/mL)')
for(i in county_aggs$county[1:5]){
  tmp <- subset(m_out, county==i)
  points(2000:2022,tmp[-1]+1,typ = 'o', 
         col = which(i==county_aggs$county[1:5]),
         lwd = 2)
}
abline(h=c(1e5), lty = 5)
legend('bottomleft',county_aggs$county[1:5], lwd = 2, col = 1:5, cex = .7)

plot(2000:2022,m_out2[1,-1], ylim = range(m_out2[,-1],na.rm = T), typ = 'n',
     xlab = 'year', ylab = 'Proportion of samples >1e5 Kb density')
for(i in county_aggs$county[1:5]){
  tmp <- subset(m_out2, county==i)
  points(2000:2022,tmp[-1],typ = 'o', 
         col = which(i==county_aggs$county[1:5]),
         lwd = 2)
}
legend('topright',county_aggs$county[1:5], lwd = 2, col = 1:5, cex = .7)

plot(2000:2022,m_out3[1,-1], ylim = range(m_out3[,-1],na.rm = T), typ = 'n',
     xlab = 'year', ylab = '# months 90%tile of samples >1e5 Kb density')
for(i in county_aggs$county[1:5]){
  tmp <- subset(m_out3, county==i)
  points(2000:2022,tmp[-1],typ = 'o', 
         col = which(i==county_aggs$county[1:5]),
         lwd = 2, pch = which(i==county_aggs$county[1:5]))
}
legend('topright', county_aggs$county[1:5], lwd = 2, col = 1:5, pch = 1:5, cex = .7)

