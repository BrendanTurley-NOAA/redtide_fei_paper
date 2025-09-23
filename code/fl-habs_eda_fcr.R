
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
latbox_s <- 24 ### southern edge of Ket West

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

habs_sf <- st_as_sf(habs, coords = c('LONGITUDE','LATITUDE'), crs = crs(fl_gulfco))

hab_buf <- fl_gulfco |>
  st_buffer(dist = 5*1609) |>
  st_join(habs_sf)

county_aggs <- aggregate(CELLCOUNT ~ NAME , data = hab_buf, quantile, .9, na.rm = T) |>
  arrange(desc(CELLCOUNT))
aggregate(CELLCOUNT ~ NAME , data = hab_buf, median, na.rm = T) |>
  arrange(desc(CELLCOUNT))
aggregate(CELLCOUNT ~ NAME , data = hab_buf, sum, na.rm = T) |>
  arrange(desc(CELLCOUNT))

m_out <- matrix(NA, 23, 24) |>
  as.data.frame()
names(m_out) <- c('county', paste(seq(2000, 2022)))
yrs <- data.frame(year = seq(2000, 2022))
fl_coastco <- unique(fl_gulfco$NAME)
for(i in fl_coastco){
  # i <- fl_coastco[1]
  
  h_temp <- subset(fl_gulfco, fl_gulfco$NAME==i) |>
    st_buffer(dist = 5*1609) |>
    st_join(habs_sf, left = T)
  
  t_agg <- aggregate(CELLCOUNT ~ year(SAMPLE_DATE), h_temp, quantile, .9, na.rm = T) |>
    setNames(c('year','celldensity')) |>
    merge(yrs, all = T)
  
  m_out[which(i==fl_coastco), ] <- c(i, t_agg$celldensity)
}
m_out <- type.convert(m_out)

data.frame(county = m_out$county, 
           q.9 = apply(m_out[,-1],1,quantile, .5, na.rm = T)) |>
  arrange(desc(q.9))

plot(2000:2022,m_out[1,-1], ylim = range(m_out[,-1]+1,na.rm = T), typ = 'n',log='y')
for(i in county_aggs$NAME[1:5]){
  tmp <- subset(m_out, county==i)
  points(2000:2022,tmp[-1]+1,typ = 'o', 
         col = which(i==county_aggs$NAME[1:5]),
         lwd = 2)
}
abline(h=c(1e4), lty = 5)
legend('bottomleft',county_aggs$NAME[1:5], lwd = 2, col = 1:5, cex = .5)
