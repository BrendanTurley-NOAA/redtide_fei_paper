
library(fields)
library(lubridate)
library(MASS)
library(ncdf4)
# library(rgdal)
library(terra)
library(scales)
library(sf)


lonbox_e <- -81 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 31 ### northern coast
latbox_s <- 25 ### southern edge of Ket West

### load map
# setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cb_2019_us_county_500k")
# counties <- vect('cb_2019_us_county_500k.shp')
# FL <- subset(counties, counties$STATEFP=='12')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ne_10m_admin_0_countries")
world <- vect('ne_10m_admin_0_countries.shp') |>
  crop(ext(lonbox_w, lonbox_e, latbox_s, latbox_n))

plot(setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

# image(topo_lon, topo_lat, topo)

lons <- topo_lon[which(topo_lon>=lonbox_w & topo_lon<=lonbox_e)]
lats <- topo_lat[which(topo_lat>=latbox_s & topo_lat<=latbox_n)]

wfs <- topo[which(topo_lon>=lonbox_w & topo_lon<=lonbox_e), 
            which(topo_lat>=latbox_s & topo_lat<=latbox_n)]
wfs[which(wfs<(-150) | wfs>(-10))] <- NA

ind <- expand.grid(which(lons>(-82)), which(lats>28))
wfs[ind[,1],ind[,2]] <- NA

image(lons, lats, wfs, asp = 1)              
hist(wfs)
wfs_r <- rast(t(wfs[,ncol(wfs):1]), extent = ext(lonbox_w,
              lonbox_e,
              latbox_s,
              latbox_n))

crs(wfs_r) <- 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'
cell_areas <- cellSize(wfs_r, unit = 'km')
sum(cell_areas[], na.rm = T)

median(wfs,na.rm=T)
mean(wfs,na.rm=T)
as.vector(wfs) |> summary()

world_sf <- st_as_sf(world)
dist <- 170 * 1000
fl_100 <- st_buffer(world_sf, dist)

# plot(st_geometry(world_sf))
plot(world)
plot(st_geometry(fl_100),add=T)
contour(lons, lats, wfs, levels = c(-145), add = T)

