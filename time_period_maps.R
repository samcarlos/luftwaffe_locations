library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(geosphere)
library(countrycode)
library(ggimage)
library(zoo)
library(lubridate)
library(magick)
library(grid)
library( RColorBrewer)
library(gridtext)

load('data/luftwaffe_sizes_locations_2.rdata')

#merge location data with sizes and loss data 
#map_front are geographicial locations where luftwaffe fought

luftwaffe_sizes[is.na(luftwaffe_sizes)] = ''
location_sizes = merge(
  luftwaffe_sizes %>% mutate(date_start = date_start + days(14)) %>% 
    select(group_type, squadron_number, group_number, subgroup, date_start, add,total, add_new, add_maintenance, add_other_units, lost, lost_enemy, lost_accident, lost_maintenance, lost_other_units, total_eom
    ) %>% 
    group_by(group_type, squadron_number, group_number, subgroup, date_start) %>% ungroup()%>% as.data.frame() ,
  luftwaffe_locations %>% subset(disbanded == 0) %>% mutate(date_start = as.Date(date_start)),# %>% select(group_squadron, group_type, squadron_number, group_number, subgroup, date_start,lon, lat, group_squadron), 
  on = c(group_type, squadron_number, group_number, subgroup, date_start)) %>% 
  mutate(approx_size = as.numeric(approx_size))

min_max_lat_lon = location_sizes %>% summarise(min_lat = min(lat), min_lon = min(lon), max_lat = max(lat), max_lon = max(lon)) %>%as.data.frame()
tem_map = ggmap( get_googlemap(
  center = c(lon = 37.6173, lat = 55.7558), #Long/lat of centre
  zoom=3, 
  size=c(600,600),
  maptype='roadmap', #also hybrid/terrain/roadmap
  scale = 2)
)+ scale_y_continuous(limits=c(30.25000, 70.06944))+scale_x_continuous(limits = c(-4.486076, 44.2))

                      
                      
tem_map+ geom_point(data = location_sizes%>% subset(substr(date_start, 9,10) == '15') %>% filter(group_type!='') %>% group_by(date_start,lat, lon, group_type) %>% summarise(approx_size = sum(approx_size, na.rm = TRUE), 
                                                                                                                                                                             lost_enemy = sum(lost_enemy, na.rm = TRUE),
                                                                                                                                                                             lost_accident = sum(lost_accident, na.rm = TRUE),
                                                                                                                                                                             lost_maintenance = sum(lost_maintenance, na.rm = TRUE)
) %>% mutate(lost = lost_enemy + lost_accident+lost_maintenance)%>%
  as.data.frame() %>% subset(group_type == 'Jagdgeschwader')%>%subset(date_start > '1942-06-01' & date_start < '1944-06-01'), aes(x = lon, y = lat, size = approx_size, colour = lost)) + facet_grid(date_start ~ .)




library(rgdal)
library(lubridate)
library(ggplot2)
library(grid)
library(rworldmap)
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
library(dplyr)
library(rnaturalearth)


list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII')[grep('.shp',list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII'))]
my_spdf <- readOGR( 
  dsn= '/users/sweiss/Downloads/EuropeanBorders_WWII/March_31_1942.shp' ,
  verbose=FALSE
)
summary(my_spdf)
germG <- spTransform(my_spdf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(germG, axes=T)

north_africa_me <- c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", 
                     "Israel", "Syria", "Iran", "Iraq", 
                     'Saudi Arabia',
                     "Jordan", "Palestine")

shp_files_1938 = readOGR( 
  dsn= '/users/sweiss/Downloads/1938/cntry1938.shp' ,
  verbose=FALSE
)

locs_to_keep_shp_files_1938 = shp_files_1938[(shp_files_1938$NAME %in% north_africa_me),]


test = ggplot() +
  geom_polygon(data = germG, aes(x = long, y = lat, group = group) ,color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal()
test +     geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal()+
  coord_sf(xlim = c(-5,50), ylim = c(30,70)) + theme_minimal()

library(broom)
spdf_fortified <- tidy(my_spdf)
spdf_fortified = spdf_fortified %>% as.data.frame()
# Plot it
library(ggplot2)
plot_1 = ggplot() +
  geom_polygon(data = spdf_fortified, aes(x = long/100000, y = lat/100000, group = group) ,color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal()  
  geom_point(data = luftwaffe_locations %>% subset(date_start == '1942-01-01'), 
                              aes(x = lon-20, y = lat-20) 
  )


plot_1
library(ggplot2)
library(grid)
#library(rworldmap)
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
library(dplyr)
#> 
north_africa_me <- c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", 
                   "Israel", "Syria", "Iran", "Iraq", 
                   'Saudi Arabia',
                   "Jordan")
north_africa_me_map <- world_map %>% filter(name %in% europeanUnion)
geom_sf(data = north_africa_me_map, fill = 'grey') +
  xlim(-5,50) + theme_minimal()+ylim(30,70) 
library(rnaturalearth)
world_map <- ne_countries(scale = 50, returnclass = 'sf', )
north_africa_me_map <- world_map %>% filter(name %in% europeanUnion)

p <- ggplot() + 
  geom_sf(data = europe_map, fill = 'grey') +
  xlim(-5,50) + theme_minimal()+ylim(30,70) 

p  
