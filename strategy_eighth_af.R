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
library(rgdal)
library(lubridate)
library(ggplot2)
library(grid)
library(rworldmap)
library(sf)

library(rnaturalearth)
library(gganimate)

load('/users/sweiss/src/luftwaffe_locations/data/luftwaffe_sizes_locations.rdata')


##

#list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII')[grep('.shp',list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII'))]
#load_map_data = function(date){
#  my_spdf <- readOGR( 
#    dsn= paste0('/users/sweiss/Downloads/EuropeanBorders_WWII/', date, '.shp'),
#    verbose=FALSE
#  )
#  my_spdf <- spTransform(my_spdf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#  temp_df = broom::tidy(my_spdf) %>% mutate(date = date) %>% as.data.frame()
#  metadata = my_spdf@data %>% mutate(id  = rownames(my_spdf@data))
#  temp_df = merge(temp_df, metadata, on = 'id') %>% mutate(date_start = as.Date(gsub('_','-',date),'%B-%d-%Y'))
#  return(temp_df)
#}

#for maps found here https://web.archive.org/web/20210304022330/https://web.stanford.edu/group/spatialhistory/cgi-bin/site/pub.php?id=51
#all_map_files = list.files('/users/sweiss/Downloads/EuropeanBorders_WWII/')
#all_map_files = all_map_files[grep('.shp',all_map_files)]

#all_map_files = all_map_files[-grep('.xml',all_map_files)]
#all_map_files = gsub('.shp', '', all_map_files)
#european_map_data = lapply(all_map_files[1:88], load_map_data)
#european_map_data = do.call(rbind,european_map_data)
#save(european_map_data, file = '/Users/sweiss/downloads/european_map_data.rdata')
load( '/Users/sweiss/downloads/european_map_data.rdata')

european_map_data = european_map_data %>% mutate(date_start = as.Date(as.yearmon(date_start)) + days(14) )


north_africa_me <- c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", 
                     "Israel", "Syria", "Iran", "Iraq", 
                     'Saudi Arabia',
                     "Jordan", "Palestine")

shp_files_1938 = readOGR( 
  dsn= '/users/sweiss/Downloads/1938/cntry1938.shp' ,
  verbose=FALSE
)

locs_to_keep_shp_files_1938 = shp_files_1938[(shp_files_1938$NAME %in% north_africa_me),]



#merge location data with sizes and loss data 
#map_front are geographicial locations where luftwaffe fought

luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurow'),'lat'] = 49.4230
luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurow'),'lon'] = 26.9871

luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurov'),'lat'] = 49.4230
luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurov'),'lon'] = 26.9871


luftwaffe_locations[which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 52I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0),'lat'] = 51.7682
luftwaffe_locations[which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 52I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0),'lon'] = 21.9557

which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 3 "Udet"I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0)
subset(luftwaffe_locations, location == 'Millerowo', )



location_sizes = merge(
  luftwaffe_sizes %>% mutate(date_start = date_start + days(14)) %>% 
    select( group_type, squadron_number, group_number, subgroup, date_start, add,total, add_new, add_maintenance, add_other_units, lost, lost_enemy, lost_accident, lost_maintenance, lost_other_units, total_eom
    ) %>% 
    group_by( group_type, squadron_number, group_number, subgroup, date_start) %>% 
    summarise(total = sum(as.numeric(total), na.rm = TRUE),
              add = sum(as.numeric(add), na.rm = TRUE), add_new = sum(as.numeric(add_new), na.rm = TRUE),
              add_maintenance = sum(as.numeric(add_maintenance), na.rm = TRUE),
              add_other_units = sum(as.numeric(add_other_units), na.rm = TRUE),
              lost = sum(as.numeric(lost), na.rm = TRUE),
              lost_enemy = sum(as.numeric(lost_enemy), na.rm = TRUE),
              lost_accident = sum(as.numeric(lost_accident), na.rm = TRUE),
              lost_maintenance = sum(as.numeric(lost_maintenance), na.rm = TRUE),
              lost_other_units = sum(as.numeric(lost_other_units), na.rm = TRUE),
              total_eom = sum(as.numeric(total_eom), na.rm = TRUE)
    ) %>% 
    as.data.frame() ,
  luftwaffe_locations %>% subset(disbanded == 0),# %>% select(group_squadron, group_type, squadron_number, group_number, subgroup, date_start,lon, lat, group_squadron), 
  on = c(group_type, squadron_number, group_number, subgroup, date_start), all.y = TRUE) %>% 
  mutate(approx_size = as.numeric(approx_size))


location_sizes = location_sizes %>% mutate(total_w_approx = ifelse(is.na(total), approx_size, total), missing_data = is.na(total)*1 )
location_sizes = location_sizes %>% subset(group_type_graph != '' & group_type_graph != 'Transport')

location_sizes = location_sizes %>% mutate(lat_cut = cut(lat, seq(-100,100,2)), lon_cut = cut(lon, seq(-100,100,2))) %>% 
  mutate(lat_cut = gsub('\\(', '', lat_cut), lon_cut = gsub('\\(', '', lon_cut))

location_sizes[,'lat_cut'] = as.numeric(sapply(strsplit(location_sizes[,'lat_cut'], ','), function(x) x[[1]]))
location_sizes[,'lon_cut'] = as.numeric(sapply(strsplit(location_sizes[,'lon_cut'], ','), function(x) x[[1]]))
location_sizes = location_sizes %>% mutate(lat = lat_cut, lon = lon_cut)

location_sizes  = location_sizes %>% filter(group_type_graph %in% c('Fighter', 'Night Fighter', 'TE_Fighter'))

# group by map_front 
location_sizes_map_front = location_sizes %>% subset(format(date_start, format = "%d") == '15') %>% 
  group_by(date_start, group_type_graph, map_front, missing_data) %>% 
  summarise(total = sum(as.numeric(total), na.rm = TRUE),add = sum(as.numeric(add), na.rm = TRUE), add_new = sum(as.numeric(add_new), na.rm = TRUE),
            add_maintenance = sum(as.numeric(add_maintenance), na.rm = TRUE),
            add_other_units = sum(as.numeric(add_other_units), na.rm = TRUE),
            lost = sum(as.numeric(lost), na.rm = TRUE),
            lost_enemy = sum(as.numeric(lost_enemy), na.rm = TRUE),
            lost_accident = sum(as.numeric(lost_accident), na.rm = TRUE),
            lost_maintenance = sum(as.numeric(lost_maintenance), na.rm = TRUE),
            lost_other_units = sum(as.numeric(lost_other_units), na.rm = TRUE),
            total_eom = sum(as.numeric(total_eom), na.rm = TRUE),
            approx_total = sum(approx_size, na.rm = TRUE),
            lat = mean(lat), 
            lon = mean(lon),
            total_w_approx = sum(total_w_approx, na.rm = TRUE)) %>% as.data.frame()


#group by map_front and get previousmonths data
location_sizes_map_front_prev = location_sizes %>% subset(format(date_start, format = "%d") == '15')  %>% 
  group_by(group_squadron) %>% 
  arrange(date_start) %>% 
  mutate(prev_map_front = lag(map_front)) %>% 
  as.data.frame() %>% 
  group_by(date_start, group_type_graph, map_front, missing_data,prev_map_front) %>% 
  summarise(total = sum(as.numeric(total), na.rm = TRUE),add = sum(as.numeric(add), na.rm = TRUE), add_new = sum(as.numeric(add_new), na.rm = TRUE),
            add_maintenance = sum(as.numeric(add_maintenance), na.rm = TRUE),
            add_other_units = sum(as.numeric(add_other_units), na.rm = TRUE),
            lost = sum(as.numeric(lost), na.rm = TRUE),
            lost_enemy = sum(as.numeric(lost_enemy), na.rm = TRUE),
            lost_accident = sum(as.numeric(lost_accident), na.rm = TRUE),
            lost_maintenance = sum(as.numeric(lost_maintenance), na.rm = TRUE),
            lost_other_units = sum(as.numeric(lost_other_units), na.rm = TRUE),
            total_eom = sum(as.numeric(total_eom), na.rm = TRUE),
            approx_total = sum(approx_size, na.rm = TRUE),
            lat = mean(lat), 
            lon = mean(lon),
            total_w_approx = sum(total_w_approx, na.rm = TRUE)) %>% as.data.frame()


#calcluate order of map_fronts and average lat / lon by map_front
avg_locs_map_front = location_sizes_map_front %>% group_by(map_front) %>% summarise(lat = mean(lat), lon = mean(lon)) %>% 
  as.data.frame()

distance_to_e_germany = apply(avg_locs_map_front[,c('lat','lon')], 1, function(x){ distm(x, c(52.51521, 11.820705), fun = distHaversine )/1000} )

avg_locs_map_front[,'distance_to_e_germany'] = distance_to_e_germany
western_fronts = c('Austria', 'Denmark', 'northern_france', 'southern_france','northern_italy','southern_italy','Tunisia','western_germany','southern_germany','Greece', "Morocco",
                   "Egypt", "Libya","Norway",'low_countries', "France", 'denmark_norway_arctic')
avg_locs_map_front[,'eastern_front'] = 'East'
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] %in% western_fronts),'eastern_front'] = 'West'

avg_locs_map_front = avg_locs_map_front%>% mutate(distance_to_e_germany_plot = distance_to_e_germany - 2*(eastern_front == 'West')*distance_to_e_germany )  
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'eastern_germany'),'eastern_front'] = 'West'

map_front_order = c("Egypt", "Libya","Greece", "Tunisia", "southern_italy", "northern_italy",
                    "France", "low_countries", "denmark_norway_arctic",  "southern_germany", 
                    "western_germany", "eastern_germany", "hungary_czech", "prussia_poland", "Yugoslavia", 
                    "Baltics", "bulgaria_romania", "western_ukraine", "Belarus", "north_russia",       
                    "central_russia", "eastern_ukraine", "southern_russia"  )
map_front_names = c("Egypt", "Libya","Greece", "Tunisia", "S Italy", "N Italy",
                    "France", "Benelux", "North",  "S Germ", 
                    "W Germ", "E Germ", "Hungary / Czech", "Prussia / Poland", "Yugoslavia", 
                    "Baltics", "Bulgaria / Romania", "W Ukraine", "Belarus", "N Russia",       
                    "C Russia", "E Ukraine", "S Russia"  )
map_front_names_df = data.frame(  map_front_names, map_front_order)

avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'France'),c('lon','lat')] = c(-2,45)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'low_countries'),c('lon','lat')] = c(2.5,49.3904)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'western_germany'),c('lon')] = c(6.5)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'southern_germany'),c('lon', 'lat')] = c(9.0, 47)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'eastern_germany'),c('lon', 'lat')] = c(13, 51.5)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'northern_italy'),c('lon')] = c(9)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'denmark_norway_arctic'),c('lon', 'lat')] = c(5.733, 58.9700)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'prussia_poland'),c('lon')] = c(18)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'Baltics'),c('lon')] = c(22)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'bulgaria_romania'),c('lon')] = c(22)
avg_locs_map_front[which(avg_locs_map_front[,'map_front'] == 'Tunisia'),c('lon')] = c(8)

location_sizes_map_front = merge(location_sizes_map_front %>% mutate(map_front_lat = lat, map_front_lon = lon) %>% select(-lat,-lon), avg_locs_map_front, by = 'map_front')

location_sizes_map_front_prev = location_sizes %>% subset(format(date_start, format = "%d") == '15')  %>% 
  group_by(group_squadron) %>% 
  arrange(date_start) %>% 
  mutate(prev_map_front = lag(map_front), prev_lat = lag(lat), prev_lon = lag(lon)) %>% 
  as.data.frame() %>% 
  group_by(date_start, group_type_graph, map_front, missing_data,prev_map_front, prev_lat, prev_lon, lat, lon) %>% 
  summarise(total = sum(as.numeric(total), na.rm = TRUE),add = sum(as.numeric(add), na.rm = TRUE), add_new = sum(as.numeric(add_new), na.rm = TRUE),
            add_maintenance = sum(as.numeric(add_maintenance), na.rm = TRUE),
            add_other_units = sum(as.numeric(add_other_units), na.rm = TRUE),
            lost = sum(as.numeric(lost), na.rm = TRUE),
            lost_enemy = sum(as.numeric(lost_enemy), na.rm = TRUE),
            lost_accident = sum(as.numeric(lost_accident), na.rm = TRUE),
            lost_maintenance = sum(as.numeric(lost_maintenance), na.rm = TRUE),
            lost_other_units = sum(as.numeric(lost_other_units), na.rm = TRUE),
            total_eom = sum(as.numeric(total_eom), na.rm = TRUE),
            approx_total = sum(approx_size, na.rm = TRUE),
            lat = mean(lat), 
            lon = mean(lon),
            total_w_approx = sum(total_w_approx, na.rm = TRUE)) %>% as.data.frame()

missions_lat_long_agg %>% subset(Date >= '1943-10-01' & Date < '1943-11-01') %>% 
  group_by(target_type_2, lon, lat, A.C.Type) %>% summarise(Dispatched = sum(Dispatched)) %>% head()

location_sizes_map_front_prev = location_sizes_map_front_prev %>% mutate(plot_lost = lost_enemy + lost_accident )
location_sizes_map_front_prev_1 = location_sizes_map_front_prev %>% select(prev_map_front, map_front, group_type_graph, prev_lat, 
                                                                           prev_lon, lat, lon, total_w_approx, date_start, plot_lost)# %>%  subset(date_start > '1940-04-01' & date_start < '1941-10-01')

missions_lat_long_agg = missions_lat_long_agg %>% mutate(lat_cut = cut(lat, seq(-100,100,2)), lon_cut = cut(lon, seq(-100,100,2))) %>% 
  mutate(lat_cut = gsub('\\(', '', lat_cut), lon_cut = gsub('\\(', '', lon_cut))

missions_lat_long_agg[,'lat_cut'] = as.numeric(sapply(strsplit(missions_lat_long_agg[,'lat_cut'], ','), function(x) x[[1]]))
missions_lat_long_agg[,'lon_cut'] = as.numeric(sapply(strsplit(missions_lat_long_agg[,'lon_cut'], ','), function(x) x[[1]]))

missions_lat_long_agg_1 = missions_lat_long_agg %>%mutate(date_start = as.Date(as.yearmon(Date)) + days(14))  %>% 
  mutate(lat = lat_cut, lon = lon_cut) %>% 
  group_by(target_type_2, lon, lat, A.C.Type,date_start) %>%  summarise(plot_lost = sum(Total.Lost)) %>% mutate(group_type_graph = A.C.Type) %>%
  select(-A.C.Type) %>%
  as.data.frame()
missions_lat_long_agg_1_uncounted = missions_lat_long_agg_1 %>% uncount(plot_lost) %>% mutate(af = 'us')
location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_1 %>% uncount(plot_lost) %>% mutate(af = 'luft')
location_sizes_map_front_prev_uncounted = gtools::smartbind(location_sizes_map_front_prev_uncounted, missions_lat_long_agg_1_uncounted)
# 
# location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_1 %>% uncount(plot_lost)
# location_sizes_map_front_prev_uncounted_lost[,'lon_to_go'] = as.numeric(location_sizes_map_front_prev_uncounted_lost[,'date_start']) 
# 
# location_sizes_map_front_prev_uncounted_lost[,'eastern_front'] = 'East'
# location_sizes_map_front_prev_uncounted_lost[which(location_sizes_map_front_prev_uncounted_lost[,'map_front'] %in% western_fronts),'eastern_front'] = 'West'

xmin = -5
xmax = 47
ymin = 30 - 3307/ 300
ymax = 70

# location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% 
#   mutate(lon_to_go = ((lon_to_go - as.numeric(as.Date('1939-09-15') )))) %>% 
#   mutate(lon_to_go = lon_to_go/(as.numeric(as.Date('1945-05-15') )-as.numeric(as.Date('1939-09-15') ) ) )
# #location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = lon_to_go - min(lon_to_go)  ) %>% mutate(lon_to_go = lon_to_go/max(lon_to_go) )
# 
# 
# 
# location_sizes_map_front_prev_uncounted_lost= location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = lon_to_go * (xmax - xmin) + xmin)
# #location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% group_by( date_start, eastern_front) %>% mutate(plocation_order =  1:n()) %>%
# #  mutate(new_lat = plocation_order/300) %>% 
# #  #mutate(new_lat = ymin + new_lat ) %>% as.data.frame()
# #  mutate(new_lat = ymin + new_lat*(eastern_front == 'East') + (1915/300) - (eastern_front != 'East')*new_lat  ) %>% as.data.frame()
# 
# location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% group_by( date_start) %>% mutate(plocation_order =  1:n()) %>%
#   mutate(new_lat = plocation_order/300) %>% 
#   mutate(new_lat = ymin + new_lat ) %>% as.data.frame()
# 
# 
# 
# location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(plocation_order = paste0(date_start, plocation_order, group_type_graph))
# lost_to_maps = rbind(location_sizes_map_front_prev_uncounted_lost %>% mutate(lat = new_lat, lon = lon_to_go, date_start = date_start + months(1) - days(20)) %>% select(lat, lon, plocation_order, date_start, group_type_graph),
#                      location_sizes_map_front_prev_uncounted_lost %>% mutate(lat = new_lat, lon = lon_to_go, date_start = date_start + months(1) - days(10)) %>% select(lat, lon, plocation_order, date_start, group_type_graph),
#                      location_sizes_map_front_prev_uncounted_lost %>% select(lat, lon, plocation_order, date_start, group_type_graph)
# ) %>% mutate(location_order = plocation_order) %>% mutate(new_lat = lat, new_lon = lon)


location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_uncounted %>% mutate(date_end = date_start ) %>% 
  mutate(plocation_order = 1:n())

# to_maps = rbind(location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon) %>% 
#                   select(lat, lon, plocation_order, date_start, group_type_graph), 
#                 
#                 location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon,
#                                                                    date_start = date_start + months(1) - days(20)) %>% 
#                   select(lat, lon, plocation_order, date_start, group_type_graph),
#                 
#                 location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon,
#                                                                    date_start = date_start + months(1) - days(10)) %>% 
#                   select(lat, lon, plocation_order, date_start, group_type_graph),
#                 
#                 location_sizes_map_front_prev_uncounted %>% mutate(date_start = date_end)  %>% select(lat, lon, plocation_order, date_start,group_type_graph)
# ) 
to_maps = location_sizes_map_front_prev_uncounted
to_maps = to_maps %>% mutate(fighters = (group_type_graph == 'Fighter') + (group_type_graph == 'Night Fighter') + (group_type_graph == 'TE_Fighter'))

#to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
#  group_by(date_start,lat, lon) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
#  mutate(location_order = 1:n() / 50) %>% 
#  mutate(new_lat = location_order  + lat ,
#         new_lon =  lon) %>%
#  as.data.frame()

# to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
#   group_by(date_start,lat, lon,fighters) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
#   mutate(location_order = 1:n()) %>% 
#   mutate(new_lat = (lat + ((location_order) %% 100 )/50)*fighters  +( (lat - ((location_order) %% 100 )/50) + 2)*(1-fighters)  ,
#          new_lon =  (lon + floor(location_order/100) / 5+.1) *fighters + ((lon - floor(location_order/100) / 5 + 2 - .1))*(1-fighters) #,Plane_Type = group_type_graph
#   ) %>%
#   as.data.frame()
# to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
#   group_by(date_start,lat, lon,fighters) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
#   mutate(location_order = 1:n()) %>% 
#   mutate(new_lat = (lat + ((location_order) %% 50 )/50)  ,
#          new_lon =  (lon + floor(location_order/50) / 5+.1)  #,Plane_Type = group_type_graph
#   ) %>%
#   as.data.frame()

to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
  mutate(af = (af == 'luft')*1) %>%
  group_by(date_start,lat, lon,group_type_graph, af) %>% arrange(lat, lon,group_type_graph,date_start, af) %>% 
  mutate(location_order = 1:n()) %>% 
  mutate(new_lat = (lat + ((location_order) %% 100 )/50)*af  +( (lat - ((location_order) %% 100 )/50) + 2)*(1-af)  ,
         new_lon =  (lon + floor(location_order/100) / 5+.1) *af + ((lon - floor(location_order/100) / 5 + 2 - .1))*(1-af) #,Plane_Type = group_type_graph
  ) %>%
  as.data.frame()

START_DATE = '1943-06-01'
END_DATE = '1944-08-05'
# ggplot()+ 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + 
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,
#                color = "black", size = 0.1, fill = "white", linetype = 2) +
#   coord_equal() +
#   geom_polygon(data = european_map_data %>% subset(date_start >= START_DATE & date_start <END_DATE),
#                aes(x = long, y = lat, group = paste0(group, date_start)) ,
#                color = "black", size = 0.1,linetype = 2, fill = "white"
#   ) +
#   annotate("rect", xmin = xmin, xmax = xmax, ymin = 00, ymax = 29,
#           alpha =1,fill = "white")+
#   geom_vline(xintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
#   geom_hline(yintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
#   geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=30), fill="white", alpha=1) +
#   coord_sf(xlim = c(-5,20), ylim = c(40,60))+
#   geom_point(data = to_maps %>% filter(!is.na(date_start)) %>%
#                subset(date_start >= START_DATE & date_start <END_DATE)
#              , aes(x = new_lon, y = new_lat, colour = as.factor(af)), size = .1 ) + 
#   scale_size(range = c(0,.01),guide = 'none') + facet_wrap(date_start~.  )

monthly_lost_dispacted_by_ac = missions_lat_long_agg %>% filter( (A.C.Type %in% c('B-17', 'B-24', 'P-51','P-47','P-38'))) %>% 
  mutate(yearmon = as.yearmon(Date)) %>% 
  group_by(yearmon, A.C.Type) %>%
  summarise(dispatched = sum(Dispatched), recalled = sum(Recalled+RTB), lost = sum(Total.Lost)) %>%
  mutate(dispatched_2 = dispatched - recalled) %>%
  mutate(lost_ratio = lost  / dispatched_2*100) %>%
  as.data.frame()

monthly_8th_af_losses = ggplot() + 
  
  geom_bar(data = monthly_lost_dispacted_by_ac %>% subset(yearmon> '1943-05-01' & yearmon < '1944-09-01'),
           aes(x = as.Date(yearmon), y = lost, fill = A.C.Type)
           , position = 'stack',stat = 'identity')+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) + 
  xlab('Date (Month)') + ylab('Number of Losses') + 
  ggtitle('Monthly USAAF 8th Air Force Losses by Aircraft Type')



monthly_8th_af_losses
timeline_df = data.frame( 
  start_date = as.Date(c('1944-01-21', '1944-02-20','1944-03-05', '1943-06-14', '1944-06-06', '1943-10-14','1943-08-17','1943-12-05')),
  end_date = as.Date(c('1944-01-21', '1944-02-25','1944-03-08','1944-04-19', '1944-06-06','1943-10-14','1943-08-17','1943-12-05')),
  event = c('First Duty Change','Operation Argument','Battle of Berlin','Operation Pointblank','D-Day','2nd Schweinfurt','1st Scwheinfurt', 'First Mustang Escort'),
  position = c(-.5,-1,-1,-2,-.5,-1,-1, 
               -.5)*10,
  text_position = -c(.55,1.05,1.05,2.05,.95,1.05,1.05,.55)
)
timeline_df = timeline_df   %>% subset(event != 'First Mustang Escort')
monthly_8th_af_losses = monthly_8th_af_losses + geom_segment(data = timeline_df, aes(x = as.Date(start_date), xend = as.Date(end_date), y = position, yend = position)) + 
  geom_label_repel(data = timeline_df, aes(x = as.Date(start_date), y = position, label = event), size= 2.5, min.segment.length = 0)

ggsave(monthly_8th_af_losses, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/monthly_8th_af_losses.png', dpi = 600)

monthly_lost_dispacted_by_ac_melt= melt(monthly_lost_dispacted_by_ac, id.vars= c('yearmon', 'A.C.Type'))

heavy_dispatched_loss_by_month_graph = subset(monthly_lost_dispacted_by_ac_melt, as.Date(yearmon) < as.Date('1944-08-01') & as.Date(yearmon) > as.Date('1942-12-31')) %>%
  filter(variable %in% c('dispatched_2', 'lost','lost_ratio')) %>% 
  ggplot( aes(x = as.Date(yearmon), y= value))+
  geom_bar(stat = 'identity')+
  facet_grid(variable~., scales = 'free', labeller = labeller(variable = new_labels_heavy_month))+ylab('Count')+xlab('Date (Month)')+theme_bw()+
  theme(text = element_text(size=20))+
  ggtitle('Heavy Bomber Dispatched and Lossed by Month')


#

names(monthly_lost_dispacted_by_ac_melt) <- c("lost",'dispatched_2','lost_ratio')
new_labels_heavy_month <- c("Total Lost",'Dispatched','Lost %', 'Timeline')
names(new_labels_heavy_month) <- c("lost",'dispatched_2','lost_ratio', 'Timeline')

dispatched_launched_by_plane = subset(monthly_lost_dispacted_by_ac_melt, as.Date(yearmon) < as.Date('1944-08-01') & as.Date(yearmon) > as.Date('1943-06-01')) %>%
  filter(variable %in% c('dispatched_2', 'lost','lost_ratio')) %>% 
  ggplot( aes(x = as.Date(yearmon), y = value, fill = A.C.Type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_grid(variable~., scales = 'free', labeller = labeller(variable = new_labels_heavy_month))+ylab('Count')+xlab('Date (Month)')+theme_bw()+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) + 
  ggtitle('8th AirFroce Planes Dispatched and Lossed by Month')

dispatched_launched_by_plane = ggplot() + 
  geom_bar(data = subset(monthly_lost_dispacted_by_ac_melt, as.Date(yearmon) < as.Date('1944-08-01') & as.Date(yearmon) > as.Date('1943-06-01')) %>%
             filter(variable %in% c('dispatched_2', 'lost','lost_ratio')), 
           aes(x = as.Date(yearmon), y = value, fill = A.C.Type),
           stat = 'identity', position = 'dodge')+
 geom_segment(data = timeline_df %>% mutate(position = position / 10 + 10) %>% mutate(variable = ('Timeline')), aes(x = as.Date(start_date), xend = as.Date(end_date), y = position, yend = position)) + 
  geom_label_repel(data = timeline_df %>% mutate(position = position / 10 + 10) %>% mutate(variable = ('Timeline')), aes(x = as.Date(start_date), y = position, label = event), size= 3, min.segment.length = 0)+ 
  
  facet_grid(variable~., scales = 'free', labeller = labeller(variable = c(new_labels_heavy_month)))+ylab('Count')+xlab('Date (Month)')+theme_bw()+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) + 
  ggtitle('8th AirFroce Planes Dispatched and Lossed by Month')

dispatched_launched_by_plane 


#
ggsave(dispatched_launched_by_plane, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/dispatched_losses_by_plane_8th.png', dpi = 600)


missions_lat_long_agg %>% filter(A.C.Type %in% c('B-17', 'B-24')) %>%
  group_by(target_type_2) %>% summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame() %>% arrange(-Dispatched)

missions_lat_long_agg[,'target_type_3'] = missions_lat_long_agg[,'target_type_2']

missions_lat_long_agg[which(missions_lat_long_agg[,'target_type_2'] %in% c('Airfield', 'Air Depot')),'target_type_3'] = 'Airfield / Depot'
missions_lat_long_agg[which(missions_lat_long_agg[,'target_type_2'] %in% c('Transportation', 'Communications')),'target_type_3'] = 'Transport / Communication'
missions_lat_long_agg[which(missions_lat_long_agg[,'target_type_2'] %in% c('Aircraft')),'target_type_3'] = 'Aircraft Industry'
missions_lat_long_agg[which(missions_lat_long_agg[,'target_type_2'] %in% c('Leaflets')),'target_type_3'] = 'Leaflet'

target_type_3_to_display = missions_lat_long_agg %>% filter(A.C.Type %in% c('B-17', 'B-24')) %>%
  group_by(target_type_3) %>% summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame() %>% arrange(-Dispatched) %>% head(10)
target_type_3_to_display = c('Uboats','Airfield / Depot','Transport / Communication','Tactical','Industry','Aircraft Industry', 'V-Weapons','Oil','City')
missions_lat_long_agg[which(!missions_lat_long_agg[,'target_type_3'] %in% target_type_3_to_display),'target_type_3'] = 'Other'
target_type_3_to_display_order= c('Uboats','V-Weapons','Airfield / Depot','Transport / Communication','Tactical',
                                  'Industry','Aircraft Industry', 'Oil','City', 'Other')

missions_lat_long_agg[,'target_type_3'] = factor(missions_lat_long_agg[,'target_type_3'], levels = target_type_3_to_display_order)

START_DATE = '1943-06-01'
END_DATE = '1944-01-05'
START_DATE = '1943-06-01'
END_DATE = '194-09-05'

ggplot() + 
  geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,
               color = "black", size = 0.1, fill = "white", linetype = 2) +
  coord_equal() +
  geom_polygon(data = european_map_data %>% subset(date_start >= START_DATE & date_start <END_DATE),
               aes(x = long, y = lat, group = paste0(group, date_start)) ,
               color = "black", size = 0.1,linetype = 2, fill = "white"
  ) +
  coord_sf(xlim = c(-5,20), ylim = c(40,60))+
  geom_point(data = to_maps %>% filter(!is.na(date_start)) %>%#subset(af == 0) %>%
               subset(date_start >= START_DATE & date_start <END_DATE)
             , aes(x = new_lon, y = new_lat, colour = as.factor(af)), size = .1 ) + 
  scale_color_manual(values = c("1" = "Blue",
                                "0"="Red"))+ 
  scale_size(range = c(0,.01),guide = 'none') +
  geom_point(data = missions_lat_long_agg %>%
               subset(target_type_2 != 'Escort')%>%
                    mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
                    subset(date_start >= START_DATE & date_start <END_DATE) %>%
                 group_by(target_type_3, lon, lat,date_start) %>% 
               summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame(),  aes(x = lon, y = lat, shape = target_type_3, size = Dispatched, color = )
                 )+  scale_size(range = c(0,5),guide = 'none') + facet_wrap(date_start~.  )+
      # geom_text_repel(data = missions_lat_long_agg %>%
      #        subset(target_type_2 != 'Escort')%>%
      #             mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
      #             subset(date_start >= START_DATE & date_start < END_DATE) %>%
      #          group_by(target_type_3, lon, lat,date_start) %>% summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame(), 
      #        aes(x = lon, y = lat, label = target_type_3, size = Dispatched)
      #          )+  scale_size(range = c(0,5),guide = 'none') +

  geom_vline(xintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
  geom_hline(yintercept = seq(-100,100,2), linetype = 1, alpha = .25) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = 'bottom')
  

ggplot() + 
  geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,
               color = "black", size = 0.1, fill = "white", linetype = 2) +
  coord_equal() +
  geom_polygon(data = european_map_data %>% subset(date_start >= START_DATE & date_start <END_DATE),
               aes(x = long, y = lat, group = paste0(group, date_start)) ,
               color = "black", size = 0.1,linetype = 2, fill = "white"
  ) +
  coord_sf(xlim = c(-5,20), ylim = c(40,60))+
  geom_point(data = to_maps %>% filter(!is.na(date_start)) %>%subset(af == 0) %>%
               subset(date_start >= START_DATE & date_start <END_DATE)
             , aes(x = new_lon, y = new_lat, colour = as.factor(af)), size = .1 ) + 
  scale_color_manual(values = c("1" = "Blue",
                                "0"="Red"))+ 
  scale_size(range = c(0,.01),guide = 'none') +
  geom_point(data = missions_lat_long_agg %>%
               subset(target_type_2 != 'Escort')%>%
               mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
               subset(date_start >= START_DATE & date_start <END_DATE) %>%
               group_by(target_type_3, lon, lat,date_start) %>% 
               summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame(),  aes(x = lon, y = lat, shape = target_type_3, size = Dispatched )
  )+  scale_size(range = c(0,5),guide = 'none') +
  # geom_text_repel(data = missions_lat_long_agg %>%
  #        subset(target_type_2 != 'Escort')%>%
  #             mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
  #             subset(date_start >= START_DATE & date_start < END_DATE) %>%
  #          group_by(target_type_3, lon, lat,date_start) %>% summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame(), 
  #        aes(x = lon, y = lat, label = target_type_3, size = Dispatched)
  #          )+  scale_size(range = c(0,5),guide = 'none') +
  
  geom_vline(xintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
  geom_hline(yintercept = seq(-100,100,2), linetype = 1, alpha = .25) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = 'bottom')

  
  

ggplot() + 
  geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,
               color = "black", size = 0.1, fill = "white", linetype = 2) +
  coord_equal() +
  geom_polygon(data = european_map_data %>% subset(date_start >= START_DATE & date_start <END_DATE),
               aes(x = long, y = lat, group = paste0(group, date_start)) ,
               color = "black", size = 0.1,linetype = 2, fill = "white"
  ) +
  coord_sf(xlim = c(-5,20), ylim = c(40,60))+
  # geom_point(data = to_maps %>% filter(!is.na(date_start)) %>%subset(af == 0) %>%
  #              subset(date_start >= START_DATE & date_start <END_DATE)
  #            , aes(x = new_lon, y = new_lat, colour = as.factor(af)), size = .1 ) + 
  # scale_color_manual(values = c("1" = "Blue",
  #                               "0"="Red"))+ 
  scale_size(range = c(0,.01),guide = 'none') + 
  geom_point(data = missions_lat_long_agg %>%
               subset(target_type_2 != 'Escort')%>%
               mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
               subset(date_start >= START_DATE & date_start <END_DATE) %>%
               group_by(target_type_3, lon, lat,date_start) %>% 
               summarise(Dispatched = (sum(Dispatched)), lost = sum(Total.Lost)) %>% 
               mutate(lost_ratio = (lost / Dispatched)) %>% 
               subset(lost_ratio <1) %>% 
               
               as.data.frame() %>% 
               mutate(lost_breaks = cut(lost,breaks = c(-1,10,20,30,40,60,130))),
             aes(x = lon, y = lat, size = Dispatched, colour = lost_ratio, alpha = .5), shape = 10
  )+  scale_size(range = c(0,5),guide = 'none') + scale_colour_gradient(low = 'blue', high = 'red') + 
  facet_grid(date_start~target_type_3)+
  # geom_text_repel(data = missions_lat_long_agg %>%
  #        subset(target_type_2 != 'Escort')%>%
  #             mutate(date_start = as.Date(as.yearmon(Date)) + days(14)) %>%
  #             subset(date_start >= START_DATE & date_start < END_DATE) %>%
  #          group_by(target_type_3, lon, lat,date_start) %>% summarise(Dispatched = (sum(Dispatched))) %>% as.data.frame(), 
  #        aes(x = lon, y = lat, label = target_type_3, size = Dispatched)
  #          )+  scale_size(range = c(0,5),guide = 'none') +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = 'bottom')




START_DATE = '1943-06-01'
END_DATE = '1944-01-01'
#

map = ggplot()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,
  #              color = "black", size = 0.1, fill = "white", linetype = 2) +
  # coord_equal() +
  geom_polygon(data = european_map_data %>% subset(date_start == '1943-06-15'),
               aes(x = long, y = lat, group = paste0(group, date_start)) ,
               color = "black", size = 0.1,linetype = 2, fill = "white"
  ) +
  annotate("rect", xmin = xmin, xmax = xmax, ymin = 00, ymax = 29,
          alpha =1,fill = "white")+
  geom_vline(xintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
  geom_hline(yintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=30), fill="white", alpha=1) +
  coord_sf(xlim = c(-5,20), ylim = c(40,60))
  

map + geom_point(data = to_maps %>% filter(!is.na(date_start)) %>%
             subset(date_start >= START_DATE & date_start <END_DATE) %>% mutate(date = date_start)
           , aes(x = new_lon, y = new_lat, colour = as.factor(af)), size = .1 ) +
  scale_size(range = c(0,.01),guide = 'none') + facet_grid(date_start~.)
  
# 
# geom_text_repel(data = missions_lat_long_agg %>% subset(as.Date(date_start) == '1944-03-15') %>% 
#                group_by(target_type_2, lon, lat) %>% summarise(Dispatched = sum(Dispatched)) %>%
#                subset(target_type_2 != 'Escort'),  aes(x = lon, y = lat, label = target_type_2, size = Dispatched), max.overlaps = 500
#                )+
  # geom_point(data = missions_lat_long_agg %>% subset(Date >= START_DATE & Date <END_DATE) %>% 
  #                   group_by(target_type_2, lon, lat, ) %>% summarise(Dispatched = sum(Dispatched)) %>%
  #                   subset(target_type_2 != 'Escort')%>% as.data.frame() %>% mutate(date_start = date),  aes(x = lon, y = lat, shape = target_type_2, size = Dispatched)
  # )+
  # geom_point(data = missions_lat_long_agg %>% subset(Date >= '1943-10-01' & Date < '1943-11-01') %>% 
  #              group_by(target_type_2, lon, lat) %>% summarise(Total.Lost = sum(Total.Lost)) %>%
  #              subset(target_type_2 != 'Escort'),  aes(x = lon, y = lat, size = Total.Lost ,colour = Total.Lost)
  # )+
  scale_size(range = c(0,10),guide = 'none') + facet_wrap(as.factor(date_start)~.)
  



to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
  group_by(date_start,lat, lon,fighters) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
  mutate(location_order = 1:n()) %>% 
  mutate(new_lat = (lat + ((location_order) %% 100 )/50)*fighters  +( (lat - ((location_order) %% 100 )/50) + 2)*(1-fighters)  ,
         new_lon =  (lon + floor(location_order/100) / 5+.1) *fighters + ((lon - floor(location_order/100) / 5 + 2 - .1))*(1-fighters) #,Plane_Type = group_type_graph
  ) %>%
  as.data.frame()



