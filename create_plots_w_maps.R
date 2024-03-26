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
library(sf)

load('data/luftwaffe_sizes_locations_2.rdata')

#merge location data with sizes and loss data 
#map_front are geographicial locations where luftwaffe fought
load( '/users/sweiss/src/luftwaffe_locations/data/country_1939_lat_lon.rdata')

luftwaffe_locations = merge(luftwaffe_locations, new_country_abrev_1 %>% mutate(new_map_front = name), by = c('lat', 'lon'), how = 'outer')


# Create an sf object for the point
shp_files_1938 <- st_read('/users/sweiss/Downloads/1938/cntry1938.shp')
shp_files_1938 <- st_set_crs(shp_files_1938, 4326)

unique_locs = luftwaffe_locations%>% subset(!is.na(lat)) %>% select(map_front, lat, lon) %>% unique()
unique_locs = unique_locs %>% subset(map_front !='Spain')
unique_locs_w_germany = unique_locs %>% subset(map_front == 'western_germany')



ggplot() +
  geom_sf(data = shp_files_1938 %>% filter(NAME %in% c(unique(luftwaffe_locations[,'name']), 'Sweden'))) +
  geom_point(data = unique_locs , aes(x = lon, y = lat), size = 1, shape = 4) + 
  #geom_sf(data = unique_locs , aes(size = 1, shape = 4)) + 
  xlim(xmin = -10, xmax = 40)  + 
  ylim(ymin = 30, ymax = 70)  + 
  ggtitle("Map of Selected Countries by Name") +
 # coord_sf(crs = "+proj=robin")+
  theme_minimal() + theme(legend.position = '')

##
library(sf)
library(ggplot2)

# Create the plot
map <- ggplot() +
  geom_sf(data = shp_files_1938 %>% filter(NAME %in% c(unique(luftwaffe_locations[,'name']), 'Sweden')), fill = "lightblue", color = "white") +
  geom_point(data = unique_locs , aes(x = lon, y = lat), alpha = .5, size = .5, shape = 4, colour = 'red') + 
  xlim(xmin = -10, xmax = 40)  + 
  ylim(ymin = 30, ymax = 70)  + 
  theme_minimal() +  theme(legend.position = '') + theme_void() 

# Save the plot, adjust 'width' and 'height' based on your calculated dimensions
ggsave(map, file = "/users/sweiss/downloads/map_plot.png", width = 8, height = 8, units = "in")

##
map_front_order = c("Egypt", "Libya","Greece", "Tunisia", "southern_italy", "northern_italy",
                    "France", "low_countries", "denmark_norway_arctic",  "southern_germany", 
                    "western_germany", "eastern_germany", "hungary_czech", "prussia_poland", "Yugoslavia", 
                    "Baltics", "bulgaria_romania", "western_ukraine", "Belarus", "north_russia",       
                    "central_russia", "eastern_ukraine", "southern_russia"  )


get_map_front_map = function(temp_map_front){
  unique_locs_w_germany = unique_locs %>% subset(map_front == temp_map_front)

  
  
  temp_map = ggplot() +
    geom_sf(data = shp_files_1938 %>% filter(NAME %in% unique(luftwaffe_locations[,'name']))) +
    geom_point(data = unique_locs_w_germany, aes(x = lon, y = lat), size = 1, shape = 4) + 
    #facet_wrap(map_front~.)+
    xlim(xmin = min(unique_locs_w_germany[,'lon']), xmax = max(unique_locs_w_germany[,'lon']))  + 
    ylim(ymin = min(unique_locs_w_germany[,'lat']), ymax = max(unique_locs_w_germany[,'lat']))  + 
    theme_minimal() + theme(legend.position = '') + theme_void() 
  
  return(temp_map)
}

unique_locs %>% group_by(map_front) %>% summarise(min_lon = min(lon), max_lon = max(lon),
                                              min_lat = min(lat), max_lat = max(lat)) %>% 
  as.data.frame()

maps = lapply(map_front_order, get_map_front_map)

  

location_sizes = merge(
  luftwaffe_sizes %>% mutate(date_start = date_start + days(14)) %>% 
    select(group_type, squadron_number, group_number, subgroup, date_start, add,total, add_new, add_maintenance, add_other_units, lost, lost_enemy, lost_accident, lost_maintenance, lost_other_units, total_eom
    ) %>% 
    group_by(group_type, squadron_number, group_number, subgroup, date_start) %>% 
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

location_sizes = location_sizes
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

location_sizes_map_front = merge(location_sizes_map_front %>% select(-lat,-lon), avg_locs_map_front, by = 'map_front')
location_sizes_map_front_prev = merge(location_sizes_map_front_prev %>% select(-lat,-lon), 
                                      avg_locs_map_front %>% select(map_front, distance_to_e_germany_plot) , 
                                      by = 'map_front')
location_sizes_map_front_prev = merge(location_sizes_map_front_prev, 
                                      avg_locs_map_front %>% select(map_front, distance_to_e_germany_plot,lat , lon) %>%
                                        mutate(distance_to_e_germany_plot_prev = distance_to_e_germany_plot, 
                                               prev_map_front = map_front, 
                                               prev_lat = lat, prev_lon = lon) %>%
                                        select(prev_map_front, distance_to_e_germany_plot_prev, prev_lat, prev_lon), by = 'prev_map_front')


location_sizes_map_front_prev[,'map_front'] = factor(location_sizes_map_front_prev[,'map_front'], levels = map_front_order )
location_sizes_map_front[,'map_front'] = factor(location_sizes_map_front[,'map_front'], levels = map_front_order )

real_size_loss_data = location_sizes_map_front %>% subset(group_type_graph != '' & missing_data == 0)

#include all permutations of data in order to maintain consistancy of graphs
unique_values_combinations = expand.grid(
  unique(location_sizes_map_front[,'map_front']),
  unique(location_sizes_map_front[,'date_start']),
  unique(location_sizes_map_front[,'group_type_graph']),
  unique(location_sizes_map_front[,'missing_data']))
colnames(unique_values_combinations) = c('map_front','date_start','group_type_graph','missing_data')

location_sizes_map_front = merge(location_sizes_map_front, unique_values_combinations, on = c('map_front','date_start','group_type_graph','missing_data'), all = TRUE)
location_sizes_map_front[is.na(location_sizes_map_front)] = 0
location_sizes_map_front = subset(location_sizes_map_front, group_type_graph != '')


location_sizes_map_front_2 = location_sizes_map_front %>% 
  group_by(map_front, date_start, group_type_graph) %>% summarise(
    total = sum(total), 
    lost_enemy = sum(lost_enemy),
    lost_accident = sum(lost_accident),  
    lost_maintenance = sum(lost_maintenance),
    total_w_approx = sum(total_w_approx)
    
  ) %>% as.data.frame() %>% mutate(loss_ratio = (lost_enemy  + lost_accident + lost_maintenance) / total_w_approx)

location_sizes_map_front_2 = merge(location_sizes_map_front_2, 
                                   real_size_loss_data %>% subset(group_type_graph != '' & missing_data == 0) %>% mutate(has_actual_data  = 1) %>% select(map_front, date_start, group_type_graph)  %>% mutate(has_actual_data  = 1),
                                   on = c('map_front', 'date_start', 'group_type_graph'), all.x = TRUE
)

location_sizes_map_front_2[,'map_front'] = factor(location_sizes_map_front_2[,'map_front'], levels = map_front_order)
location_sizes_map_front_2[which(is.na(location_sizes_map_front_2[,'has_actual_data'])) , 'loss_ratio'] = NA
location_sizes_map_front_2[,'Fighter'] = 'Fighter'
location_sizes_map_front_2[which(location_sizes_map_front_2[,'group_type_graph'] %in% c('Ground Attack', 'Bomber', "Transport")),'Fighter'] = 'Bomber'




x_coord_white_space = 100
x_coord_graph_space = 200
max_height_graph = 750 /10
max_height_graph = 550

#used for locations on graph 
num_aircraft_by_map_front = subset(location_sizes_map_front_2, group_type_graph !='') %>% group_by(map_front, date_start) %>% summarise(num_aircraft = sum(total_w_approx)) %>% as.data.frame()
num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(white_space_vertical = 0) %>% arrange(date_start, map_front) %>% group_by(date_start) %>%
  mutate(height = cumsum(num_aircraft + white_space_vertical), height_2 = (as.numeric(map_front) - 1)*max_height_graph) %>% as.data.frame()


# num_aircraft_by_map_front = merge(
#   
#   num_aircraft_by_map_front %>% mutate(white_space_vertical = 0) %>% arrange(date_start, map_front) %>% group_by(date_start) %>% 
#   mutate(height = cumsum(num_aircraft + white_space_vertical)) %>% as.data.frame(),
#   
#   num_aircraft_by_map_front %>% group_by(map_front) %>% arrange(-num_aircraft) %>% slice(3) %>%
#     mutate(height_2 = num_aircraft*(num_aircraft>500/10) + 500/10*(num_aircraft<500/10))  %>% as.data.frame() %>% 
#     mutate(height_2 = cumsum(height_2)) %>% select(map_front, height_2) %>% as.data.frame(), 
#   by = 'map_front')

num_aircraft_by_map_front = merge(num_aircraft_by_map_front,subset(num_aircraft_by_map_front, map_front == 'eastern_germany') %>% mutate(eastern_german_height = height_2) %>% select(date_start, eastern_german_height) , by = 'date_start')
#num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(end_height = height_2 - eastern_german_height) %>% 
#  mutate(start_height = end_height - num_aircraft )
num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(start_height = height_2 - eastern_german_height) %>% 
  mutate(end_height = start_height + num_aircraft)

num_aircraft_by_map_front = merge(num_aircraft_by_map_front, num_aircraft_by_map_front %>% select(date_start) %>% unique() %>% mutate( date_numeric_factor =  as.numeric(factor(date_start)) ) %>%
                                    mutate(x_coord = x_coord_graph_space) %>%  mutate(x_coord_end = cumsum((x_coord+x_coord_white_space)) ) %>% mutate(x_coord_start = x_coord_end-x_coord_graph_space) %>%
                                    select(date_start, x_coord_end, x_coord_start, date_numeric_factor) , on = 'date_start')
num_aircraft_by_map_front = merge(num_aircraft_by_map_front, map_front_names_df, by.y = 'map_front_order', by.x = 'map_front')
location_sizes_map_front_2 = merge(location_sizes_map_front_2, map_front_names_df, by.y = 'map_front_order', by.x = 'map_front')
# num_aircraft_by_map_front = merge(num_aircraft_by_map_front %>% mutate(map_front_cont = as.numeric(map_front))  %>% mutate(odd_map_fronts = map_front_cont%%2),
#                                   num_aircraft_by_map_front %>% mutate(map_front_cont = as.numeric(map_front))  %>% mutate(odd_map_fronts = map_front_cont%%2) %>% select(date_start, odd_map_fronts) %>% unique() %>% mutate( date_numeric_factor =  as.numeric(factor(date_start)) ) %>%
#                                     mutate(x_coord = x_coord_graph_space + x_coord_graph_space*odd_map_fronts/2) %>%  mutate(x_coord_end = cumsum((x_coord+x_coord_white_space)) ) %>% mutate(x_coord_start = x_coord_end-x_coord_graph_space) %>%
#                                     select(date_start, x_coord_end, x_coord_start, date_numeric_factor,odd_map_fronts) , on = c('date_start','odd_map_fronts'))


num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(x_coord_end = x_coord_end + 3000, x_coord_start = x_coord_start + 3000)
#num_aircraft_by_map_front[which(num_aircraft_by_map_front[,'end_height'] <=0),c('end_height', 'start_height')] = num_aircraft_by_map_front[which(num_aircraft_by_map_front[,'end_height'] <=0),c('end_height', 'start_height')] - 50


elements_to_pivot = num_aircraft_by_map_front %>% subset(num_aircraft > max_height_graph) %>% select(date_start, map_front)
elements_to_pivot = elements_to_pivot[-which((elements_to_pivot[,'date_start'] == '1944-10-15')*(elements_to_pivot[,'map_front'] == 'eastern_germany')==1),]
elements_to_pivot = elements_to_pivot[-which((elements_to_pivot[,'date_start'] == '1944-12-15')*(elements_to_pivot[,'map_front'] == 'eastern_germany')==1),]
elements_to_pivot = elements_to_pivot[-which((elements_to_pivot[,'date_start'] == '1940-05-15')*(elements_to_pivot[,'map_front'] == 'western_germany')==1),]
elements_to_pivot = elements_to_pivot[-which((elements_to_pivot[,'date_start'] == '1945-01-15')*(elements_to_pivot[,'map_front'] == 'eastern_germany')==1),]

for(x in unique(elements_to_pivot[,'date_start']) ){
  
  
  dates_to_shift = which(num_aircraft_by_map_front[,'date_start'] > x)
  num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] + x_coord_graph_space
  
  temp_loc = which((num_aircraft_by_map_front[,'date_start'] == x)*(num_aircraft_by_map_front[,'map_front']  %in% ( subset(elements_to_pivot, date_start == x) %>% pull(map_front) )) == 1)
  num_aircraft_by_map_front[temp_loc,'x_coord_end'] = num_aircraft_by_map_front[temp_loc,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[temp_loc,'x_coord_start'] = num_aircraft_by_map_front[temp_loc,'x_coord_start'] + x_coord_graph_space
  
}

elements_to_pivot = data.frame(
  date_start = c('1944-11-15'),
  map_front =  c('eastern_germany')
)

for(x in 1:nrow(elements_to_pivot)){
  
  dates_to_shift = which(num_aircraft_by_map_front[,'date_start'] > elements_to_pivot[x,'date_start'])
  num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] + x_coord_graph_space
  
  temp_loc = which((num_aircraft_by_map_front[,'date_start'] == elements_to_pivot[x,'date_start'])*(num_aircraft_by_map_front[,'map_front']  == elements_to_pivot[x,'map_front']) == 1)
  num_aircraft_by_map_front[temp_loc,'x_coord_end'] = num_aircraft_by_map_front[temp_loc,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[temp_loc,'x_coord_start'] = num_aircraft_by_map_front[temp_loc,'x_coord_start'] + x_coord_graph_space
  
}


location_sizes_map_front_2_simple = merge(location_sizes_map_front_2 %>% subset(group_type_graph != '') %>% mutate(approx_loss = loss_ratio* total_w_approx) %>%
                                            group_by(Fighter, map_front, date_start) %>%
                                            summarise( total = sum(total,na.rm = TRUE), total_w_approx = sum(total_w_approx,na.rm = TRUE))%>%
                                            as.data.frame(), 
                                          expand.grid(
                                            Fighter = c("Fighter", 'Bomber'),
                                            date_start= unique(location_sizes_map_front[,'date_start']),
                                            map_front = unique(location_sizes_map_front[,'map_front'])) ,
                                          by = c('Fighter','map_front', 'date_start'), all = TRUE) %>% 
  group_by(Fighter, map_front, date_start) %>% 
  summarise(total = sum(total, na.rm = TRUE), total_w_approx = sum(total_w_approx, na.rm = TRUE)) %>% as.data.frame()


location_sizes_map_front_2_simple = merge(location_sizes_map_front_2_simple, 
                                          location_sizes_map_front_2 %>% subset(group_type_graph != '')  %>% subset(has_actual_data == 1) %>% mutate(approx_loss = loss_ratio* total_w_approx) %>%
                                            group_by(Fighter, map_front, date_start) %>% summarise(  total_w_approx = sum(total_w_approx,na.rm = TRUE), approx_loss = sum(approx_loss,na.rm = TRUE)) %>%
                                            mutate(loss_ratio = approx_loss / total_w_approx) %>% as.data.frame() %>% select(-total_w_approx) ,
                                          by = c('Fighter','map_front', 'date_start'), all.x = TRUE)

location_sizes_map_front_2_simple = merge(location_sizes_map_front_2_simple, map_front_names_df, by.y = 'map_front_order', by.x = 'map_front')

temp_data = location_sizes_map_front_2_simple %>%
  subset(date_start == num_aircraft_by_map_front[x,c('date_start')] & map_front == num_aircraft_by_map_front[x,c('map_front')])

library(ggbrick)

get_barplot_date_map_simple = function(temp_data){
  
  my_colors <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
  
  if(which(map_front_names == temp_data[1,'map_front_names']) %%2 == 0 ){
    temp_colour = "#88CCEE"
  }else{temp_colour = "#44AA99"}
  
  total_aircraft = sum(temp_data[,'total_w_approx'])
  temp_data = temp_data %>% mutate(new_name = ifelse(Fighter == 'Bomber', 'B', 'F'))
  temp_data %>%
    ggplot(aes(x = new_name, y = total_w_approx, fill = log(loss_ratio+.0001)), label = total_w_approx )+geom_bar(stat = 'identity') +
    scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0), breaks = unique(c(temp_data[,'total_w_approx'], total_aircraft))) +
    #geom_text(data = temp_data , aes(x = Fighter, y = total_w_approx+20, label = total_w_approx)) + 
    # geom_text(data = temp_data, aes(x = factor(new_name),  y=total_w_approx-10, label = new_name), 
    #             position = position_dodge(0.9), size = 1, angle = 90,  color = "Black", hjust = 'left')+
    geom_point(data = temp_data  ,
               aes( y=approx_loss, x=new_name), stat ='identity', size = .25 ) +
    
    
    
    scale_colour_gradient(
      low = "blue",
      high = "red",
      aesthetics = "fill",
      limits = c(-5, 0.56),
      na.value = "grey50"
    ) + ylab(temp_data[1,'map_front_names'])+#theme( axis.text.y=element_text(size=1) ,  axis.title.y = element_text(size = 1) )+
    theme(text = element_text(size=(3.5)),legend.position="none",axis.title.x=element_blank(),
          axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.margin = margin(), 
          panel.background = element_rect(fill = temp_colour,
                                          colour = temp_colour),
          
          axis.ticks.length.x = unit(0,'pt'),axis.ticks.length.y = unit(0,'pt')#,axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.ticks.length.y = unit(0,'pt')
    )
  
  
  
  
}

get_barplot_date_map_simple = function(temp_data){
  
  my_colors <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
  
  if(which(map_front_names == temp_data[1,'map_front_names']) %%2 == 0 ){
    temp_colour = "#88CCEE"
  }else{temp_colour = "#44AA99"}
  
  total_aircraft = sum(temp_data[,'total_w_approx'])
  temp_data = temp_data %>% mutate(new_name = ifelse(Fighter == 'Bomber', 'B', 'F'))

    (temp_data %>% mutate(total_w_approx_loss = max(total_w_approx - approx_loss, 0)) %>% 
        select(Fighter, total_w_approx_loss, approx_loss) %>% 
        reshape::melt(id.vars = 'Fighter')) %>% ggplot() + geom_waffle(aes(as.factor(Fighter), value, fill = as.character(variable))) +
      scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0), breaks = unique(c(temp_data[,'total_w_approx'], total_aircraft))) +
    #geom_text(data = temp_data , aes(x = Fighter, y = total_w_approx+20, label = total_w_approx)) + 
    # geom_text(data = temp_data, aes(x = factor(new_name),  y=total_w_approx-10, label = new_name), 
    #             position = position_dodge(0.9), size = 1, angle = 90,  color = "Black", hjust = 'left')+

    ylab(temp_data[1,'map_front_names'])+#theme( axis.text.y=element_text(size=1) ,  axis.title.y = element_text(size = 1) )+
    theme(text = element_text(size=(3.5)),legend.position="none",axis.title.x=element_blank(),
          axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.margin = margin(), 
          panel.background = element_rect(fill = temp_colour,
                                          colour = temp_colour),
          
          axis.ticks.length.x = unit(0,'pt'),axis.ticks.length.y = unit(0,'pt')#,axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.ticks.length.y = unit(0,'pt')
    )
  
  
  
  
}


temp_plots_simple = lapply(1:nrow(num_aircraft_by_map_front),
                           
                           function(x){
                             plot = location_sizes_map_front_2_simple %>%
                               subset(date_start == num_aircraft_by_map_front[x,c('date_start')] & map_front == num_aircraft_by_map_front[x,c('map_front')]) %>%
                               get_barplot_date_map_simple
                             return(plot)
                             
                           }
                           
)
df <- data.frame(x = c((-100),max(num_aircraft_by_map_front[,'x_coord_end'])),
                 y = c(min(num_aircraft_by_map_front[,'start_height']) *1.1,max(num_aircraft_by_map_front[,'start_height'])*1.1))

base <- ggplot(df, aes(x, y)) +
  geom_blank() +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.text.x=element_blank(),axis.text.y=element_blank()
  )+ xlab('')+ylab('') 

library(grid)

# Function to rotate a grob
rotateGrob <- function(grob, angle = 90) {
  grob$children <- lapply(grob$children, function(child) {
    if (inherits(child, "grob")) rotateGrob(child, angle) else child
  })
  grob$vp <- viewport(angle = angle, width = unit(1,"npc"), height = unit(1,"npc"))
  return(grob)
}

maps = lapply(map_front_order, get_map_front_map)


# Create the plot
map_overall <- ggplot() +
  geom_sf(data = shp_files_1938 %>% filter(NAME %in% c(unique(luftwaffe_locations[,'name']), 'Sweden', 'Portugal','Morocco','Algeria', 'Turkey') )) +
  geom_point(data = unique_locs , aes(x = lon, y = lat), alpha = .5, size = 1, shape = 4, colour = 'red') + 
  xlim(xmin = -10, xmax = 40)  + 
  ylim(ymin = 30, ymax = 70)  + 
  theme_minimal() +  theme(legend.position = '') + theme_void() 


g <- ggplotGrob(map_overall)
# Rotate the grob
rotated_grob <- rotateGrob(g, angle = 90)  # Rotate 90 degrees
#grid.newpage()
#grid.draw(rotated_grob)
ggsave(rotated_grob,file = "/users/sweiss/downloads/rotated_plot.png", width = 6*aspect_ratio, height = 6)
img <- png::readPNG("/users/sweiss/downloads/rotated_plot.png")
grob <- rasterGrob(img, width=unit(1*aspect_ratio,"npc"), height=unit(1,"npc"))
base = base +  annotation_custom(grob = grob,
                                 ymin = -2500/2,
                                 ymax = 2500/2,
                                 xmin = 0,
                                 xmax = 2500) 

# Save the plot, adjust 'width' and 'height' based on your calculated dimensions
ggsave(map, file = "/users/sweiss/downloads/map_plot.png", width = 8, height = 8, units = "in")



for(x in 1:length(map_front_order)){
  
  points_sf <- st_as_sf(unique_locs %>% subset(map_front == map_front_order[x]), coords = c("lon", "lat"), crs = 4326)
  
  # Now, assuming your shapefile's CRS is something else, for example, EPSG:3857
  # You transform your points to match
  points_transformed <- st_transform(points_sf, crs = 3857)
  
  
  # Assuming `points_transformed` is your points sf object and `shapefile_data` is your shapefile sf object
  
  # Calculate width and height based on the extent of the points
  extent <- st_bbox(points_transformed)
  x_range <- extent$xmax - extent$xmin
  y_range <- extent$ymax - extent$ymin
  
  
  # Assuming an aspect ratio of 1, or adjust according to your needs
  aspect_ratio <- y_range / x_range  
  #lat_lon_ratio = temp_max_mins_lat_lon %>% mutate(rat_lon = max_lon / min_lon, rat_lat = max_lat / min_lat) %>% 
  #  mutate(ratio_ratio_lat_lon = rat_lon/rat_lat) %>% 
  #  pull(ratio_ratio_lat_lon)
  

  temp_max_mins_lat_lon = unique_locs %>% group_by(map_front) %>% summarise(min_lon = min(lon), max_lon = max(lon),
                                                    min_lat = min(lat), max_lat = max(lat)) %>% 
    as.data.frame() %>% subset(map_front == map_front_order[x])
  
  lat_lon_data = temp_max_mins_lat_lon %>% mutate(lon_len = max_lon - min_lon, lat_len = max_lat - min_lat) %>% 
    mutate(ratio_ratio_lat_lon = lat_len/lon_len)
  
  
  lat_lon_ratio = lat_lon_data %>% 
    pull(ratio_ratio_lat_lon)
  #lat_lon_ratio = 1/lat_lon_ratio
  
  temp_df = num_aircraft_by_map_front %>% subset(date_start == min(date_start)) %>% subset(map_front == map_front_order[x]) %>% 
    mutate(end_height = start_height + 500 )

  
  g <- ggplotGrob(maps[[x]])
  
  # Rotate the grob
  rotated_grob <- rotateGrob(g, angle = 90)  # Rotate 90 degrees
  #grid.newpage()
  #grid.draw(rotated_grob)
  ggsave(rotated_grob,file = "/users/sweiss/downloads/rotated_plot.png", width = 6, height = 6)
  img <- png::readPNG("/users/sweiss/downloads/rotated_plot.png")
  grob <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
  

  base = base +  annotation_custom(grob = grob,
                                   ymin = temp_df[,'start_height'],
                                   ymax = temp_df[,'end_height'],
                                   xmin = 2500,
                                   xmax = 3000) 
  
  
}

colSums(abs(df))

ggsave(base, file="/users/sweiss/downloads/main_plot_country_maps.png",width = 35000, height = 13310, units = "px",limitsize = FALSE)

for( x in 1:length(temp_plots_simple)){
  if((num_aircraft_by_map_front[x,'date_start'] > '1939-08-01' )& (num_aircraft_by_map_front[x, 'num_aircraft'] > 1.6 )){
    base = base +  annotation_custom(grob = ggplotGrob(temp_plots_simple[[x]]),
                                     ymin = num_aircraft_by_map_front[x,'start_height'],
                                     ymax = num_aircraft_by_map_front[x,'end_height'],
                                     xmin = num_aircraft_by_map_front[x,'x_coord_start'],
                                     xmax = num_aircraft_by_map_front[x,'x_coord_end']
                                     )
    
    
  }
  
}

top_movements = (location_sizes_map_front_prev %>%
                   subset(group_type_graph !='' & map_front != prev_map_front & group_type_graph !='') %>%
                   group_by(prev_map_front, map_front, date_start, distance_to_e_germany_plot_prev, distance_to_e_germany_plot) %>%
                   summarise(total_w_approx = sum(total_w_approx)) %>% as.data.frame() %>%
                   mutate(curve_heights = sign( distance_to_e_germany_plot_prev - distance_to_e_germany_plot)*total_w_approx ))  %>% subset(total_w_approx >60)


for( x in  1:nrow(top_movements) ){
  
  end_data = subset(num_aircraft_by_map_front, map_front == top_movements[x,'map_front'] & date_start == top_movements[x,'date_start'])
  start_data = subset(num_aircraft_by_map_front, map_front == top_movements[x,'prev_map_front'] & date_start == top_movements[x,'date_start']-months(1))
  
  y_start = start_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
  x_start = start_data %>% pull(x_coord_end)
  
  y_end = end_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
  x_end = end_data %>% pull(x_coord_start)
  temp_df_start_end = data.frame(y_start, x_start, y_end, x_end, size = top_movements[x,'total_w_approx'])
  map_front_adjustment_y = (max(as.numeric(num_aircraft_by_map_front[,'map_front'])) - as.numeric(start_data[,'map_front'])) /max(as.numeric(num_aircraft_by_map_front[,'map_front']))*(max_height_graph/2)
  print(map_front_adjustment_y)
  temp_df_start_end = data.frame(y_start, x_start,  y_end , x_end, size = top_movements[x,'total_w_approx'])
  
  base = base +  geom_curve(data = temp_df_start_end,
                            aes(x = x_start, xend = x_end,
                                y = y_start, yend = y_end  ,
                                alpha = .02,  size = size
                            ),
                            arrow = arrow(type="closed",length = unit(0.25/2,"cm")), 
                            curvature = .005) + scale_size(range = c(0, 2), guide = 'none')
  
  
  
  
}
# 
date_locs = num_aircraft_by_map_front %>% group_by(date_start) %>% summarise(start_x = min(x_coord_start), end_x = max(x_coord_end)) %>% as.data.frame()
for(x in 1:nrow(date_locs)){
  
  base = base + annotation_custom(grob = textGrob(substr(date_locs[x,'date_start'], 1,7), rot = 0, gp=gpar(fontsize=5,fontface="italic")),  xmin = date_locs[x,'start_x'], xmax = date_locs[x,'end_x'], ymin = min(df[,'y']), ymax = min(df[,'y']))
  
  if(as.numeric(substr(date_locs[x,'date_start'], 6,7)) %% 2 == 0){
    base = base + annotate("rect", ymin = -Inf, ymax = Inf, xmin = date_locs[x,'start_x'], xmax = date_locs[x,'end_x'], fill = "grey", alpha = .1, color = NA)
    
  }
  
}

shoah = read.csv('data/shoah_timeline.csv')
shoah = merge(
  shoah, 
  luftwaffe_locations %>% select(country, map_front) %>% unique() %>% as.data.frame(), 
  by = 'country', all.x = TRUE
)
shoah = shoah %>% mutate(date_start = as.Date(paste0(substr(date, 1 ,8 ), '15') ))
shoah = merge(shoah, 
              num_aircraft_by_map_front %>% select(date_start, map_front, end_height, start_height, x_coord_end, x_coord_start),
              by = c('date_start','map_front')
)
for(x in 1:nrow(shoah)){
  
  base = base + annotation_custom(grob = textGrob(shoah[x,'Text'], rot = 90,gp=gpar(fontsize=3,fontface="italic", col = "#f8a53c")),  xmin = shoah[x,'x_coord_end']+50, xmax = shoah[x,'x_coord_start']+100, ymin = shoah[x,'end_height'], ymax = shoah[x,'start_height'])
  
  
}


title = 'A High Altitude Overview of the European Theater 1939-1945'
second_title = 'Und Dass Sowas Von Sowas Kommt'
base = base + annotation_custom(grob = textGrob(title, rot = 90,gp=gpar(fontsize=20,fontface="bold", col = "black")),  xmin = -100, xmax = -66, ymin = min(df[,'y']), ymax = max(df[,'y']))
base = base + annotation_custom(grob = textGrob(second_title, rot = 90,gp=gpar(fontsize=10, fontface = "italic")),  xmin = -30, xmax = 0, ymin = min(df[,'y']), ymax = max(df[,'y']))

date_min_max_heights = num_aircraft_by_map_front%>% subset(num_aircraft > 0) %>% group_by(date_start) %>% 
  summarise(min_height = min(start_height) , max_height = max(end_height))%>% as.data.frame()
timeline_text = read.csv('data/ww2_chronology.csv')
timeline_text = merge(timeline_text %>% mutate(date_start = as.Date(date)), date_locs, by = 'date_start')
timeline_text = merge(timeline_text, date_min_max_heights, by = 'date_start')
# timeline_text = timeline_text %>% mutate(y_graph_end = ifelse(east == "East", max_height, min_height))
timeline_text = timeline_text %>% mutate(y_start = ifelse(east == "East", max_height+100, min(df[,'y'])+100 ))
timeline_text = timeline_text %>% mutate(y_end = ifelse(east == "East", max((df[,'y'])), min_height-100  ))

base = base + annotation_custom(grob = textGrob("Data and code can be found here www.ww2.dk and code and here github.com/samcarlos/luftwaffe_locations.", rot = 90,gp=gpar(fontsize=10, fontface = "italic")),  xmin = max(df[,'x'])+200, xmax = max(df[,'x'])+300, ymin = min(df[,'y']), ymax = max(df[,'y']))



explanation = "Below is a graphic that displays the movements, sizes, and losses (where data is available) of the Luftwaffe during the Second World War. Each Luftwaffe unit is geolocated and placed in a specified 'Front'. Germany is in the middle of the graphic while others are roughly ordered by the distance to Berlin (in E Germany). The left side are the Western Fronts (predominantly USA, GB) and the right side are the Eastern Fronts (USSR). For each month there are barplots for bombers (the first) and fighters (the second) in each of these Fronts. The width of each front is proportional to the sum of the number bombers and fighters. Between March 1942 and the end of 1944 there are actual numbers along with 'lost' aircraft written off for any reason.  If loss data exists then the barchart is colored depending on the loss rate (relatively high losses are red, low are blue) and a point is placed on the barplot to display the actual number of aircraft lossed. If there is no data available the barplots are gray and the sizes are estimated. Arrows represent the number of larger movements between fronts over time. As Luftwaffe units normally go back and forth between Germany to refit the arrows display only moves consisting of at least 60 planes. Black text outside the main plot describes the war on a month-month basis. Yellow text within the graphic describes stages of the Holocaust." 
base = base + annotation_custom(
  grob = textbox_grob(
    explanation,hjust = 0, vjust = 0, halign = .5, valign = 0,
    width = unit(5, "inches"), height = unit(1, "inches"),
    orientation = "left-rotated"#, box_gp = gpar(col = "black"),
    ,gp=gpar(fontsize=6)
  )   ,
  xmin = 0,
  xmax = 600,
  ymin = -400,
  ymax = 800
)



for(x in 1:nrow(timeline_text)){
  hadjust = 0
  if(timeline_text[x,'east'] == 'West'){hadjust = 1}
  
  base = base + annotation_custom(
    grob = textbox_grob(
      timeline_text[x,'text'],hjust = hadjust, vjust = 0, halign = .5, valign = 0,
      width = unit(2, "inches"), height = unit(1, "inches"),
      orientation = "left-rotated"#, box_gp = gpar(col = "black"),
      ,gp=gpar(fontsize=6)
    )   ,
    xmin = timeline_text[x,'start_x'],
    xmax = timeline_text[x,'end_x'],
    ymin = timeline_text[x,'y_start'],
    ymax = timeline_text[x,'y_end']
  )
  
}
base = base+theme(legend.position = 'none')

colSums(abs(df))
ggsave(base, file="/users/sweiss/downloads/main_plot_country_maps.png",width = 35000, height = 13310, units = "px",limitsize = FALSE)
ggsave(base, file="/users/sweiss/downloads/main_plot_country_maps.png",width = 35000, height = 35000*4/8, units = "px",limitsize = FALSE)


ggsave(base, file="plots/main_plot.png",width = 20000, height = 6000, units = "px",limitsize = FALSE)



