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
load('data/luftwaffe_sizes_locations_2.rdata')


#merge location data with sizes and loss data 
#map_front are geographicial locations where luftwaffe fought


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

location_sizes_map_front_prev = location_sizes_map_front_prev %>% mutate(plot_lost = lost_enemy + lost_accident + lost_maintenance )
location_sizes_map_front_prev_1 = location_sizes_map_front_prev %>% select(prev_map_front, map_front, group_type_graph, prev_lat, 
                                                                           prev_lon, lat, lon, total_w_approx, date_start, plot_lost)# %>%  subset(date_start > '1940-04-01' & date_start < '1941-10-01')
location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_1 %>% uncount(total_w_approx)
location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_1 %>% uncount(plot_lost)
location_sizes_map_front_prev_uncounted_lost[,'lon_to_go'] = as.numeric(location_sizes_map_front_prev_uncounted_lost[,'date_start']) 

location_sizes_map_front_prev_uncounted_lost[,'eastern_front'] = 'East'
location_sizes_map_front_prev_uncounted_lost[which(location_sizes_map_front_prev_uncounted_lost[,'map_front'] %in% western_fronts),'eastern_front'] = 'West'

location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% 
  mutate(lon_to_go = ((lon_to_go - as.numeric(as.Date('1939-09-15') )))) %>% 
  mutate(lon_to_go = lon_to_go/(as.numeric(as.Date('1945-05-15') )-as.numeric(as.Date('1939-09-15') )))
#location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = lon_to_go - min(lon_to_go)  ) %>% mutate(lon_to_go = lon_to_go/max(lon_to_go) )

xmin = -5
xmax = 47
ymin = 30 - 3307/ 300
ymax = 70


location_sizes_map_front_prev_uncounted_lost= location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = lon_to_go * (xmax - xmin))
#location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% group_by( date_start, eastern_front) %>% mutate(plocation_order =  1:n()) %>%
#  mutate(new_lat = plocation_order/300) %>% 
#  #mutate(new_lat = ymin + new_lat ) %>% as.data.frame()
#  mutate(new_lat = ymin + new_lat*(eastern_front == 'East') + (1915/300) - (eastern_front != 'East')*new_lat  ) %>% as.data.frame()

location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% group_by( date_start) %>% mutate(plocation_order =  1:n()) %>%
  mutate(new_lat = plocation_order/300) %>% 
  mutate(new_lat = ymin + new_lat ) %>% as.data.frame()



location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(plocation_order = paste0(date_start, plocation_order, group_type_graph))
lost_to_maps = rbind(location_sizes_map_front_prev_uncounted_lost %>% mutate(lat = new_lat, lon = lon_to_go, date_start = date_start + months(1) - days(14)) %>% select(lat, lon, plocation_order, date_start, group_type_graph),
                     location_sizes_map_front_prev_uncounted_lost %>% select(lat, lon, plocation_order, date_start, group_type_graph)
) %>% mutate(location_order = plocation_order) %>% mutate(new_lat = lat, new_lon = lon)


location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_uncounted %>% mutate(date_end = date_start + months(1) - days(1)) %>% 
  mutate(plocation_order = 1:n())
to_maps = rbind(location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon) %>% 
                  select(lat, lon, plocation_order, date_start, group_type_graph), 
                
                location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon,
                                                                   date_start = date_start + months(1) - days(14)) %>% 
                  select(lat, lon, plocation_order, date_start, group_type_graph),
                location_sizes_map_front_prev_uncounted %>% mutate(date_start = date_end)  %>% select(lat, lon, plocation_order, date_start,group_type_graph)
) 

to_maps = to_maps %>% mutate(fighters = (group_type_graph == 'Fighter') + (group_type_graph == 'Night Fighter') + (group_type_graph == 'TE_Fighter'))

#to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
#  group_by(date_start,lat, lon) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
#  mutate(location_order = 1:n() / 50) %>% 
#  mutate(new_lat = location_order  + lat ,
#         new_lon =  lon) %>%
#  as.data.frame()

to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
  group_by(date_start,lat, lon,fighters) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
  mutate(location_order = 1:n()) %>% 
  mutate(new_lat = (lat + ((location_order) %% 100 )/50)*fighters  +( (lat - ((location_order) %% 100 )/50) + 2)*(1-fighters)  ,
         new_lon =  (lon + floor(location_order/100) / 5+.1) *fighters + ((lon - floor(location_order/100) / 5 + 2 - .1))*(1-fighters) #,Plane_Type = group_type_graph
         ) %>%
  as.data.frame()



test_permant_df = merge(location_sizes_map_front_prev_uncounted_lost %>% select(new_lat, lon_to_go, plocation_order, date_start, group_type_graph),
                        expand.grid(date_start = c(lost_to_maps%>% pull(date_start) %>% unique(), 
                                                   to_maps%>% pull(date_start) %>% unique()), 
                                    plocation_order = location_sizes_map_front_prev_uncounted_lost %>% pull(plocation_order) %>% unique() ) , 
                        by = c('date_start', 'plocation_order'), all = TRUE)%>% arrange(plocation_order, date_start, group_type_graph) 
test_permant_df = test_permant_df %>% group_by(plocation_order) %>% fill(new_lat, lon_to_go , group_type_graph) %>% as.data.frame()
test_permant_df = test_permant_df[-which(is.na(test_permant_df[,'new_lat'])),]
test_permant_df = test_permant_df %>% mutate(location_order = paste(date_start, plocation_order), lat = new_lat, lon = lon_to_go, new_lon = lon_to_go) %>% select(-lon_to_go)
test_permant_df = merge(test_permant_df, 
                        lost_to_maps %>% group_by(plocation_order) %>% summarise(date_start = min(date_start)) %>% as.data.frame() %>% mutate(exclude = 1),
                        by =  c('date_start', 'plocation_order'), all = TRUE
)
test_permant_df = test_permant_df %>% filter( is.na(exclude)) %>% select(-exclude)
lost_to_maps = rbind(lost_to_maps, test_permant_df)

library(rnaturalearth)
library(gganimate)
world_map <- ne_countries(scale = 50, returnclass = 'sf')

luft_pallette = c('#f6bb19', '#1f1917','#ceb267','#1c2f5b', '#cf3110','#b77d42','#1b2010')

text_dates = data.frame(dates = as.Date(c( '1939-09-15', '1940-01-15', '1941-01-15', '1942-01-15', '1943-01-15', '1944-01-15', '1945-01-15', '1945-05-15')),
                        text = c('Start', '1940', '1941', '1942', '1943', '1944', '1945', 'End'))
text_dates = text_dates %>% mutate(x_loc = as.numeric(dates)) %>%  mutate(x_loc = ((x_loc - as.numeric(as.Date('1939-09-15') )))) %>% 
  mutate(x_loc = x_loc/(as.numeric(as.Date('1945-05-15') )-as.numeric(as.Date('1939-09-15') )))

text_dates = text_dates %>% mutate(x_loc = x_loc * (xmax))

y_scales = data.frame(y_loc = ymin + c(5, 10, 15, 20, 25, 30 )/ 3,
                      text = c(5, 10, 15, 20, 25, 30)*100)

##

#list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII')[grep('.shp',list.files('/Users/sweiss/Downloads/EuropeanBorders_WWII'))]
my_spdf <- readOGR( 
  dsn= '/users/sweiss/Downloads/EuropeanBorders_WWII/December_31_1942.shp' ,
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

map = ggplot()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + theme(axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank()) +
   geom_polygon(data = germG, aes(x = long, y = lat, group = group) ,color = "black", size = 0.1, fill = "white", linetype = 2) +
  coord_equal() 

map = map +     geom_polygon(data = locs_to_keep_shp_files_1938, aes(x = long, y = lat, group = group) ,color = "black", size = 0.1,linetype = 2, fill = "white") +
  coord_equal()+
  coord_sf(xlim = c(-5,50), ylim = c(30,70)) +  geom_vline(xintercept = seq(-100,100,2), linetype = 1, alpha = .25) + 
  geom_hline(yintercept = seq(-100,100,2), linetype = 1, alpha = .25) 

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#0072B2","#F0E442","#E69F00", "#CC79A7", "#009E73")
names(cbPalette) = c('Bomber', 'Fighter', 'Night Fighter', 'Ground Attack', 'TE_Figther')
data.frame(cbPalette) %>% mutate(index = 1:n()) %>% ggplot(aes(x = index, y=  index, colour = cbPalette, label = cbPalette))+
  geom_point() +scale_colour_manual('',values=cbPalette) + geom_text()

animation_data = rbind(to_maps %>% select( -numeric_group_type_graph, - fighters), lost_to_maps )
animation_data[,'group_type_graph'] = factor(animation_data[,'group_type_graph'], 
                                             levels = c('Fighter', 'TE_Fighter', 'Night Fighter', 'Bomber', 'Ground Attack'))
temp_map =  map +
  annotate("rect", xmin = xmin, xmax = xmax, ymin = 00, ymax = 29,
           alpha =1,fill = "white")+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=30), fill="white", alpha=1) +
  geom_point(data = animation_data %>% mutate(Plane_Type = group_type_graph) %>% 
               mutate(date_start = (date_start)) %>% arrange(date_start) %>%  subset(date_start == '1942-02-15'  )
             ,aes(x = new_lon , y = new_lat  , group = plocation_order, colour = Plane_Type, 
                  alpha = 1 , size =.1), inherit.aes = FALSE) +
  scale_size(range = c(0,.01), guide = 'none') +
  #geom_point(data = luftwaffe_locations%>%  subset(date_start > '1942-02-14' & date_start < '1944-12-14'  ) %>% select(date_start, lon, lat) 
  #           %>% unique(), aes(x = lon, y = lat),shape = 3, size = .1) + 
  geom_text(data = text_dates, aes(x = x_loc, y = ymin-2, label = text, angle = 90))+ xlab('')+ylab('')+
  geom_text(data = y_scales, aes(x = 0, y = y_loc, label = text)) + 
  coord_sf(xlim = c(xmin,xmax), ylim = c(ymin-2,ymax)) + 
  scale_colour_manual(name = "group_type_graph", values=cbPalette)+  
  geom_text(aes(x = 20, y = 28, label = 'Number of Planes Lost By Month'))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  #geom_sf(data = world_map, color = 'grey')+ 
  #transition_time((date_start )) + 
  #transition_reveal(date_start) + 
  #exit_disappear()+
  theme(legend.position = "bottom") +#ease_aes('linear', interval = 0.01,state_length = 1)+
  labs(title = paste('Monthly Movements and Losses of the Luftwaffe',' (Month): {frame_time}'))+
  scale_alpha(guide = 'none')  + theme(text = element_text(size = 20)) + 
  theme(plot.title = element_text(hjust = 0.5))
temp_map



animation = map +
  annotate("rect", xmin = xmin, xmax = xmax, ymin = 00, ymax = 29,
           alpha =1,fill = "white")+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=30), fill="white", alpha=1) +
  geom_point(data = rbind(to_maps %>% select( -numeric_group_type_graph, - fighters), lost_to_maps ) %>% 
               mutate(date_start = (date_start)) %>% arrange(date_start) %>%  subset(date_start > '1943-07-15'  & date_start < '1944-07-15'  )
             ,aes(x = new_lon , y = new_lat  , group = plocation_order, colour = group_type_graph, 
                  alpha = 1 , size =.1), inherit.aes = FALSE) +
  scale_size(range = c(0,.01),guide = 'none') +
  #geom_point(data = luftwaffe_locations%>%  subset(date_start > '1942-02-14' & date_start < '1944-12-14'  ) %>% select(date_start, lon, lat) 
  #           %>% unique(), aes(x = lon, y = lat),shape = 3, size = .1) + 
  geom_text(data = text_dates, aes(x = x_loc, y = ymin-2, label = text, angle = 90))+ xlab('')+ylab('')+
  geom_text(data = y_scales, aes(x = 0, y = y_loc, label = text)) + 
  coord_sf(xlim = c(xmin,xmax), ylim = c(ymin-2,ymax)) + 
  scale_colour_manual(values=cbPalette)+  
  geom_text(aes(x = 20, y = 28, label = 'Number of Planes Lost By Month'))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  #geom_sf(data = world_map, color = 'grey')+ 
  transition_time((date_start )) + 
  #transition_reveal(date_start) + 
  exit_disappear()+
  theme(legend.position = "bottom") +ease_aes('linear', interval = 0.01,state_length = 1)+
  labs(title = paste('Monthly Movements and Losses of the Luftwaffe',' (Month): {frame_time}'))+
  scale_alpha(guide = 'none') +  theme(text = element_text(size = 20)) + 
  theme(plot.title = element_text(hjust = 0.5))

temp_move_plot = animate(animation, 
                         duration = 60, 
                         fps  =  30, height = 2338/2 , width = 3600/2,
                         start_pause = 60, end_pause = 120)



anim_save(filename = "/users/sweiss/downloads/mid_war2.gif", temp_move_plot)

