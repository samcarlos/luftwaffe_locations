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



location_sizes_map_front %>% mutate(loss_ratio = (lost_enemy  + lost_accident + lost_maintenance) / total_w_approx) %>% mutate(approx_loss = loss_ratio* total_w_approx) %>% 
  group_by(map_front, date_start, group_type_graph) %>% 
  subset(group_type_graph == 'Fighter') %>% 
  filter(date_start %in% as.Date(c('1944-05-15', '1944-06-15','1944-07-15'))) %>% 
  filter(map_front %in% c('France', 'eastern_germany', 'western_germany', 'southern_germany')) %>%
  summarise(totals = sum(total_w_approx), sum_lost = sum(approx_loss)) %>% View()


lost_fighters_by_month = location_sizes_map_front %>% subset(group_type_graph == 'Fighter') %>% 
  mutate(loss_ratio = (lost_enemy  + lost_accident + lost_maintenance) / total_w_approx) %>% 
  mutate(approx_loss = loss_ratio* total_w_approx) %>% 
  group_by(date_start) %>% 
  summarise(Total = sum(total_w_approx), Losses = sum(approx_loss, na.rm= TRUE)) %>% 
  as.data.frame() 
lost_fighters_by_month_melt = melt(lost_fighters_by_month, id.vars = 'date_start')

lost_fighters_by_month_france = location_sizes_map_front %>% subset(group_type_graph == 'Fighter' & map_front == 'France') %>% 
  mutate(loss_ratio = (lost_enemy  + lost_accident + lost_maintenance) / total_w_approx) %>% 
  mutate(approx_loss = loss_ratio* total_w_approx) %>% 
  group_by(date_start) %>% 
  summarise(Total = sum(total_w_approx), Losses = sum(approx_loss, na.rm= TRUE)) %>% 
  as.data.frame() 
lost_fighters_by_month_france_melt = melt(lost_fighters_by_month_france, id.vars = 'date_start')
lost_fighters_by_month_france_melt[,'variable'] = paste0(lost_fighters_by_month_france_melt[,'variable'], ' in France')
first_half_44_fighters_lost_france = rbind(lost_fighters_by_month_france_melt) %>% subset(date_start > '1944-01-01' & date_start < '1944-07-01') %>% ggplot(aes(x = as.Date(as.Date(date_start - days(14))), y = value, group = variable, colour = variable)) + 
  geom_line(size = .75)+
  theme_minimal() +
  theme(legend.position="bottom", text = element_text(size = 5), panel.grid.minor = element_blank()) +
  ggtitle('Number Luftwaffe Fighter Planes in France \nTotals and Losses (Jan - Jun 1944)') + 
  ylab('Total Number of Fighter Planes') + 
  xlab('Date (Month)') +scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  scale_color_manual(values = c("Total in France" = "#f6bb19", "Losses in France" = "#cf3110"))

first_half_44_fighters_lost = rbind(lost_fighters_by_month_melt) %>% subset(date_start > '1944-01-01' & date_start < '1944-07-01') %>% ggplot(aes(x = as.Date(as.Date(date_start - days(14))), y = value, group = variable, colour = variable)) + 
  geom_line(size = .75)+
  theme_minimal() +
  theme(legend.position="bottom", text = element_text(size = 5), panel.grid.minor = element_blank()) +
  ggtitle('Number Luftwaffe Fighter Planes All Fronts \nTotals and Losses (Jan - Jun 1944)') + 
  ylab('Total Number of Fighter Planes') + 
  xlab('Date (Month)') +scale_x_date(date_labels="%b %y",date_breaks  ="1 month") + 
  scale_color_manual(values = c("Total" = "#f6bb19", "Losses" = "#cf3110"))

ggsave(first_half_44_fighters_lost_france, file="plots/first_half_44_fighters_lost_france.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)

ggsave(first_half_44_fighters_lost, file="plots/first_half_44_fighters_lost.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)


ggplot() + geom_bar(data = lost_fighters_by_month %>% subset(date_start > '1944-01-01' & date_start < '1944-08-01'), 
                    aes(x=date_start, y = totals, fill = sum_lost/totals), stat = 'identity') + 
  geom_point(data = lost_fighters_by_month %>% subset(date_start > '1944-01-01' & date_start < '1944-08-01'), 
           aes(x=date_start, y = sum_lost))+scale_fill_gradient(low="blue", high="red")

lost_fighters_by_month %>% subset(date_start > '1944-01-01' & date_start < '1944-08-01') %>% ggplot(aes(x = date_start, y = value, group = variable, fill = variable)) + geom_bar(stat = 'identity')

subset(date_start > '1944-01-01' & date_start < '1944-09-01') %>%
  
location_sizes_map_front %>% group_by(map_front, date_start, group_type_graph) %>% 
  subset(group_type_graph == 'Fighter') %>% 
  summarise(totals = sum(total_w_approx)) %>% View()






subset(location_sizes, date_start == '1943-04-15') %>% ggplot(aes(x = lon, y=  lat, size = approx_size)) + geom_point()+ facet_wrap(group_type~.)

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

# location_sizes_map_front_prev = merge(location_sizes_map_front_prev %>% mutate(og_lat = lat, og_lon = lon) %>% select(-lat,-lon), 
#                                       avg_locs_map_front %>% select(map_front, distance_to_e_germany_plot, lat, lon) , 
#                                       by = 'map_front')
# location_sizes_map_front_prev = merge(location_sizes_map_front_prev, 
#                                       avg_locs_map_front %>% select(map_front, distance_to_e_germany_plot,lat , lon) %>%
#                                         mutate(distance_to_e_germany_plot_prev = distance_to_e_germany_plot, 
#                                                prev_map_front = map_front, 
#                                                prev_lat = lat, prev_lon = lon,
#                                                prev_map_front_lat = lat, prev_map_front_lon = lon) %>%
#                                         select(prev_map_front, distance_to_e_germany_plot_prev, prev_lat, prev_lon), by = 'prev_map_front')

location_sizes_map_front_prev = location_sizes_map_front_prev %>% mutate(plot_lost = lost_enemy + lost_accident + lost_maintenance )
location_sizes_map_front_prev_1 = location_sizes_map_front_prev %>% select(prev_map_front, map_front, group_type_graph, prev_lat, 
                                                                           prev_lon, lat, lon, total_w_approx, date_start, plot_lost)# %>%  subset(date_start > '1940-04-01' & date_start < '1941-10-01')
location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_1 %>% uncount(total_w_approx)
location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_1 %>% uncount(plot_lost)
location_sizes_map_front_prev_uncounted_lost[,'lon_to_go'] = as.numeric(location_sizes_map_front_prev_uncounted_lost[,'date_start'])
location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = ((lon_to_go - min(lon_to_go)))) %>% mutate(lon_to_go = lon_to_go/max(lon_to_go))
location_sizes_map_front_prev_uncounted_lost= location_sizes_map_front_prev_uncounted_lost %>% mutate(lon_to_go = lon_to_go * 40)
location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% group_by(group_type_graph, date_start) %>% mutate(plocation_order = 1:n()) %>%
  mutate(new_lat = 30+plocation_order/10) %>% as.data.frame()
location_sizes_map_front_prev_uncounted_lost = location_sizes_map_front_prev_uncounted_lost %>% mutate(plocation_order = paste0(date_start, plocation_order, group_type_graph))
lost_to_maps = rbind(location_sizes_map_front_prev_uncounted_lost %>% mutate(lat = new_lat, lon = lon_to_go, date_start = date_start + months(1) - days(14)) %>% select(lat, lon, plocation_order, date_start, group_type_graph),
                     location_sizes_map_front_prev_uncounted_lost %>% select(lat, lon, plocation_order, date_start, group_type_graph)
) %>% mutate(location_order = plocation_order) %>% mutate(new_lat = lat, new_lon = lon)

location_sizes_map_front_prev_uncounted = location_sizes_map_front_prev_uncounted %>% mutate(date_end = date_start + months(1) - days(1)) %>% 
  mutate(plocation_order = 1:n())

#to_maps = rbind(location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon) %>% 
#                  select(lat, lon, plocation_order, date_start, group_type_graph), 
#                
#                location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon,
#                                                                   date_start = date_start + months(1) - days(14)) %>% 
#                  select(lat, lon, plocation_order, date_start, group_type_graph),
#                location_sizes_map_front_prev_uncounted %>% mutate(date_start = date_end)  %>% select(lat, lon, plocation_order, date_start,group_type_graph)
#) 
to_maps = location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon) %>% 
                    select(lat, lon, plocation_order, date_start, group_type_graph)
to_maps = rbind(location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon) %>% 
                  select(lat, lon, plocation_order, date_start, group_type_graph), 
                
                location_sizes_map_front_prev_uncounted %>% mutate(lat = prev_lat, lon = prev_lon,
                                                                   date_start = date_start + months(1) - days(14)) %>% 
                  select(lat, lon, plocation_order, date_start, group_type_graph),
                location_sizes_map_front_prev_uncounted %>% mutate(date_start = date_end)  %>% select(lat, lon, plocation_order, date_start,group_type_graph)
) 

to_maps = to_maps %>% mutate(numeric_group_type_graph =as.numeric(as.factor(group_type_graph))) %>%
  group_by(date_start,lat, lon) %>% arrange(lat, lon,group_type_graph,date_start) %>% 
  mutate(location_order = 1:n() / 50) %>% 
  mutate(new_lat = location_order  + lat ,
         new_lon =  lon,
         Plane_Type = group_type_graph) %>%
  as.data.frame()


to_maps = to_maps %>% mutate(date_start = date_start %m-% months(1))


library(rnaturalearth)
world_map <- ne_countries(scale = 50, returnclass = 'sf')


library(gganimate)


normandy_invasion_movement = ggplot(data = world_map, color = 'grey') + xlim(xlims_normandy) + ylim(ylims_normandy)+
  #annotate("rect", xmin = -5, xmax = 50, ymin = 00, ymax = 30,
  #         alpha = 1,fill = "white")+
  geom_sf() + geom_point(data =   to_maps %>% select( -numeric_group_type_graph, -Plane_Type) %>% 
                                                             mutate(date_start = (date_start)) %>% arrange(date_start) %>% 
                                                             subset(date_start > '1944-05-01' & date_start < '1944-07-10'),
                                                           aes(x = new_lon , y = new_lat  , group = plocation_order, colour = Plane_Type, alpha = .9 , size =.1), inherit.aes = FALSE)  + 
  scale_size(range = c(0,.01), guide = 'none')  + theme_minimal() + #geom_sf(data = world_map, color = 'grey')+ 
  transition_time((date_start )) + exit_disappear()+theme(legend.position = "bottom") +ease_aes('linear', interval = 0.01,state_length = 1)+
  labs(title = 'Luftwaffe Plane Locations during the Battle of Normanday (Month): {frame_time}', x = 'Longitude', y = 'Latitude')+
  scale_alpha(guide = 'none')
  
PLOT_WIDTH = 1000
normandy_invasion_movement = animate(normandy_invasion_movement, 
        duration = 5, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
        fps  =  30, height =PLOT_WIDTH*(ylims_normandy[2]-ylims_normandy[1])/(xlims_normandy[2]-xlims_normandy[1]) , width =PLOT_WIDTH)


anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/normandy_movement.gif", normandy_invasion_movement)




xlims_poland = c(0,25)
ylims_poland = c(46,65)
map_poland <- ggplot() + 
  geom_sf(data = world_map, fill = 'grey') +
  xlim(xlims_poland) + theme_minimal()+ylim(ylims_poland)+
  annotate("rect", xmin = -5, xmax = 50, ymin = 00, ymax = 30,
           alpha =1,fill = "white")

library(gganimate)


poland_invasion_movement = ggplot(data = world_map, color = 'grey') + xlim(xlims_poland) + ylim(ylims_poland)+
  #annotate("rect", xmin = -5, xmax = 50, ymin = 00, ymax = 30,
  #         alpha = 1,fill = "white") +
  geom_sf() + geom_point(data =   to_maps %>% select( -numeric_group_type_graph) %>% 
                           mutate(date_start = (date_start)) %>% arrange(date_start) %>% 
                           subset(date_start > '1939-08-01' & date_start < '1939-12-10'),
                         aes(x = new_lon , y = new_lat  , group = plocation_order, colour = Plane_Type, alpha = .9 , size =.1), inherit.aes = FALSE)  + 
  scale_size(range = c(0,.01), guide = 'none')  + theme_minimal() + #geom_sf(data = world_map, color = 'grey')+ 
  transition_time((date_start )) + exit_disappear()+theme(legend.position = "bottom") +ease_aes('linear', interval = 0.01,state_length = 1)+
  labs(title = 'Luftwaffe Plane Locations During and After Pland Invasion (Month): {frame_time}', x = 'Longitude', y = 'Latitude')+
  scale_alpha(guide = 'none')

PLOT_WIDTH = 1000
poland_invasion_movement = animate(poland_invasion_movement, 
                                     duration = 10, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
                                     fps  =  30, height =PLOT_WIDTH*(xlims_poland[2]-ylims_poland[1])/(xlims_poland[2]-xlims_poland[1]) , width =PLOT_WIDTH)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
luft_pallette = c('#f6bb19','#313722', '#b77d42','#1c2f5b', '#5f7c3a')
luft_pallette = c('#f6bb19', '#1f1917','#ceb267','#1c2f5b', '#cf3110','#b77d42','#1b2010')

get_movement_plots = function(start_date, end_date, xlims, ylims, text_title){
  num_months = round(as.numeric(as.Date(end_date) - as.Date(start_date)) / 30)
  
  
  temp_move_plot = ggplot(data = world_map, color = 'grey') + xlim(xlims) + ylim(ylims)+
    geom_sf() + geom_point(data =   to_maps %>% select( -numeric_group_type_graph) %>% 
                             mutate(date_start = (date_start)) %>% arrange(date_start) %>% 
                             subset(date_start > start_date & date_start < end_date),
                           aes(x = new_lon , y = new_lat  , group = plocation_order, colour = Plane_Type, alpha = .9 , size =.1), inherit.aes = FALSE)  + 
    scale_size(range = c(0,.01), guide = 'none')  + theme_minimal() +
    transition_time((date_start )) + exit_disappear()+theme(legend.position = "bottom") +ease_aes('linear', interval = 0.01,state_length = 1)+
    labs(title = paste(text_title,' (Month): {frame_time}'), x = 'Longitude', y = 'Latitude')+
    scale_alpha(guide = 'none') + theme(text = element_text(size = 20)) + 
    scale_colour_manual(values=luft_pallette)+theme(plot.title = element_text(hjust = 0.5))
  
  PLOT_WIDTH = 1000
  temp_move_plot = animate(temp_move_plot, 
                                     duration = num_months*3, 
                                    fps  =  30, height =PLOT_WIDTH*(ylims[2]-ylims[1])/(xlims[2]-xlims[1]) , width =PLOT_WIDTH)
  
  return(temp_move_plot)
  
}

normandy_invasion = get_movement_plots('1944-05-01','1944-07-10' ,
                                       c(-5,15), c(45,55), 'Luftwaffe Locations \n Battle of Normandy')


poland_invasion = get_movement_plots('1939-08-01','1939-12-10' ,
                          c(0,25), c(46,58), 'Luftwaffe Locations \n During and After the Invasion of Poland')

norway_invasion = get_movement_plots('1940-03-01','1940-05-10' ,
                                     c(0,25), c(46,70), 'Luftwaffe Locations \n Invasion of Norway and Denmark')

france_invasion = get_movement_plots('1940-04-10','1940-07-10' ,
                                     c(-5,25), c(46,70), 'Luftwaffe Locations \n Invasion of France and Low Countries')
battle_of_britain = get_movement_plots('1940-06-10','1940-11-10' ,
                                     c(-5,25), c(46,70), 'Luftwaffe Locations \n Battle of Britain')

invasion_of_greece = get_movement_plots('1941-03-10','1941-06-10' ,
                                       c(-5,30), c(31,70), 'Luftwaffe Locations \n Invasion of Greece and Yugoslavia')



barbarosa = get_movement_plots('1941-05-10','1942-03-10' ,
                               c(-5,42), c(31,70), 'Luftwaffe Locations \n Invasion of Soviet Union')

mid_war = get_movement_plots('1942-03-10' ,'1943-08-10',
                               c(-5,42), c(31,75), 'Luftwaffe Locations \n mid')

anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/normandy_invasion.gif", normandy_invasion)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/poland_movement.gif", poland_invasion)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/norway_invasion.gif", norway_invasion)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/france_invasion.gif", france_invasion)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/battle_of_britain.gif", battle_of_britain)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/invasion_of_greece.gif", invasion_of_greece)
anim_save(filename = "/users/sweiss/src/luftwaffe_locations/plots/barbarosa.gif", barbarosa)





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
lost_to_maps = lost_to_maps %>% mutate(lat = lat-30, new_lat = new_lat-30)
lost_to_maps_1= lost_to_maps %>% mutate(lost_to_maps, new_lat = new_lat/4)
map <- get_googlemap(center = 'Košice', zoom = 4,
                     style = 'feature:administrative.country|element:labels|visibility:off')

animation
animation = ggplot(data = world_map, color = 'grey') +geom_sf() + geom_point(data =   rbind(to_maps %>% select( -numeric_group_type_graph, -Plane_Type), lost_to_maps) %>% 
                                                                               mutate(date_start = (date_start)) %>% arrange(date_start) %>% 
                                                                               subset(date_start > '1942-05-14' & date_start < '1942-12-14'),
                                                                             aes(x = new_lon , y = new_lat  , group = plocation_order, colour = group_type_graph, 
                                                                                 shape = as.factor(group_type_graph), alpha = .5 , size =.1), inherit.aes = FALSE) +
  xlim(-5,50) + theme_minimal()+ylim(30,70)  + #geom_sf(data = world_map, color = 'grey')+ 
  transition_time((date_start )) + exit_disappear()+theme(legend.position = "None") +ease_aes('linear', interval = 0.01,state_length = 1)

PLOT_WIDTH = 1000
temp_move_plot = animate(animation, 
                         duration = 15, 
                         fps  =  30, height =1000 , width =1000)

test =ggplot(data =   rbind(to_maps %>% select( -numeric_group_type_graph, -Plane_Type), lost_to_maps) %>% 
               mutate(date_start = as.Date(date_start)) %>% arrange(date_start) %>% 
               subset(date_start > '1941-08-14' & date_start < '1942-01-14'),
             aes(x = new_lon , y = new_lat  , group = plocation_order, colour = group_type_graph, 
                 shape = as.factor(group_type_graph), alpha = .5 )) +
  geom_point(alpha = 1/50)+scale_size(range = c(0,2))  + xlim(-5,50) + theme_minimal()+ylim(30,70)  - geom_sf(data = world_map, color = 'grey')+
  transition_time(as.Date(date_start)) + exit_disappear()+theme(legend.position = "None") +ease_aes('linear', interval = 0.01,state_length = 1)




animation = ggplot() +  geom_sf(data = world_map, color = 'grey') +
  #ggmap(map) +
  # geom_path(data = filled_in_prev_dates_1  %>% filter(!is.na(lat)) %>% subset(disbanded == 0) %>% subset(date_start > '1941-06-01' & date_start < '1941-11-30') %>% subset(format(date_start, format = "%d") == '01') ,
  # aes(x = lon , y = lat  , group = group_squadron),size=0.05, arrow=arrow(length=unit(0.3, 'cm') )) +
  geom_point(data =  rbind(to_maps %>% select( -numeric_group_type_graph), lost_to_maps) %>% 
               mutate(date_start = as.Date(date_start)) %>% arrange(date_start) %>% 
               subset(date_start > '1944-05-14' & date_start < '1944-08-14')
             ,
             aes(x = new_lon , y = new_lat  , group = plocation_order, colour = group_type_graph, 
                 shape = as.factor(group_type_graph), alpha = .5 ), inherit.aes = FALSE) + 
  geom_point(alpha = 1/10)+scale_size(range = c(0,2)) + 
  #transition_reveal(as.Date(yearmon))
  
  transition_time((date_start)) + exit_disappear()+theme(legend.position = "None") +ease_aes('linear', interval = 0.01,state_length = 1)
#xlim(-5,50) + theme_minimal()+ylim(30,70) 
ggtitle("Date: {frame_along}") 
animate(animation, 
        duration = 10, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
        fps  =  30, height = 2000, width =2000*55/40)


animation = ggmap(map) +
  # geom_path(data = filled_in_prev_dates_1  %>% filter(!is.na(lat)) %>% subset(disbanded == 0) %>% subset(date_start > '1941-06-01' & date_start < '1941-11-30') %>% subset(format(date_start, format = "%d") == '01') ,
  # aes(x = lon , y = lat  , group = group_squadron),size=0.05, arrow=arrow(length=unit(0.3, 'cm') )) +
  geom_point(data = to_maps %>% 
               subset(date_start > '1942-04-14' & date_start < '1944-08-14') %>%
               mutate(yearmon = as.yearmon(date_start)) %>% arrange(date_start),
             aes(x = new_lon , y = new_lat  , group = plocation_order, colour = group_type_graph, shape = as.factor(group_type_graph), alpha = .5 )) + geom_point(alpha = 1/10)+scale_size(range = c(0,2)) +
  transition_time(as.Date(yearmon)) + theme(legend.position = "None")+
  ease_aes('linear', interval = 0.01,state_length = 1) +exit_disappear()+
  ggtitle("Date: {frame_along}") 



