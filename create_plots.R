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

load('data/luftwaffe_sizes_locations.rdata')

#merge location data with sizes and loss data 
#map_front are geographicial locations where luftwaffe fought


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



location_sizes_map_front = merge(location_sizes_map_front, avg_locs_map_front, by = 'map_front')
location_sizes_map_front_prev = merge(location_sizes_map_front_prev, 
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

#location_sizes_map_front %>%  subset(group_type_graph!='' ) %>% group_by(eastern_front, date_start, group_type_graph) %>% summarise(writeoff = sum(lost_enemy + lost_accident + lost_maintenance)) %>%as.data.frame()%>% ggplot(aes(x = date_start, y=  writeoff, group  = eastern_front, colour = eastern_front)) + geom_line() + facet_grid(group_type_graph~., scales = 'free')
#location_sizes_map_front %>%subset(group_type_graph!='' ) %>% group_by(eastern_front, date_start, group_type_graph) %>% summarise(writeoff = sum(lost_enemy + lost_accident + lost_maintenance)) %>%as.data.frame()%>% ggplot(aes(x = date_start, y=  writeoff, group  = eastern_front, colour = eastern_front)) + geom_line() + facet_grid(group_type_graph~., scales = 'free')



#function to create_barplots on graph
get_barplot_date_map = function(temp_data){
  
  
  total_aircraft = sum(temp_data[,'total_w_approx'])
  temp_data %>% 
    ggplot(aes(x = group_type_graph, y = total_w_approx, fill = (loss_ratio)) )+geom_bar(stat = 'identity') + 
    scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0)) +
    
    geom_point(data = temp_data %>% subset(has_actual_data == 1) ,
               aes( y=loss_ratio*total_w_approx, x=group_type_graph), stat ='identity' )+
    
    
    scale_colour_gradient(
      low = "blue",
      high = "red",
      aesthetics = "fill",
      limits = c(0, .8),
      na.value = "grey50"
    ) + ylab(temp_data[1,'map_front'])+theme( axis.title.x=element_text(size=.5) ,  axis.title.y = element_text(size = .5 ) )+ 
    theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())  +
    theme(text = element_text(size=(5)),legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.margin = margin(), axis.ticks.length.x = unit(0,'pt')#,axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.ticks.length.y = unit(0,'pt')
    )+scale_y_continuous(breaks = 100) + facet_grid(.~Fighter, scales = 'free_x')
  
  
  
}

# get_barplot_date_map_simple = function(temp_data){
# 
# 
#   total_aircraft = sum(temp_data[,'total_w_approx'])
#   temp_data[is.na(temp_data[,'has_actual_data']),'has_actual_data'] = 0
# 
# 
#   temp_data_all = temp_data%>% mutate(approx_loss = loss_ratio* total_w_approx) %>%
#     group_by(Fighter, map_front) %>%
#     summarise( total = sum(total,na.rm = TRUE), total_w_approx = sum(total_w_approx,na.rm = TRUE))%>%
#     as.data.frame()
#   temp_data_actual_data = temp_data %>% subset(has_actual_data == 1) %>% mutate(approx_loss = loss_ratio* total_w_approx) %>%
#     group_by(Fighter, map_front) %>% summarise(  total_w_approx = sum(total_w_approx,na.rm = TRUE), approx_loss = sum(approx_loss,na.rm = TRUE)) %>%
#     mutate(loss_ratio = approx_loss / total_w_approx) %>% as.data.frame()
#   if(nrow(temp_data_actual_data) > 0){
#     temp_data_all = merge(temp_data_all, temp_data_actual_data %>% select(Fighter, map_front, loss_ratio), on = c('Fighter','map_front')) %>%
#       mutate(approx_loss = total_w_approx* loss_ratio)
# 
#     temp_data_all[,'to_remove'] = 0
# 
#   }else{
#     dummy_row = temp_data_all[1,]
#     temp_data_all = rbind(temp_data_all, dummy_row)
#     temp_data_all[,'loss_ratio'] = 1
#     temp_data_all[,'approx_loss'] = 1
#     temp_data_all[1:(nrow(temp_data_all)-1),'loss_ratio'] = NA
#     temp_data_all[1:(nrow(temp_data_all)-1),'approx_loss'] = NA
#     temp_data_all[,'to_remove'] = 0
#     temp_data_all[nrow(temp_data_all),'to_remove'] = 1
#   }
#   temp_data_all %>% subset(to_remove == 0)%>%
#     ggplot(aes(x = Fighter, y = total_w_approx, fill = (loss_ratio)) )+geom_bar(stat = 'identity') +
#     scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0)) +
#      geom_point(data = temp_data_all %>% subset(to_remove == 0) ,
#                                                    aes( y=approx_loss, x=Fighter), stat ='identity' ) +
# 
# 
# 
#     scale_colour_gradient(
#       low = "blue",
#       high = "red",
#       aesthetics = "fill",
#       limits = c(0, .8),
#       na.value = "grey50"
#     ) + ylab(temp_data[1,'map_front'])+theme( axis.text.x=element_text(size=3) ,  axis.title.x = element_text(size = 3/.pt , colour = "red" ) )+
#     theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
# 
# 
# 
# }
get_barplot_date_map_simple = function(temp_data){


  total_aircraft = sum(temp_data[,'total_w_approx'])

  temp_data %>%
    ggplot(aes(x = Fighter, y = total_w_approx, fill = log(loss_ratio)), label = total_w_approx )+geom_bar(stat = 'identity') +
    scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0)) +
    #geom_text(data = temp_data , aes(x = Fighter, y = total_w_approx+20, label = total_w_approx)) + 
    geom_point(data = temp_data  ,
               aes( y=approx_loss, x=Fighter), stat ='identity' ) +



    scale_colour_gradient(
      low = "blue",
      high = "red",
      aesthetics = "fill",
      limits = c(-5, 0.56),
      na.value = "grey50"
    ) + ylab(temp_data[1,'map_front'])+#theme( axis.text.y=element_text(size=1) ,  axis.title.y = element_text(size = 1) )+
    theme(text = element_text(size=(3.5)),legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.margin = margin(), axis.ticks.length.x = unit(0,'pt'),axis.ticks.length.y = unit(0,'pt')#,axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.ticks.length.y = unit(0,'pt')
         )



}

x_coord_white_space = 250
x_coord_graph_space = 250
max_height_graph = 750 /10

#used for locations on graph 
num_aircraft_by_map_front = subset(location_sizes_map_front_2, group_type_graph !='') %>% group_by(map_front, date_start) %>% summarise(num_aircraft = sum(total_w_approx)/10) %>% as.data.frame()
num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(white_space_vertical = 70) %>% arrange(date_start, map_front) %>% group_by(date_start) %>%
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
num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(end_height = height_2 - eastern_german_height) %>% 
  mutate(start_height = end_height - num_aircraft )

num_aircraft_by_map_front = merge(num_aircraft_by_map_front, num_aircraft_by_map_front %>% select(date_start) %>% unique() %>% mutate( date_numeric_factor =  as.numeric(factor(date_start)) ) %>%
                                    mutate(x_coord = x_coord_graph_space) %>%  mutate(x_coord_end = cumsum((x_coord+x_coord_white_space)) ) %>% mutate(x_coord_start = x_coord_end-x_coord_graph_space) %>%
                                    select(date_start, x_coord_end, x_coord_start, date_numeric_factor) , on = 'date_start')

# num_aircraft_by_map_front = merge(num_aircraft_by_map_front %>% mutate(map_front_cont = as.numeric(map_front))  %>% mutate(odd_map_fronts = map_front_cont%%2),
#                                   num_aircraft_by_map_front %>% mutate(map_front_cont = as.numeric(map_front))  %>% mutate(odd_map_fronts = map_front_cont%%2) %>% select(date_start, odd_map_fronts) %>% unique() %>% mutate( date_numeric_factor =  as.numeric(factor(date_start)) ) %>%
#                                     mutate(x_coord = x_coord_graph_space + x_coord_graph_space*odd_map_fronts/2) %>%  mutate(x_coord_end = cumsum((x_coord+x_coord_white_space)) ) %>% mutate(x_coord_start = x_coord_end-x_coord_graph_space) %>%
#                                     select(date_start, x_coord_end, x_coord_start, date_numeric_factor,odd_map_fronts) , on = c('date_start','odd_map_fronts'))


num_aircraft_by_map_front = num_aircraft_by_map_front %>% mutate(x_coord_end = x_coord_end + 1000, x_coord_start = x_coord_start + 1000)

#num_aircraft_by_map_front[which(num_aircraft_by_map_front[,'end_height'] <=0),c('end_height', 'start_height')] = num_aircraft_by_map_front[which(num_aircraft_by_map_front[,'end_height'] <=0),c('end_height', 'start_height')] - 50

elements_to_pivot = data.frame(
  date_start = c('1944-11-15', '1944-12-15','1944-06-15'),
   map_front =  c('eastern_germany', 'western_germany','France')
)

elements_to_pivot = num_aircraft_by_map_front %>% subset(num_aircraft > max_height_graph) %>% select(date_start, map_front)

for(x in 1:nrow(elements_to_pivot)){

  dates_to_shift = which(num_aircraft_by_map_front[,'date_start'] > elements_to_pivot[x,'date_start'])
  num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] = num_aircraft_by_map_front[dates_to_shift,'x_coord_start'] + x_coord_graph_space

  temp_loc = which((num_aircraft_by_map_front[,'date_start'] == elements_to_pivot[x,'date_start'])*(num_aircraft_by_map_front[,'map_front']  == elements_to_pivot[x,'map_front']) == 1)
  num_aircraft_by_map_front[temp_loc,'x_coord_end'] = num_aircraft_by_map_front[temp_loc,'x_coord_end'] + x_coord_graph_space
  num_aircraft_by_map_front[temp_loc,'x_coord_start'] = num_aircraft_by_map_front[temp_loc,'x_coord_start'] + x_coord_graph_space

}


temp_plots = lapply(1:nrow(num_aircraft_by_map_front),

                    function(x){
                      plot = subset(location_sizes_map_front_2, group_type_graph !='') %>%
                        subset(date_start == num_aircraft_by_map_front[x,c('date_start')] & map_front == num_aircraft_by_map_front[x,c('map_front')]) %>%
                        get_barplot_date_map
                      return(plot)

                    }

)

location_sizes_map_front_2_simple = merge(location_sizes_map_front_2 %>% subset(group_type_graph != '') %>% mutate(approx_loss = loss_ratio* total_w_approx) %>%
  group_by(Fighter, map_front, date_start) %>%
  summarise( total = sum(total,na.rm = TRUE), total_w_approx = sum(total_w_approx,na.rm = TRUE))%>%
  as.data.frame(), 
  expand.grid(
       Fighter = c("Fighter", 'Bomberg'),
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


temp_plots_simple = lapply(1:nrow(num_aircraft_by_map_front),

                           function(x){
                             plot = location_sizes_map_front_2_simple %>%
                               subset(date_start == num_aircraft_by_map_front[x,c('date_start')] & map_front == num_aircraft_by_map_front[x,c('map_front')]) %>%
                               get_barplot_date_map_simple
                             return(plot)

                           }

)
# 
# temp_plots_simple = lapply(1:nrow(num_aircraft_by_map_front), 
#                            
#                            function(x){
#                              plot = location_sizes_map_front_2_simple %>% 
#                                subset(date_start == num_aircraft_by_map_front[x,c('date_start')] & map_front == num_aircraft_by_map_front[x,c('map_front')]) %>% 
#                                get_barplot_date_map_simple
#                              return(plot)
#                              
#                            }
#                            
# )
# 


  
  
df <- data.frame(x = c(0,num_aircraft_by_map_front[,'x_coord_end']),
                 y = c(min(num_aircraft_by_map_front[,'start_height']) *1.5,max(num_aircraft_by_map_front[,'start_height'])*1.5))
base <- ggplot(df, aes(x, y)) +
  geom_blank() +
  theme_bw()




for( x in 1:length(temp_plots)){
  if((num_aircraft_by_map_front[x,'date_start'] > '1939-08-01' )& (num_aircraft_by_map_front[x, 'num_aircraft']>0 )){
    base = base +  annotation_custom(grob = ggplotGrob(temp_plots_simple[[x]]),
                                     ymin = num_aircraft_by_map_front[x,'start_height'],
                                     ymax = num_aircraft_by_map_front[x,'end_height'],
                                     xmin = num_aircraft_by_map_front[x,'x_coord_start'],
                                     xmax = num_aircraft_by_map_front[x,'x_coord_end']) 
    
    
  }
  
}


top_movements = (location_sizes_map_front_prev %>%
                   subset(group_type_graph !='' & map_front != prev_map_front & group_type_graph !='') %>%
                   group_by(prev_map_front, map_front, date_start, distance_to_e_germany_plot_prev, distance_to_e_germany_plot) %>%
                   summarise(total_w_approx = sum(total_w_approx)) %>% as.data.frame() %>%
                   mutate(curve_heights = sign( distance_to_e_germany_plot_prev - distance_to_e_germany_plot)*total_w_approx ))  %>% subset(total_w_approx >60)



for( x in 1:nrow(top_movements)){
  
  end_data = subset(num_aircraft_by_map_front, map_front == top_movements[x,'map_front'] & date_start == top_movements[x,'date_start'])
  start_data = subset(num_aircraft_by_map_front, map_front == top_movements[x,'prev_map_front'] & date_start == top_movements[x,'date_start']-months(1))
  
  y_start = start_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
  x_start = start_data %>% pull(x_coord_end)
  
  y_end = end_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
  x_end = end_data %>% pull(x_coord_start)
  temp_df_start_end = data.frame(y_start, x_start, y_end, x_end, size = top_movements[x,'total_w_approx'])
  map_front_adjustment_y = (max(as.numeric(num_aircraft_by_map_front[,'map_front'])) - as.numeric(start_data[,'map_front'])) /max(as.numeric(num_aircraft_by_map_front[,'map_front']))*(max_height_graph/2)
  print(map_front_adjustment_y)
  base = base +  geom_curve(data = temp_df_start_end,
                            aes(x = x_start, xend = x_end,
                                y = y_start, yend = y_end + map_front_adjustment_y ,
                                alpha = .02, curvature = .005, size = size
                            ),
                            arrow = arrow(type="closed",length = unit(0.25/2,"cm")), 
                            curvature = .005) + scale_size(range = c(0, 2), guide = 'none')
  
  
  
  
}



#add dates 
date_locs = num_aircraft_by_map_front %>% group_by(date_start) %>% summarise(start_x = min(x_coord_start), end_x = max(x_coord_end)) %>% as.data.frame() 
for(x in 1:nrow(date_locs)){
  
  base = base + annotation_custom(grob = textGrob(substr(date_locs[x,'date_start'], 1,7), rot = 0),  xmin = date_locs[x,'start_x'], xmax = date_locs[x,'end_x'], ymin = -1000, ymax = -1000)
  if(as.numeric(substr(date_locs[x,'date_start'], 6,7)) %% 2 ==0){
    #base = base + geom_segment(x = date_locs[x,'start_x'], xend = date_locs[x,'end_x'], y = 0, yend = 0)
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

  base = base + annotation_custom(grob = textGrob(shoah[x,'Event'], rot = 90),  xmin = shoah[x,'x_coord_start'], xmax = shoah[x,'x_coord_start']+200, ymin = shoah[x,'end_height'], ymax = shoah[x,'start_height'])


}


date_min_max_heights = num_aircraft_by_map_front%>% subset(num_aircraft > 0) %>% group_by(date_start) %>% summarise(min_height = min(start_height) , max_height = max(end_height))%>% as.data.frame()
timeline_text = read.csv('data/ww2_chronology.csv')
timeline_text = merge(timeline_text %>% mutate(date_start = as.Date(date)), date_locs, by = 'date_start')
timeline_text = merge(timeline_text, date_min_max_heights, by = 'date_start')
timeline_text = timeline_text %>% mutate(y_graph_end = ifelse(east == "East", max_height, min_height))
timeline_text = timeline_text %>% mutate(y_start = ifelse(east == "East", y_graph_end, y_graph_end - 1000 ))
timeline_text = timeline_text %>% mutate(y_end = ifelse(east == "East", y_graph_end + 1000, y_graph_end  ))

# 
# for(x in 1:nrow(timeline_text)){
# 
#   base = base + annotation_custom(
#                                   grob = textbox_grob(timeline_text[x,'text']),  xmin = timeline_text[x,'x_coord_start'],
#                                   xmax = timeline_text[x,'x_coord_start']+200, ymin = timeline_text[x,'y_start'], ymax = timeline_text[x,'y_end']
#                                   )
# 
# 
# }
library(gridtext)

for(x in 1:nrow(timeline_text)){
  
  base = base + annotation_custom(
    grob = textbox_grob(timeline_text[x,'text'], hjust = 1, vjust = 0, halign = 0.5, valign = 0.5,
                    width = unit(2, "inches"), height = unit(1, "inches"),
                    orientation = "left-rotated"#, box_gp = gpar(col = "black"),
                    ,gp=gpar(fontsize=8,fontface="bold")
                    )   ,  
    xmin = timeline_text[x,'start_x'],
    xmax = timeline_text[x,'end_x'], 
    ymin = timeline_text[x,'y_start'], 
    ymax = timeline_text[x,'y_end']
  )
  
}


ggsave(base, file=paste0("/Users/sweiss/Downloads/temp_plots/temp5",".png"),width = 25000, height = 3000, units = "px",limitsize = FALSE)
ggsave(base, file=paste0("/Users/sweiss/Downloads/temp_plots/plot_souther_germany",".png"),width = 30, height = 15, units = "in",dpi = 600*2,limitsize = FALSE)

ggsave(base, file=paste0("/Users/sweiss/Downloads/temp_plots/plot_souther_germany",".png"), dpi = 300)

ggsave(temp_plots[[56]], file=paste0("/Users/sweiss/Downloads/temp_plots/plot_souther_germany",".png"),  units = "px", dpi = 300)


ggsave(temp_plots[[56]], file=paste0("/Users/sweiss/Downloads/temp_plots/plot_souther_germany",".png"), width = 500, height = 1000, units = "px",limitsize = FALSE)


