grop_type_graph_names = data.frame(group_type_graph = c('Bomber', 'Fighter', 'Ground Attack', 'Night Fighter', 'TE_Fighter'), 
                                   group_type_graph_name = c('B', 'F','GA','NF', 'TEF'))


get_barplot_date_map = function(temp_data){
  
  
  total_aircraft = sum(temp_data[,'total_w_approx'])
  temp_data
  
  temp_data %>% 
    ggplot(aes(x = group_type_graph_name, y = total_w_approx, fill = (loss_ratio)) )+geom_bar(stat = 'identity') + 
    scale_y_continuous(limits=c(0,total_aircraft), expand = c(0, 0)) +
    geom_text(data = temp_data, aes(x = factor(group_type_graph_name),  y=total_w_approx+5, label = group_type_graph_name), 
              position = position_dodge(0.9), size = 2.25, angle = 90,  color = "Black", hjust = 'left')+
  
    geom_point(data = temp_data %>% subset(has_actual_data == 1) ,
               aes( y=loss_ratio*total_w_approx, x=group_type_graph_name), stat ='identity' )+
    
    
    scale_colour_gradient(
      low = "blue",
      high = "red",
      aesthetics = "fill",
      limits = c(0, 2),
      na.value = "grey50"
    ) + ylab(temp_data[1,'map_front_names'])+theme( axis.title.y = element_text(size = 10 ) )+ 
    theme(legend.position="none",axis.ticks.x=element_blank())  +
    theme(text = element_text(size=(5)),legend.position="none",
          axis.ticks.x=element_blank(), plot.margin = margin(), axis.ticks.length.x = unit(0,'pt'),
          rect = element_rect(fill = "transparent"),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA)
    ) + facet_grid(.~Fighter, scales = 'free_x')  +   theme(axis.title.x=element_blank(),
                                                            axis.text.x=element_blank(),
                                                            axis.ticks.x=element_blank())
  
  
  
  
}

library( RColorBrewer)
map <- get_googlemap(center = 'Częstochowa', zoom = 4,maptype= "terrain",
                     style = 'feature:administrative.country|element:labels|visibility:off', scale = 4)

get_map_overall_by_date = function(start_date, data){
  
  
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- c('total',  'approx_total')
  colScale <- scale_colour_manual(name = "variable",values = myColors[1:2])
  
  
  
  
  unique_map_fronts = unique(data[,'map_front'])
  temp_barplots = lapply(unique_map_fronts, function(x) {
    plot = data %>%  subset( date_start ==start_date)%>% subset(map_front == x) %>% get_barplot_date_map
    return(plot)
    
    
  })
  names(temp_barplots) = unique_map_fronts
  
  num_planes_by_map_front = data %>%  subset( date_start ==start_date) %>% group_by(map_front) %>% summarise(total = sum(total_w_approx)) %>% as.data.frame()
  
  map_1 = ggmap(map)
  for(x in unique_map_fronts){
    heights = num_planes_by_map_front %>% 
      subset(map_front == x) %>% pull(total)
    temp_cluster_loc = subset(avg_locs_map_front, map_front == x)
    if(heights>0){
      map_1 = map_1 + inset(ggplotGrob(temp_barplots[[x]] ), xmin = temp_cluster_loc[1,'lon'], xmax = temp_cluster_loc[1,'lon']+4, 
                            ymin = temp_cluster_loc[1,'lat'], ymax = temp_cluster_loc[1,'lat']+heights/100) 
    }
    
  }
  
  temp_top_movements = subset(top_movements, date_start == start_date)
  if(nrow(temp_top_movements) > 0){
    for( x in 1:nrow(temp_top_movements)){
      
      end_data = subset(avg_locs_map_front, map_front == temp_top_movements[x,'map_front'] )
      start_data = subset(avg_locs_map_front, map_front == temp_top_movements[x,'prev_map_front'] )
      
      y_start = start_data %>% pull(lat)
      x_start = start_data %>% pull(lon)
      
      y_end = end_data %>% pull(lat)
      x_end = end_data%>% pull(lon)
      temp_df_start_end = data.frame(y_start, x_start, y_end, x_end, size = temp_top_movements[x,'total_w_approx'])
      
      map_1 = map_1 +  geom_segment(data = temp_df_start_end,
                                    aes(x = x_start, xend = x_end,
                                        y = y_start, yend = y_end  ,
                                        alpha = .02, size = size
                                    ),
                                    arrow = arrow(type="closed",length = unit(0.25/2,"cm"))) + scale_size(range = c(0, 2), guide = 'none')+theme(legend.position="none")
      
      
      
      
    }
    
    
  }
  
  map_1 = map_1 + geom_point(data = location_sizes %>% subset(date_start == start_date) %>% select(lat, lon) %>% unique(), aes(x = lon, y = lat, shape = 7))+scale_shape_identity()
  return(map_1)
  
}

test_map = get_map_overall_by_date('1944-06-15', merge(grop_type_graph_names, location_sizes_map_front_2, by = 'group_type_graph') )+ ggtitle('Luftwaffe Planes by Front')
lapply(unique(location_sizes_map_front_2[,'date_start']), function(x) {
  ggsave( get_map_overall_by_date(x, merge(grop_type_graph_names, location_sizes_map_front_2, by = 'group_type_graph')) + ggtitle(paste('Luftwaffe Planes by Front', (x))), 
          file=paste0("/Users/sweiss/Downloads/temp_plots/big_plots/",x,".png"),width = 5000, height = 5000, units = "px",limitsize = FALSE)    }
)

ggsave(test_map, file=paste0("/Users/sweiss/Downloads/temp_plots/single_big_map",".png"),width = 5000, height = 5000, units = "px",limitsize = FALSE)    



