

maps_of_map_fronts_image_magick = lapply(num_aircraft_by_map_front %>% pull(map_front) %>% unique(), function(x) image_read(paste0("/Users/sweiss/Downloads/temp_plots/plot_",x,".png")))

names(maps_of_map_fronts_image_magick) = num_aircraft_by_map_front %>% pull(map_front) %>% unique()



for (q in sort(unique(num_aircraft_by_map_front[,'date_start']))[-c(1:2)]){
  temp_date_end = as.Date(q)
  print(temp_date_end)
  temp_date_start = as.Date(temp_date_end) - months(6)
  
  num_aircraft_by_map_front_subset = num_aircraft_by_map_front %>% subset(date_start <= temp_date_end & date_start >= temp_date_start)
  num_aircraft_by_map_front_subset_subtract = num_aircraft_by_map_front_subset  %>% summarise(min(x_coord_start)) %>% pull() - 125
  num_aircraft_by_map_front_subset = num_aircraft_by_map_front_subset %>%
    mutate(x_coord_end = x_coord_end - num_aircraft_by_map_front_subset_subtract,
           x_coord_start = x_coord_start -num_aircraft_by_map_front_subset_subtract)
  
  
  
  df <- data.frame(x = c(min(c(-2*max(num_aircraft_by_map_front_subset[,'x_coord_end']), -4250)) ,2500),
                   y = c(min(num_aircraft_by_map_front_subset[,'start_height']) *1.5,max(num_aircraft_by_map_front_subset[,'start_height'])*1.5))
  base <- ggplot(df, aes(x, y)) +
    geom_blank() +
    theme_bw()
  
  base <- ggplot(df, aes(x, y)) +
    geom_blank() +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                       axis.text.x=element_blank(),axis.text.y=element_blank()
    )+ xlab('')+ylab('') 
  big_map = image_read(paste0("/Users/sweiss/Downloads/temp_plots/big_plots/",as.Date(q),".png"))
  base = base +  annotation_custom(grob = rasterGrob(image_rotate(big_map, degrees = 270)),
                                   ymin = min(df[,'y']) ,
                                   ymax = max(df[,'y']),
                                   xmin = min(df[,'x']),
                                   xmax = 0)

  
  
  # for(x in names(maps_of_map_fronts_image_magick)){
  #   base = base +  annotation_custom(grob = rasterGrob(image_rotate(maps_of_map_fronts_image_magick[[x]], degrees = 270)),
  #                                    ymin = subset(num_aircraft_by_map_front, date_start == '1940-01-15' & map_front == x)[,'start_height'] -75 ,
  #                                    ymax = subset(num_aircraft_by_map_front, date_start == '1940-01-15' & map_front == x)[,'start_height'] ,
  #                                    xmin = 0,
  #                                    xmax = 100)
  # 
  # 
  # 
  # 
  # 
  # }

  
  for( x in as.numeric(rownames(num_aircraft_by_map_front_subset)) ){
    
    if((num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),'date_start'] > '1939-08-01' ) & (num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)), 'num_aircraft']>0 )){
      print(num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),])
      base = base +  annotation_custom(grob = ggplotGrob(temp_plots_simple[[x]]),
                                       ymin = num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),'start_height'],
                                       ymax = num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),'end_height'],
                                       xmin = num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),'x_coord_start'],
                                       xmax = num_aircraft_by_map_front_subset[which(rownames(num_aircraft_by_map_front_subset) == as.character(x)),'x_coord_end']) 
      
      
    }
    
  }
  
  top_movements_subset = top_movements  %>% subset(date_start <= temp_date_end & date_start >= temp_date_start)
  
  if(nrow(top_movements_subset) > 0 ){
    for( x in 1:nrow(top_movements_subset)){
      
      end_data = subset(num_aircraft_by_map_front_subset, map_front == top_movements_subset[x,'map_front'] & date_start == top_movements_subset[x,'date_start'])
      start_data = subset(num_aircraft_by_map_front_subset, map_front == top_movements_subset[x,'prev_map_front'] & date_start == top_movements_subset[x,'date_start']-months(1))
      if(nrow(start_data) > 0){
        
        y_start = start_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
        x_start = start_data %>% pull(x_coord_end)
        
        y_end = end_data %>% mutate(avg_height = (end_height + start_height)/2 ) %>% pull(avg_height)
        x_end = end_data %>% pull(x_coord_start)
        print(end_data)
        temp_df_start_end = data.frame(y_start, x_start, y_end, x_end, size = top_movements_subset[x,'total_w_approx'])
        base = base +  geom_curve(data = temp_df_start_end,
                                  aes(x = x_start, xend = x_end,
                                      y = y_start, yend = y_end ,
                                      alpha = .1, curvature = .005, size = size/100
                                  ),
                                  arrow = arrow(type="closed",length = unit(0.25,"cm")), curvature = .005
                                  
        )+ scale_size(range = c(0, 2), guide = 'none')
      }
      
      
      
      
      
    }
    
    
  }
  
  date_locs = num_aircraft_by_map_front_subset %>% group_by(date_start) %>% summarise(start_x = min(x_coord_start), end_x = max(x_coord_end)) %>% as.data.frame()
  for(x in 1:nrow(date_locs)){
    base = base + annotation_custom(grob = textGrob(substr(date_locs[x,'date_start'], 1,7), rot = 0, gp=gpar(fontsize=5,fontface="italic")),  xmin = date_locs[x,'start_x'], xmax = date_locs[x,'end_x'], ymin = min(df[,'y']), ymax = min(df[,'y']))
    
    if(as.numeric(substr(date_locs[x,'date_start'], 6,7)) %% 2 == 0){
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
                num_aircraft_by_map_front_subset %>% select(date_start, map_front, end_height, start_height, x_coord_end, x_coord_start),
                by = c('date_start','map_front')
  )
  if(nrow(shoah)>0){
    for(x in 1:nrow(shoah)){
      
      base = base + annotation_custom(grob = textGrob(shoah[x,'Event'], rot = 90,gp=gpar(fontsize=5,fontface="italic", col = "#f8a53c")),  xmin = shoah[x,'x_coord_end']+50, xmax = shoah[x,'x_coord_start']+100, ymin = shoah[x,'end_height'], ymax = shoah[x,'start_height'])
      
      
    }
    
    
  }

  
  date_min_max_heights = num_aircraft_by_map_front_subset%>% subset(num_aircraft > 0) %>% group_by(date_start) %>% summarise(min_height = min(start_height) , max_height = max(end_height))%>% as.data.frame()
  timeline_text = read.csv('data/ww2_chronology.csv')

  timeline_text = merge(timeline_text %>% mutate(date_start = as.Date(date)), date_locs, by = 'date_start')
  timeline_text = merge(timeline_text, date_min_max_heights, by = 'date_start')
  # timeline_text = timeline_text %>% mutate(y_graph_end = ifelse(east == "East", max_height, min_height))
  timeline_text = timeline_text %>% mutate(y_start = ifelse(east == "East", max_height+100, min(df[,'y'])+100 ))
  timeline_text = timeline_text %>% mutate(y_end = ifelse(east == "East", max((df[,'y'])), min_height-100  ))
  
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
  if(nrow(timeline_text) > 0){
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
      # base = base + annotation_custom(grob = textGrob(timeline_text[x,'text'], rot = 90,gp=gpar(fontsize=5)),      xmin = timeline_text[x,'start_x'],
      #                                 xmax = timeline_text[x,'end_x'],
      #                                 ymin = timeline_text[x,'y_start'],
      #                                 ymax = timeline_text[x,'y_end']
      # )
      
    }
    
    
  }
  base = base+theme(legend.position = 'none')
  
  ggsave(base, file=paste0("/Users/sweiss/Downloads/temp_plots/by_date/plot_", 'test_equal_heights_maps_date_subset21234',temp_date_end,".png"), width = max(df[,'x']) - min(df[,'x']), height = max(num_aircraft_by_map_front[,'start_height'])*1.5-min(num_aircraft_by_map_front[,'start_height']) *1.5  *2, units = "px",limitsize = FALSE)
  
  
}
list_of_time_plots = lapply(list.files("/Users/sweiss/Downloads/temp_plots/by_date/"), function(x) image_read(paste0("/Users/sweiss/Downloads/temp_plots/by_date/",x)))



library(animation)
## make sure ImageMagick has been installed in your system
saveGIF({
  for (i in list_of_time_plots) plot(image_rotate(i, degrees = 90))},
  movie.name = 'test.gif', ani.width = 10000, ani.height = 6000)

ggsave(base, file=paste0("/Users/sweiss/Downloads/base", 'test_equal_heights_maps_date_subset21234',temp_date_end,".png"), width = max(df[,'x']) - min(df[,'x']), height = max(num_aircraft_by_map_front[,'start_height'])*1.5-min(num_aircraft_by_map_front[,'start_height']) *1.5  *2, units = "px",limitsize = FALSE)



