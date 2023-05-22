library(dplyr)
library(zoo)
library(gtools)
library(ggplot2)
library(lubridate)
library(reshape)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpomological)
#claims_east = read.csv('/users/sweiss/downloads/luftwaffe_claims/claims_east.txt')
#claims_west = read.csv('/users/sweiss/downloads/claims_west.txt')
claims_east = read.csv('/users/sweiss/src/luftwaffe_locations/data/claims_east.txt')
claims_west = read.csv('/users/sweiss/src/luftwaffe_locations/data/claims_west.txt')

claims_east = mutate(claims_east, front = 'east')
claims_west = mutate(claims_west, front = 'west')

df = smartbind(claims_east, claims_west)
colnames(df)[c(3:4, 6, 7, 9)] = c('first_name','last_name','unit','aircraft','victory_time' )
df[,'dates'] = dmy(unlist(lapply(strsplit(df[,'Date'], ' '), function(x) x[[1]])))
#remove duplicates
df = unique(df)
df[,'yearmon'] = as.yearmon(df[,'dates'])
df = mutate(df, full_name = paste0(first_name, last_name))
df[,'full_name'] = tolower(iconv(df[,'full_name'],to="ASCII//TRANSLIT"))

#number of claims by time per full_name
df = df %>% arrange(full_name,dates)
df[,'counts'] = 1:nrow(df)
df = df %>% group_by(full_name)%>% mutate(rank = dense_rank(counts)) %>% as.data.frame()

#
max_date = df %>% group_by(full_name) %>% 
  filter(dates == max(dates))%>% mutate(max_date = dates)%>%select(full_name, max_date)  %>% distinct() %>%as.data.frame()

df = merge(df, max_date, by = c('full_name'), all.x = TRUE)
df = mutate(df, date_diff = max_date - dates)

min_date = df %>% group_by(full_name) %>% 
  filter(dates == min(dates))%>% mutate(min_date = dates)%>%select(full_name, min_date)  %>% distinct() %>%as.data.frame()

df = merge(df, min_date, by = c('full_name'), all.x = TRUE)

lw_aces = read.csv('/users/sweiss/downloads/lw_aces_text.txt')
lw_aces = mutate(lw_aces, full_name = paste0(First.Name, Pilot.Name))

test = lapply(lw_aces[,'Remarks'], function(q) na.omit(do.call(c,(lapply(strsplit(utf8::utf8_encode(q),' '), function(D) lubridate::dmy(D) )))))
test_1 = lapply(test, function(x) unlist(x[1]))
test_1[sapply(test_1, is.null)] = NA
test_1 = unlist(test_1)

lw_aces[,'date'] = as.Date(test_1, origin="1869-12-31")
lw_aces[,'status'] = 'none'
lw_aces[grep('KIA',lw_aces[,'Remarks']),'status'] = 'KIA'
lw_aces[grep('KILLED',lw_aces[,'Remarks'], ignore.case = TRUE),'status'] = 'KILLED'
lw_aces[grep('MIA',lw_aces[,'Remarks']),'status'] = 'MIA'
lw_aces[grep('POW',lw_aces[,'Remarks']),'status'] = 'POW'
lw_aces[grep('KIFA',lw_aces[,'Remarks']),'status'] = 'KIFA'
lw_aces[grep('Injured',lw_aces[,'Remarks']),'status'] = 'Injured'
lw_aces[grep('WIA',lw_aces[,'Remarks']),'status'] = 'WIA'
lw_aces[grep('KIC',lw_aces[,'Remarks']),'status'] = 'KIC'
lw_aces[grep('WIFA',lw_aces[,'Remarks']),'status'] = 'WIFA'
lw_aces[grep('Died',lw_aces[,'Remarks']),'status'] = 'Died'
lw_aces[grep('Surrender',lw_aces[,'Remarks']),'status'] = 'Surrender'
lw_aces[grep('suicide',lw_aces[,'Remarks']),'status'] = 'suicide'


lw_aces[,'full_name'] = tolower(iconv(lw_aces[,'full_name'],to="ASCII//TRANSLIT"))

test_2 = lapply(strsplit(lw_aces[,'Jagdgeschwader'], ','), function(x) (x[length(x)]))
test_2 = lapply(test_2, function(x) unlist(x[1]))
test_2[sapply(test_2, is.null)] = NA

lw_aces[,'last_jg'] = unlist(test_2)

lw_aces_1 = lw_aces[,c('full_name','date','status')]
colnames(lw_aces_1) = c('full_name','status_date','status')

#remove duplicate names. should find match better but its only ~66 names lost.

lw_aces_1 = lw_aces_1 %>% filter(!full_name %in% lw_aces_1[which(duplicated(lw_aces_1[,'full_name'])),'full_name'])


lw_aces_1[which(as.numeric(lw_aces_1[,'status_date'])< -30000),'status_date'] = lw_aces_1[which(as.numeric(lw_aces_1[,'status_date'])< -30000),'status_date']+years(100)
lw_aces_1[which(is.na(lw_aces_1[,'status_date'])),'status_date'] = '1945-05-08'
df_1 = merge(df, lw_aces_1, by = 'full_name', all.x = TRUE)

df_1[is.na(df_1[,'dates']),'dates'] = as.Date('1945-06-01')


#nag close recon

df_1[,'aircraft'] = gsub(' *', '',df_1[,'aircraft'])
df_1[,'aircraft'] = gsub('*', '',df_1[,'aircraft'])
df_1[,'aircraft'] = gsub('*', '',df_1[,'aircraft'])

df_1[grep('B-17',df_1[,'aircraft']),'aircraft'] = 'B-17'
df_1[grep('Fortress',df_1[,'aircraft']),'aircraft'] = 'B-17'
df_1[grep('Liberator',df_1[,'aircraft']),'aircraft'] = 'B-24'
df_1[grep('P-47',df_1[,'aircraft']),'aircraft'] = 'P-47'
df_1[grep('B-24',df_1[,'aircraft']),'aircraft'] = 'B-24'
df_1[grep('P-51',df_1[,'aircraft']),'aircraft'] = 'P-51'
df_1[grep('P-38',df_1[,'aircraft']),'aircraft'] = 'P-38'
df_1[grep('P-25',df_1[,'aircraft']),'aircraft'] = 'P-25'
df_1[grep('Il-2',df_1[,'aircraft']),'aircraft'] = 'Il-2'
df_1[grep('Spitfire',df_1[,'aircraft']),'aircraft'] = 'Spitfire'
df_1[grep('Hurric',df_1[,'aircraft']),'aircraft'] = 'Hurricane'
df_1[grep('P-40',df_1[,'aircraft']),'aircraft'] = 'P-40'
df_1[grep('Airacobra',df_1[,'aircraft']),'aircraft'] = 'P-39'
df_1[grep('P-39',df_1[,'aircraft']),'aircraft'] = 'P-39'
df_1[grep('Lancaster',df_1[,'aircraft']),'aircraft'] = 'Lancaster'
df_1[grep('Mig',df_1[,'aircraft']),'aircraft'] = 'Mig'
df_1[grep('Yak',df_1[,'aircraft']),'aircraft'] = 'Yak'
df_1[grep('Boston',df_1[,'aircraft']),'aircraft'] = 'Boston'
df_1[grep('Jak',df_1[,'aircraft']),'aircraft'] = 'Jak'
df_1[grep('Mustang',df_1[,'aircraft']),'aircraft'] = 'P-51'
df_1[grep('LaGG',df_1[,'aircraft']),'aircraft'] = 'LaGG'
df_1[grep('Typhoon',df_1[,'aircraft']),'aircraft'] = 'Typhoon'
df_1[grep('B-26',df_1[,'aircraft']),'aircraft'] = 'B-26'
df_1[grep('Thunderb',df_1[,'aircraft']),'aircraft'] = 'P-47'
df_1[grep('Spif',df_1[,'aircraft']),'aircraft'] = 'Spitfire'
df_1[grep('Wellington',df_1[,'aircraft']),'aircraft'] = 'Wellington'

df_1[,'heavy']= 0
df_1[grep('B-24',df_1[,'aircraft']),'heavy']=1
df_1[grep('B-17',df_1[,'aircraft']),'heavy']=1


df_1[,'gruppe'] = ''
df_1[,'gruppe'] = ''
df_1[grep('1[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('2[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('3[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('4[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('5[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('6[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('7[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('8[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('9[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('10[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('11[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('12[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('13[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('14[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('15[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('16[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('I',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "III"

df_1[grep('1[.]',df_1[,'Unit']),'gruppe_unit'] = 1
df_1[grep('2[.]',df_1[,'Unit']),'gruppe_unit'] = 2
df_1[grep('3[.]',df_1[,'Unit']),'gruppe_unit'] = 3
df_1[grep('4[.]',df_1[,'Unit']),'gruppe_unit'] = 4
df_1[grep('5[.]',df_1[,'Unit']),'gruppe_unit'] = 5
df_1[grep('6[.]',df_1[,'Unit']),'gruppe_unit'] = 6
df_1[grep('7[.]',df_1[,'Unit']),'gruppe_unit'] = 7
df_1[grep('8[.]',df_1[,'Unit']),'gruppe_unit'] = 8
df_1[grep('9[.]',df_1[,'Unit']),'gruppe_unit'] = 9
df_1[grep('10[.]',df_1[,'Unit']),'gruppe_unit'] = 10
df_1[grep('11[.]',df_1[,'Unit']),'gruppe_unit'] = 11
df_1[grep('12[.]',df_1[,'Unit']),'gruppe_unit'] = 12
df_1[grep('13[.]',df_1[,'Unit']),'gruppe_unit'] = 13
df_1[grep('14[.]',df_1[,'Unit']),'gruppe_unit'] = 14
df_1[grep('15[.]',df_1[,'Unit']),'gruppe_unit'] = 15
df_1[grep('16[.]',df_1[,'Unit']),'gruppe_unit'] = 16

df_1[,'group_type'] = ''
df_1[grep('JG', df_1[,'unit']),'group_type'] = 'Jagdgeschwader'
df_1[grep('NJG', df_1[,'unit']),'group_type'] = 'Nachtjagdgeschwader'
df_1[grep('ZG', df_1[,'unit']),'group_type'] = 'Zerstörergeschwader'

#df_1[,'JG'] = df_1[,'unit']
#df_1[,'JG'] = gsub('NJG', 'Nachtjagdgeschwader',df_1[,'JG'])
#df_1[,'JG'] = gsub('JG', 'Jagdgeschwader',df_1[,'JG'])
#df_1[,'JG'] = gsub('ZG', 'Zerstörergeschwader',df_1[,'JG'])

df_1[,'group_number'] = as.numeric(readr::parse_number(df_1[,'unit']))




df_1[,'index'] = 1:nrow(df_1)
altitude_1 = sapply(strsplit(df_1[,'Location'], ':'), function(x){
  
  if(length(x) > 1){ return(x[2])}else{return('')}
    
})
altitude_1 = sapply(strsplit(altitude_1, '-'), function(x){
  
return(x[1])
})
df_1 = df_1 %>% mutate(estimated_altitude = as.numeric(gsub("\\D", "", altitude_1)))
df_1 = df_1 %>% mutate(estimated_altitude = ifelse(estimated_altitude > 10000,NA,estimated_altitude))

df_1 %>% subset(aircraft %in% c("B-17", "B-24", "P-51", "P-47",'P-38')) %>% subset(dates > '1943-12-01' & dates < '1944-02-01') %>% 
  ggplot(aes(x = dates, y = estimated_altitude, colour = aircraft)) + geom_point()

df_1 %>% subset(aircraft %in% c("B-17", "B-24", "P-51", "P-47",'P-38')) %>% subset(dates > '1943-12-01' & dates < '1944-02-01') %>% 
  ggplot(aes(x = dates, y = estimated_altitude, colour = aircraft)) + geom_point()+facet_grid(aircraft~., scales = 'free') + geom_smooth()

min_max_pilot_data = df_1 %>% group_by(full_name) %>% summarise(min_date = min(dates), max_date= max(dates)) %>% as.data.frame()
min_max_pilot_data = min_max_pilot_data %>% mutate(full_name = factor( min_max_pilot_data[,'full_name'], levels = min_max_pilot_data[,'full_name'][order(min_max_pilot_data[,'min_date'])]))
# min_max_pilot_data %>% subset(min_date > '1943-06-1' ) %>% ggplot(aes(x = min_date, xend = max_date, y = full_name, yend = full_name, colour = as.numeric((as.numeric(max_date-min_date))>70))) + geom_segment()+
#   theme_update(axis.ticks.y = element_blank(),
#                axis.text.y = element_blank()) + 
#   scale_color_gradient(high = 'blue', low = 'red')
# 
# 
# df_1 %>% mutate(full_name = factor( full_name, levels = df_1 %>% group_by(full_name) %>% slice(which.min(dates)) %>% arrange(dates) %>% pull(full_name)))%>%
#   subset(dates > '1943-06-1' & dates < '1944-09-01') %>%
#   ggplot(aes(x = (dates),y = as.factor(full_name), color = front)) + geom_point(size = .1)+ geom_line(size = .1)+ geom_point(aes(x = max_date, y = full_name, shape = as.factor(2)))+
#   theme_update(axis.ticks.y = element_blank(),
#                axis.text.y = element_blank()) #+ 
# scale_color_gradient(high = 'blue', low = 'red')




enter_exit_luftwaffe_plot = min_max_pilot_data%>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>% 
  group_by(min_date_month, max_date_month) %>% summarise(counts = n()) %>%as.data.frame() %>%
  ggplot(aes(x = min_date_month, y = max_date_month, colour = counts, size = counts)) + geom_point(shape = 4, alpha = .5) +
  scale_colour_gradient(low = 'blue', high = 'red')+
  #scale_size(c(,2)) + 
  theme_bw() + 
  theme_pomological() + 
  theme(legend.position="bottom",text = element_text(size = 15))+
  xlab('Month Pilot Claimed First Aerial Victory') + 
  ylab('Month Pilot Claimed Last Aerial Victory') + 
  ggtitle('Number of Luftwaffe Pilots for Entering and Exiting Dataset by Month')

enter_exit_luftwaffe_plot_by_pilot = min_max_pilot_data %>%as.data.frame() %>%
  ggplot(aes(x = min_date, y = max_date)) + geom_point(alpha = .1, shape = 4, colour = 'red', position = 'jitter') +
  #scale_colour_gradient(low = 'blue', high = 'red')+
  #scale_size(c(,2)) + 
  #theme_bw(  ) + 
  theme_pomological() + 

  theme(legend.position="bottom",text = element_text(size = 15))+
  xlab('Date Pilot Claimed First Aerial Victory') + 
  ylab('Date Pilot Claimed Last Aerial Victory') + 
  ggtitle('First Claim Date by Last Claim Date for All Luftwaffe Pilots')


min_max_pilot_data %>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>%
  group_by(min_date_month)%>% summarise(counts = n()) %>% as.data.frame()%>%
  ggplot(aes(x = min_date_month , y= as.numeric(counts))) + geom_line()+
  scale_y_continuous() 


min_max_pilot_data %>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>%
  group_by(max_date_month)%>% summarise(counts = n()) %>% as.data.frame()%>%
  ggplot(aes(x = max_date_month , y= counts)) + geom_line()

df_1 %>% group_by(yearmon) %>% summarise(counts = n()) %>% as.data.frame() %>%  
  ggplot(aes(x = yearmon, y = counts)) + geom_line()

df_1 %>% group_by(yearmon, front) %>% summarise(counts = n()) %>% as.data.frame() %>%  
  ggplot(aes(x = yearmon, y = counts, fill = front)) + geom_bar(stat = 'identity', position = 'stack')

#df_1 %>% group_by(aircraft) %>% summarise(counts = n()) %>% arrange(-counts) %>% as.data.frame() %>% 
# write.csv(file = '/users/sweiss/downloads/luftwaffe_kills_by_plane_type.csv')
planes_by_nation = read.csv('/users/sweiss/src/luftwaffe_locations/data/luftwaffe claims by plane type.csv')
planes_by_nation = planes_by_nation %>% mutate(Belligerent = ifelse(Belligerent == 'US', 'USA',Belligerent))
planes_by_nation = planes_by_nation %>% mutate(Belligerent = ifelse(Belligerent %in% c('USA', "UK", "USSR"), Belligerent, 'Other_NA'))

df_1 = merge(df_1, planes_by_nation %>% select(-counts), by = 'aircraft', all.x = TRUE) 


library(ggpomological)

major_timelines = data.frame(name = c('Battle of France', 'Battle of Britain', 'Invasion of Russia', 'To Stalingrad',
                                      'Kursk / Sicily ', 'Battle Over Germany', 'Normandy',
                                      'Battle of the Bulge'),
                             start_date = c('1940-05-01', '1940-09-01','1941-07-22','1942-08-01', '1943-07-01', '1943-12-01',
                                            '1944-07-01','1944-12-01')
                             
                             ) %>% mutate(yearmon = as.yearmon(start_date))


major_timelines = merge(major_timelines,  df_1  %>%group_by(yearmon ) %>% summarise(counts = n()) %>% as.data.frame() , 
                        by = c('yearmon'), all.x = TRUE
                        )

major_timelines[which(major_timelines[,'name'] == 'Battle Over Germany'),'counts'] = 100
library(ggrepel)

claim_by_county =  
  ggplot() + geom_bar(data = df_1 %>% mutate(front_bell = paste(front,  Belligerent) ) %>%group_by(yearmon, Belligerent ) %>% summarise(counts = n()) %>% as.data.frame() %>%  
                        mutate(Belligerent = ifelse(Belligerent %in% c('USA', "UK", "USSR"), Belligerent, 'Other_NA')), aes(x = yearmon, y = counts, fill = Belligerent), stat = 'identity', position = 'stack') + 
  theme_pomological() + 
  geom_label_repel(data = major_timelines, aes(y = counts+100, x = as.yearmon(start_date), label = name), fill = 'light grey')+
  geom_line(aes(y = -10, x = as.yearmon(c('1943-09-01', '1944-06-01'))), size = 2) + 
  # geom_rect(mapping=aes(xmin=as.yearmon('1943-09-01'), xmax=as.yearmon('1944-06-01'),
  #                       ymin=500, ymax=2500), alpha = 0, fill = "light Grey", colour = 'black') + 
  theme(legend.position = 'bottom',text = element_text(size = 15))+
  xlab('Month') + ylab('Number of Claims') +
  ggtitle("Number of Luftwaffe Claims by Month and Belligerent") 

enter_exit_luftwaffe_plot_by_pilot = 
  ggplot() + geom_point(data = min_max_pilot_data %>%as.data.frame(),aes(x = min_date, y = max_date), alpha = .1, shape = 4, colour = 'red', position = 'jitter') +
  #scale_colour_gradient(low = 'blue', high = 'red')+
  #scale_size(c(,2)) + 
  #theme_bw(  ) + 
  theme_pomological() + 
  
  theme(legend.position="bottom",text = element_text(size = 15))+
  xlab('Date Pilot Claimed First Aerial Victory') + 
  ylab('Date Pilot Claimed Last Aerial Victory') + 
  ggtitle('First Claim Date by Last Claim Date for All Luftwaffe Pilots')


enter_exit_luftwaffe_plot_by_pilot = enter_exit_luftwaffe_plot_by_pilot + geom_text_repel(data = major_timelines, aes(y = as.Date('1939-09-1'), x = as.Date(start_date) , label = name))  
enter_exit_luftwaffe_plot_by_pilot


claim_by_front = df_1 %>% mutate(front_bell = paste(front,  Belligerent) ) %>%group_by(yearmon, front ) %>% summarise(counts = n()) %>% as.data.frame() %>%  
 # mutate(Belligerent = ifelse(Belligerent %in% c('USA', "UK", "USSR"), Belligerent, 'Other_NA')) %>% 
  ggplot(aes(x = yearmon, y = counts, fill = front)) + geom_bar(stat = 'identity', position = 'stack') + 
  theme_pomological() + 
  theme(legend.position = 'bottom',text = element_text(size = 15)) +
  xlab('Month') + ylab('Number of Claims') +
  ggtitle("Number of Claims by Month by Front") 


df_1[which(df_1[,'aircraft'] %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')),]%>% 
  subset(dates > '1943-01-01')  %>% group_by(yearmon, aircraft) %>%summarise(counts = n()) %>% 
  as.data.frame() %>% 
  mutate(new_counts = counts*((aircraft == 'B-17')+(aircraft == 'B-24')) - counts*((aircraft == 'P-51')+(aircraft == 'P-47')+(aircraft == 'P-38' )) )  %>% 
  ggplot(aes(x = yearmon, y= counts, fill = aircraft)) + geom_bar(stat = 'identity') 

df_1%>% 
  subset(dates > '1943-01-01')  %>% group_by(yearmon, aircraft) %>%summarise(counts = n()) %>% 
  as.data.frame() %>% 
  mutate(new_counts = counts*((aircraft == 'B-17')+(aircraft == 'B-24')) - counts*((aircraft == 'P-51')+(aircraft == 'P-47')+(aircraft == 'P-38' )) )  %>% 
  ggplot(aes(x = yearmon, y= counts, fill = aircraft)) + geom_bar(stat = 'identity') 


library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
usaaf_victory_credits = read.csv('/Users/sweiss/Downloads/USAAF_Victories/USAAF Victory Credits WWII-Table 1.csv')
usaaf_victory_credits_adj = usaaf_victory_credits[-c(1:4),-1]
colnames(usaaf_victory_credits_adj) = usaaf_victory_credits[4,-1]
usaaf_victory_credits_adj[,'dates'] = as.Date(usaaf_victory_credits_adj[,'Date'], '%m/%d/%y') - years(100)
usaaf_victory_credits_adj = usaaf_victory_credits_adj %>% mutate(yearmonth = as.yearmon(usaaf_victory_credits_adj[,'dates']))

grouped_kills_date_theater = usaaf_victory_credits_adj %>% group_by(yearmonth,Theater) %>% summarise(credits = sum(as.numeric(as.character(Credit)), na.rm = TRUE))  %>% as.data.frame() 

#grouped_kills_date_theater %>% ggplot(aes(x = yearmonth, y = credits, group = Theater, color = Theater)) + geom_line()

grouped_kills_date_europe = usaaf_victory_credits_adj %>% filter(Theater %in% c('MTO', 'ETO')) %>% group_by(yearmonth) %>% summarise(credits = sum(as.numeric(as.character(Credit)), na.rm = TRUE))  %>% as.data.frame() 
usaaf_kills = usaaf_victory_credits_adj %>% filter(Theater %in% c('MTO', 'ETO'))
usaaf_kills = usaaf_kills %>% arrange(dates) %>% mutate(temp_index = 1) %>% group_by(SN) %>% 
  mutate(num_credits = cumsum(Credit)) %>% as.data.frame() %>% 
  select(-temp_index)


df_1 = df_1 %>% arrange(dates) %>% mutate(temp_index = 1) %>% group_by(full_name) %>% 
  mutate(num_credits = cumsum(temp_index)) %>% as.data.frame() %>% 
  select(-temp_index)


usaaf_victory_credits_adj %>% group_by(Unit_Echelon) %>% summarise(Credit = sum(as.numeric(Credit), na.rm = TRUE)) %>% arrange(Credit)
# 
# temp_date = as.Date('1944-03-01')
# num_pilots_w_x_credits_and_recent_claim = function(data, temp_date, num_credit_value, time_lag_value){
#   subset_at_least_x_creds = data %>% subset(num_credits  >= num_credit_value)
#   subset_at_least_x_creds = subset_at_least_x_creds  %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 )
#   pilot_names = subset_at_least_x_creds %>% pull(SN) %>% unique() 
#   pilot_names_data = data %>% subset(SN %in% pilot_names) %>% group_by(SN) %>% filter(dates == min(dates)) %>% as.data.frame() %>% mutate(date_diff = temp_date - dates) %>% summarise(mean_diff_date = mean(date_diff), sd_diff_date = sd(date_diff), counts = n())
#   
#   return(pilot_names_data)
# }
# 
# num_pilots_w_x_credits_and_recent_claim_luftwaffe = function(data, temp_date, num_credit_value, time_lag_value){
#   subset_at_least_x_creds = data %>% subset(num_credits  >= num_credit_value)
#   subset_at_least_x_creds = subset_at_least_x_creds %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 )
#   pilot_names = subset_at_least_x_creds %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>%
#     pull(full_name) %>% unique() 
#   pilot_names_data = data %>% subset(full_name %in% pilot_names)  %>% group_by(full_name) %>% filter(dates == min(dates)) %>% as.data.frame() %>% mutate(date_diff = temp_date - dates) %>% summarise(mean_diff_date = mean(date_diff), sd_diff_date = sd(date_diff), counts = n())
#   return(pilot_names_data)
# }
# 

num_pilots_w_x_credits_and_recent_claim_dists = function(data, temp_date, num_credit_value, time_lag_value){
  #subset_at_least_x_creds = data %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
  #  filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
  #  mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
  #  summarise(counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds_pilots = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10, 500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds,subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  
  return(to_return)
}

num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_usaaf = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds_pilots = data %>% filter(Belligerent %in% 'USA') %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% filter(Belligerent %in% 'USA') %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  return(to_return)
}

num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds_pilots = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  return(to_return)
}





#aces_w_kills_last_30_days_usaaf = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim(usaaf_kills, temp_date, 5 , 90))
#aces_w_kills_last_30_days_usaaf = do.call(rbind,aces_w_kills_last_30_days_usaaf) %>% as.data.frame()
#aces_w_kills_last_30_days_usaaf[,'date'] = seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day')
#aces_w_kills_last_30_days_usaaf_melt = melt(aces_w_kills_last_30_days_usaaf, id.vars = 'date')

#aces_w_kills_last_30_days_usaaf_melt %>% ggplot(aes(x = date, y = value)) + geom_point() + facet_grid(variable~.,scale = 'free')

#aces_w_kills_last_30_days_luftwaffe = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe(df_1, temp_date, 5, 45))
#aces_w_kills_last_30_days_luftwaffe = do.call(rbind, aces_w_kills_last_30_days_luftwaffe)
#aces_w_kills_last_30_days_luftwaffe[,'date'] = seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day')
#aces_w_kills_last_30_days_luftwaffe_melt = melt(aces_w_kills_last_30_days_luftwaffe, id.vars = 'date')
#aces_w_kills_last_30_days_luftwaffe_melt %>% ggplot(aes(x = date, y = value)) + geom_point() + facet_grid(variable~.,scale = 'free')


aces_w_kills_last_30_days_luftwaffe_dists = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists(df_1, temp_date, 5, 30))
aces_w_kills_last_30_days_luftwaffe_dists = do.call(rbind, aces_w_kills_last_30_days_luftwaffe_dists)
aces_w_kills_last_30_days_luftwaffe_dists = aces_w_kills_last_30_days_luftwaffe_dists %>% mutate(airforce = 'luftwaffe')

aces_w_kills_last_30_days_luftwaffe_dists_usaaf = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_usaaf(df_1, temp_date, 5, 30))
aces_w_kills_last_30_days_luftwaffe_dists_usaaf = do.call(rbind, aces_w_kills_last_30_days_luftwaffe_dists_usaaf)
aces_w_kills_last_30_days_luftwaffe_dists_usaaf = aces_w_kills_last_30_days_luftwaffe_dists_usaaf %>% mutate(airforce = 'luftwaffe_only_usaaf')


aces_w_kills_last_30_days_usaaf_dists = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_dists(usaaf_kills, temp_date, 5, 30))
aces_w_kills_last_30_days_usaaf_dists = do.call(rbind, aces_w_kills_last_30_days_usaaf_dists)
aces_w_kills_last_30_days_usaaf_dists = aces_w_kills_last_30_days_usaaf_dists %>% mutate(airforce = 'usaaf')


#aces_w_kills_last_30_days_usaaf_dists %>% ggplot(aes(x = dates, y = counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity')
#aces_w_kills_last_30_days_usaaf_dists %>% ggplot(aes(x = dates, y = counts, colour = as.factor(credit_cuts))) + geom_line()
#rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#  ggplot(aes(x = dates, y = counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') + facet_grid(.~airforce)

#rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#  ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')
rolling_number_of_pilots_by_kills = rbind(#aces_w_kills_last_30_days_luftwaffe_dists,#
  aces_w_kills_last_30_days_usaaf_dists, aces_w_kills_last_30_days_luftwaffe_dists_usaaf) %>% mutate(AirForce = airforce) %>%
  ggplot(aes(x = dates, y = num_pilots_by_counts, colour = AirForce)) + geom_line() + facet_grid(credit_cuts~., scales = 'free')+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) +
  ylab('Number of Pilots w/ Claims by Claim Bucket')+
  ggtitle("Number of Pilots with Claims in Previous 30 Days")+
  xlab("Date (Rolling Average in last 30 days)")

rolling_number_of_pilots_by_kills = 
  ggplot() + 
  geom_line(data = rbind(#aces_w_kills_last_30_days_luftwaffe_dists,#
    aces_w_kills_last_30_days_usaaf_dists, aces_w_kills_last_30_days_luftwaffe_dists_usaaf) %>% mutate(AirForce = airforce) %>% 
      mutate(credit_cuts = factor(credit_cuts, levels = c('(0,3]', '(3,5]', '(5,10]', '(10,500]'))), 
    aes(x = dates, y = num_pilots_by_counts, colour = AirForce)
    ) + facet_grid(credit_cuts~., scales = 'free')+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) +
  ylab('Number of Pilots w/ Claims by Claim Bucket')+
  ggtitle("Number of Pilots with Claims in Previous 30 Days")+
  xlab("Date (Rolling Average in last 30 days)")

rolling_number_of_pilots_by_kills = rolling_number_of_pilots_by_kills + geom_segment(data = timeline_df %>% mutate(position = position / 10 ) %>% mutate(credit_cuts = ('Timeline')), 
             aes(x = as.Date(start_date) , xend = as.Date(end_date), y = position, yend = position)) + 
  geom_text_repel(data = timeline_df %>% mutate(position = position / 10 ) %>% mutate(credit_cuts = ('Timeline')), 
                  aes(x = as.Date(start_date), y = position, label = event), size= 3, min.segment.length = 0)
  
rolling_number_of_pilots_by_kills_all = 
  ggplot() + 
  geom_line(data = rbind(aces_w_kills_last_30_days_luftwaffe_dists,
    aces_w_kills_last_30_days_usaaf_dists, aces_w_kills_last_30_days_luftwaffe_dists_usaaf) %>% mutate(AirForce = airforce) %>% 
      mutate(credit_cuts = factor(credit_cuts, levels = c('(0,3]', '(3,5]', '(5,10]', '(10,500]'))), 
    aes(x = dates, y = num_pilots_by_counts, colour = AirForce)
  ) + facet_grid(credit_cuts~., scales = 'free')+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) +
  ylab('Number of Pilots w/ Claims by Claim Bucket')+
  ggtitle("Number of Pilots with Claims in Previous 30 Days")+
  xlab("Date (Rolling Average in last 30 days)")

rolling_number_of_pilots_by_kills_all = rolling_number_of_pilots_by_kills_all + geom_segment(data = timeline_df %>% mutate(position = position / 10 ) %>% mutate(credit_cuts = ('Timeline')), 
                                                                                     aes(x = as.Date(start_date) , xend = as.Date(end_date), y = position, yend = position)) + 
  geom_text_repel(data = timeline_df %>% mutate(position = position / 10 ) %>% mutate(credit_cuts = ('Timeline')), 
                  aes(x = as.Date(start_date), y = position, label = event), size= 3, min.segment.length = 0)


rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~.)

# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>% 
#   group_by(credit_cuts, dates) %>%
#   summarise(num_pilots_by_counts_ratio = num_pilots_by_counts[airforce == "luftwaffe"] / num_pilots_by_counts[airforce == "usaaf"],
#             num_claims_by_counts_ratio = num_claims_by_counts[airforce == "luftwaffe"] / num_claims_by_counts[airforce == "usaaf"]
#   ) %>% 
#   as.data.frame() %>% 
#   ggplot(aes(x = dates, y = log(num_claims_by_counts_ratio), colour = credit_cuts)) + geom_line()


# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>% 
#   group_by(credit_cuts, dates) %>%
#   summarise(num_pilots_by_counts_ratio = num_pilots_by_counts[airforce == "luftwaffe"] / num_pilots_by_counts[airforce == "usaaf"],
#             num_claims_by_counts_ratio = num_claims_by_counts[airforce == "luftwaffe"] / num_claims_by_counts[airforce == "usaaf"]
#   ) %>% 
#   as.data.frame() %>% subset(dates < '1945-01-01') %>% 
#   ggplot(aes(x = dates, y = log(num_pilots_by_counts_ratio), colour = credit_cuts)) + geom_line()


rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>% 
  group_by(dates,airforce) %>%
  summarise(num_claims_by_counts = sum(num_claims_by_counts),
            num_pilots_by_counts = sum(num_pilots_by_counts)
  ) %>% 
  as.data.frame() %>% subset(dates < '1945-01-01') %>% 
  ggplot(aes(x = dates, y = num_claims_by_counts, colour = airforce)) + geom_line()


# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>% 
#   group_by(credit_cuts, dates) %>%
#   summarise(num_pilots_by_counts_ratio = num_pilots_by_counts[airforce == "luftwaffe"] / num_pilots_by_counts[airforce == "usaaf"],
#             num_pilots_by_counts_diff = num_pilots_by_counts[airforce == "luftwaffe"] - num_pilots_by_counts[airforce == "usaaf"],
#             num_claims_by_counts_ratio = num_claims_by_counts[airforce == "luftwaffe"] / num_claims_by_counts[airforce == "usaaf"]
#   ) %>% 
#   as.data.frame() %>% subset(dates < '1945-01-01') %>% 
#   ggplot(aes(x = dates, y = num_pilots_by_counts_diff, colour = credit_cuts)) + geom_line()

# 
# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#   ggplot(aes(x = dates, y = num_claims_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') +
#   facet_grid(airforce~., scales = 'free')
# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#   ggplot(aes(x = dates, y = num_pilots_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') +
#   facet_grid(airforce~., scales = 'free')
# 
# 
# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#   ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')
# rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
#   ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')
# 
# num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_by_front_type = function(data, temp_date, num_credit_value, time_lag_value){
#   subset_at_least_x_creds_pilots = data %>% filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
#     subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
#     filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
#     mutate(credit_cuts = cut(num_credits, c(0, 5, 10,500)) ) %>% group_by(credit_cuts, front, group_type) %>% 
#     summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
#   
#   
#   subset_at_least_x_creds = data %>% filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
#     subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
#     as.data.frame() %>% subset(num_credits < 350) %>% 
#     mutate(credit_cuts = cut(num_credits, c(0,5, 10,500)) ) %>% group_by(credit_cuts, front, group_type) %>% 
#     summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
#   
#   to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates', 'front', 'group_type') )
#   
#   return(to_return)
# }
# 
# aces_w_kills_last_30_days_luftwaffe_dists_grouped = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_by_front_type(df_1, temp_date, 5, 30))
# aces_w_kills_last_30_days_luftwaffe_dists_grouped = do.call(rbind, aces_w_kills_last_30_days_luftwaffe_dists_grouped)
# aces_w_kills_last_30_days_luftwaffe_dists_grouped = aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% mutate(airforce = 'luftwaffe')
# 
# 
# aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(front))) +
#   geom_line() + facet_grid(credit_cuts~group_type, scales = 'free')
# aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(front))) +
#   geom_line() + facet_grid(credit_cuts~group_type, scales = 'free')
# 
# aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% subset(group_type == 'Jagdgeschwader') %>% 
#   ggplot(aes(x = dates, y = num_pilots_by_counts, fill = as.factor(credit_cuts))) +
#   geom_bar(stat = 'identity') + facet_grid(.~front, scales = 'free')
# 
# num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_killed = function(data, temp_date, num_credit_value, time_lag_value){
#   subset_at_least_x_creds_pilots = data %>% filter(!is.na(status_date)) %>%  
#     subset((temp_date - dates) >= 0 & status_date > temp_date) %>% group_by(full_name) %>%
#     filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
#     mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
#     summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
#   
#   
#   subset_at_least_x_creds = data %>% filter(!is.na(status_date)) %>%  
#     subset((temp_date - dates) >= 0 & status_date > temp_date) %>% group_by(full_name) %>%
#     as.data.frame() %>% subset(num_credits < 350) %>% 
#     mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
#     summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
#   
#   to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
#   
#   return(to_return)
# }
# 
# 
# alive_aces_luftwaffe = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_killed(df_1, temp_date, 5, 30))
# alive_aces_luftwaffe = do.call(rbind, alive_aces_luftwaffe)
# 
# 
# 
# #df_1 %>% filter(!is.na(status_date)) %>% group_by(full_name) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>% head()
# alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_pilots_by_counts, colour = credit_cuts)) + geom_point()
# alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_claims_by_counts, colour = credit_cuts)) + geom_point()
# #alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_claims_by_counts, fill = credit_cuts)) + geom_bar(stat = 'identity')
# 
# usaaf_kills %>%subset(yearmonth > '1943-01-01' &yearmonth < '1945-04-01')%>% group_by(yearmonth) %>% summarise(credits = sum(num_credits, na.rm = TRUE)) %>% as.data.frame() %>%
#   ggplot(aes(x = yearmonth , y=  credits)) +geom_point() +geom_line()
# 
# 
# lw_aces_1%>%mutate(yearmon = as.yearmon(status_date)) %>% group_by(yearmon) %>% summarise(counts = n())%>%
#   subset(yearmon > '1943-01-01' &yearmon < '1945-04-01') %>% ggplot(aes(x  = yearmon, y=  counts)) + geom_point() +geom_line()
# 
# 
# df_1 %>%mutate(yearmon = as.yearmon(status_date))  %>% group_by(full_name) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>%
#   group_by(yearmon, front) %>% summarise(counts = n())%>%
#   subset(yearmon > '1943-01-01' &yearmon < '1945-04-01') %>% ggplot(aes(x  = yearmon, y=  counts, colour = front)) + geom_point() +geom_line()


luftwaffe_dist_of_claims = df_1  %>%group_by(full_name) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>% 
  group_by(num_credits) %>% summarise(counts = n())%>% as.data.frame()  %>% 
  subset(num_credits < 350)%>% mutate(percentage_claims = counts / sum(counts)) %>%
  mutate(cumsum_percentage_claims = cumsum(percentage_claims)) %>% 
  mutate(airforce = 'luftwaffe')

us_dist_of_claims = usaaf_kills  %>%group_by(SN) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>% 
  group_by(num_credits) %>% summarise(counts = n())%>% as.data.frame()  %>% 
  subset(num_credits < 350)%>% mutate(percentage_claims = counts / sum(counts)) %>%
  mutate(cumsum_percentage_claims = cumsum(percentage_claims)) %>% 
  mutate(airforce = 'usaaf')
us_dist_of_claims %>% head(30)
rbind(luftwaffe_dist_of_claims, us_dist_of_claims) %>% subset(num_credits < 10) %>% ggplot(aes(x = num_credits, y = cumsum_percentage_claims, colour = airforce)) + 
  geom_line()

aerial_claims_dists = rbind(df_1  %>%group_by(full_name) %>% filter(num_credits == max(num_credits)) %>%subset(num_credits < 350) %>% as.data.frame() %>%select(num_credits) %>% mutate(af = 'luftwaffe'),
                            usaaf_kills  %>%group_by(SN) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>% select(num_credits) %>% mutate(af = 'usaaf')
                            )

dists_of_claims = aerial_claims_dists %>% mutate(AirForce = af) %>% subset(num_credits < 25) %>% 
  ggplot(aes(x = num_credits, fill = AirForce)) + geom_histogram(position = 'dodge') + 
   theme_pomological() +
  theme(legend.position = 'bottom',text = element_text(size = 15))+
  xlab("Number of Aerial Claims") + ylab("Counts of Pilots") + 
  ggtitle("Distribution of Pilot Claims")

distribution_cumsum_claims = rbind(luftwaffe_dist_of_claims, us_dist_of_claims) %>% subset(num_credits < 25)%>% mutate(AirForce = airforce)%>% 
  ggplot(aes(x = num_credits, y = cumsum_percentage_claims*100, colour = AirForce)) + 
  geom_line()+ theme_pomological() +theme(legend.position = 'bottom',text = element_text(size = 15))+ 
  xlab("Number of Aerial Claims") + ylab("Cumulative Percentage of Pilots") + 
  ggtitle("Cumulative Distribution of Pilot Claims")
library(ggarrange)
dists_of_claims = ggpubr::ggarrange(dists_of_claims,distribution_cumsum_claims, ncol = 1 )

##

usaf_serial_data_combined_text = read.csv('/users/sweiss/src/luftwaffe_locations/data/us_plane_histories.csv')
usaaf_victory_credits = read.csv('/Users/sweiss/src/luftwaffe_locations/data/USAAF Victory Credits WWII-Table 1.csv')

usaaf_victory_credits_adj = usaaf_victory_credits[-c(1:4),-1]
colnames(usaaf_victory_credits_adj) = usaaf_victory_credits[4,-1]
usaaf_victory_credits_adj[,'dates'] = as.Date(usaaf_victory_credits_adj[,'Date'], '%m/%d/%y') - years(100)
usaaf_victory_credits_adj = usaaf_victory_credits_adj %>% mutate(yearmonth = as.yearmon(usaaf_victory_credits_adj[,'dates']))

usaaf_kills = usaaf_victory_credits_adj %>% subset(Theater %in% c('ETO', 'MTO')) #%>% mutate(ordinal_nums = toOrdinal(as.numeric(gsub(' Fighter Squadron', '', Unit_Echelon )) ))

usaaf_kills[,'unit_number'] = toOrdinal::toOrdinal(lapply(as.numeric(unlist(lapply(strsplit(usaaf_kills[,'Unit_Echelon'], split = ' '), function(x) x[1]))), function(x) if(is.na(x)){return(10000)} else{return(x)} ))
usaaf_kills[,'fs'] = grepl('Fighter Squadron',usaaf_kills[,'Unit_Echelon'])*1
usaaf_kills[,'fg'] = grepl('Fighter Group',usaaf_kills[,'Unit_Echelon'])*1
usaaf_kills = usaaf_kills %>% mutate(FS_ordinal_name = ifelse(fs == 1, paste(unit_number, 'FS'),   ifelse(fg == 1, paste(unit_number, 'FG'), '' )   ))
unique_FS_ordinal_name =  unique(usaaf_kills[,'FS_ordinal_name'])
unique_FS_ordinal_name = unique_FS_ordinal_name[which(unique_FS_ordinal_name != '')]

unique_dates = seq.Date(as.Date('1941-12-01'), as.Date('1945-09-15'), by = 'day')
scaffold_fs_dates = expand.grid(unique_FS_ordinal_name, unique_dates)
colnames(scaffold_fs_dates) = c('FS_ordinal_name', 'dates')



get_plane_types = function(fs, temp_df){
  
  to_return = temp_df  %>%subset(grepl(paste0(' ', fs), new_text))%>% filter(!is.na(dates_combined)) %>% select(plane_type,  dates_combined, new_text) %>% 
    mutate(dates = dates_combined) %>% select(-dates_combined) %>%
    mutate(FS_ordinal_name = fs)
  
}

get_fs_plane_data = lapply(unique_FS_ordinal_name, function(x) get_plane_types( x, usaf_serial_data_combined_text))
get_fs_plane_data = do.call(rbind,get_fs_plane_data)
get_fs_plane_data[,'fighter_type'] = ''
get_fs_plane_data[grep('P-47', get_fs_plane_data[,'plane_type']),'fighter_type'] = 'P-47'
get_fs_plane_data[grep('P-51', get_fs_plane_data[,'plane_type']),'fighter_type'] = 'P-51'
get_fs_plane_data[grep('P-38', get_fs_plane_data[,'plane_type']),'fighter_type'] = 'P-38'
get_fs_plane_data[grep('P-40', get_fs_plane_data[,'plane_type']),'fighter_type'] = 'P-40'
table(get_fs_plane_data[,'fighter_type'])

scaffold_fs_dates_merged = merge(scaffold_fs_dates,  subset(get_fs_plane_data, fighter_type !=''), by = c('FS_ordinal_name', 'dates'), all.x = TRUE)
scaffold_fs_dates_merged = scaffold_fs_dates_merged  %>% arrange(dates) %>% group_by(FS_ordinal_name) %>% fill(plane_type,new_text, fighter_type,.direction = 'down') %>% as.data.frame()

usaaf_kills_w_planes = merge(scaffold_fs_dates_merged, usaaf_kills,  by = c('FS_ordinal_name', 'dates'), all.y = TRUE)
usaaf_kills_w_planes %>% subset(Theater %in% c('ETO', 'MTO')) %>% group_by(fighter_type, Theater) %>%
  summarise(credits =sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame()


#confirm with https://sofrep.com/news/the-top-5-american-fighter-planes-of-wwii-with-the-most-kills/

usaaf_kills_w_planes %>% subset(Theater %in% c('ETO', 'MTO')) %>% group_by(fighter_type, Theater) %>%
  summarise(credits =sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame()

usaaf_kills_w_planes %>% group_by(yearmonth, fighter_type,Theater) %>% summarise(num_credits = sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame()%>%
  ggplot(aes(x = yearmonth, y=  num_credits, colour = fighter_type)) + geom_line() + facet_grid(fighter_type~Theater)

usaaf_plane_kills_by_type = usaaf_kills_w_planes %>% subset(yearmonth> '1942-01-01') %>% group_by(yearmonth, fighter_type,Theater) %>% summarise(num_credits = sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame()%>%
  ggplot(aes(x = yearmonth, y=  num_credits, fill = fighter_type)) + geom_bar(stat = 'identity', position = 'stack') + facet_grid(.~Theater)+ theme_minimal()+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) + 
  xlab('Date (Month)') + ylab('Number of USAAF Aerial Claims') + 
  ggtitle('Number of USAAF Aerial CLaims by Theater and Fighter Type')


timeline_df = data.frame( 
  start_date = as.Date(c('1944-01-21', '1944-02-20','1944-03-05', '1943-06-14', '1944-06-06', '1943-10-14','1943-08-17','1943-12-05', '1943-07-09', '1944-12-16')),
  end_date = as.Date(c('1944-01-21', '1944-02-25','1944-03-08','1944-04-19', '1944-06-06','1943-10-14','1943-08-17','1943-12-05', '1943-07-17', '1945-01-28')),
  event = c('First Duty Change','Operation Argument','Battle of Berlin','Operation Pointblank','D-Day','2nd Schweinfurt','1st Scwheinfurt', 'First Mustang Escort', 'Invasion of Sicily', 'Battle of the Bulge'),
  position = c(-.5,-1,-1,-2,-.5,-1,-1, 
               -.5,-1,-1)*10,
  text_position = -c(.55,1.05,1.05,2.05,.95,1.05,1.05,.55,1,1)
)
timeline_df = timeline_df   %>% subset(event != 'First Mustang Escort')
timeline_df = timeline_df   %>% subset(event != 'Operation Pointblank')
timeline_df = timeline_df   %>% subset(event != '1st Scwheinfurt')
timeline_df = timeline_df   %>% subset(event != '2nd Schweinfurt')


usaaf_plane_kills_by_type = ggplot() + 
geom_bar(data = usaaf_kills_w_planes %>% subset(yearmonth> '1942-01-01') %>% group_by(yearmonth, fighter_type,Theater) %>% 
           summarise(num_credits = sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame() %>% mutate(Fighter = fighter_type), 
         aes(x = yearmonth, y=  num_credits, fill = Fighter),stat = 'identity', position = 'stack') +
  geom_segment(data = timeline_df %>% mutate(position = position / 10 + 700) %>% mutate(Theater = ('MTO')), 
               aes(x = as.yearmon(start_date) , xend = as.yearmon(end_date), y = position, yend = position)) + 
  geom_text_repel(data = timeline_df %>% mutate(position = position / 10 + 700) %>% mutate(Theater = ('MTO')), 
                   aes(x = as.yearmon(start_date), y = position, label = event), size= 3, min.segment.length = 0)+ 
   facet_grid(Theater~.)+ theme_minimal()+
  theme_pomological()+ theme(legend.position = 'bottom',text = element_text(size = 15)) + 
  xlab('Date (Month)') + ylab('Number of USAAF Aerial Claims') + 
  ggtitle('Number of USAAF Aerial CLaims by Theater and Fighter Type')


ggsave(claim_by_county, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/luftwaffe_claims_by_country.png', dpi = 600)
ggsave(rolling_number_of_pilots_by_kills, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/rolling_number_of_pilots_by_kills.png', dpi = 600)
ggsave(rolling_number_of_pilots_by_kills_all, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/rolling_number_of_pilots_by_kills_all.png', dpi = 600)


ggsave(enter_exit_luftwaffe_plot, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/enter_exit_luftwaffe_plot.png', dpi = 600)
ggsave(enter_exit_luftwaffe_plot_by_pilot, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/enter_exit_luftwaffe_plot_by_pilot.png', dpi = 600)
ggsave(usaaf_plane_kills_by_type, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/usaaf_plane_kills_by_type.png', dpi = 600)
ggsave(dists_of_claims, width = 8, height = 6, file = '/users/sweiss/src/luftwaffe_locations/plots/dists_of_claims.png', dpi = 600)



enter_exit_luftwaffe_plot
enter_exit_luftwaffe_plot_by_pilot


usaaf_plane_kills_by_type

dists_of_claims
rolling_number_of_pilots_by_kills

