library(dplyr)
library(zoo)
library(gtools)
library(ggplot2)
library(lubridate)
library(reshape)

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

claims_east = read.csv('/users/sweiss/downloads/luftwaffe_claims/claims_east.txt')
claims_west = read.csv('/users/sweiss/downloads/claims_west.txt')

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
load('data/luftwaffe_sizes_locations_2.rdata')


luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurow'),'lat'] = 49.4230
luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurow'),'lon'] = 26.9871

luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurov'),'lat'] = 49.4230
luftwaffe_locations[which(luftwaffe_locations[,'locations'] == 'Proskurov'),'lon'] = 26.9871


luftwaffe_locations[which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 52I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0),'lat'] = 51.7682
luftwaffe_locations[which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 52I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0),'lon'] = 21.9557

which((luftwaffe_locations[,'group_squadron'] == 'Jagdgeschwader 3 "Udet"I. Gruppe:_') * (luftwaffe_locations[,'date_start'] > '1944-07-01' & luftwaffe_locations[,'date_start'] < '1944-09-01')*(luftwaffe_locations[,'locations'] == 'Millerowo') > 0)

df_1[,'index'] = 1:nrow(df_1)

min_max_pilot_data = df_1 %>% group_by(full_name) %>% summarise(min_date = min(dates), max_date= max(dates)) %>% as.data.frame()
min_max_pilot_data = min_max_pilot_data %>% mutate(full_name = factor( min_max_pilot_data[,'full_name'], levels = min_max_pilot_data[,'full_name'][order(min_max_pilot_data[,'min_date'])]))
min_max_pilot_data%>% subset(min_date > '1943-06-1') %>% ggplot(aes(x = min_date, xend = max_date, y = full_name, yend = full_name, colour = as.numeric((as.numeric(max_date-min_date))>70))) + geom_segment()+
  theme_update(axis.ticks.y = element_blank(),
               axis.text.y = element_blank()) + 
  scale_color_gradient(high = 'blue', low = 'red')

min_max_pilot_data%>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>% 
  group_by(min_date_month, max_date_month) %>% summarise(counts = n()) %>%as.data.frame() %>%
  ggplot(aes(x = min_date_month, y = max_date_month, colour = counts, size = counts)) + geom_point()

min_max_pilot_data %>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>%
  group_by(min_date_month)%>% summarise(counts = n()) %>% as.data.frame()%>%
  ggplot(aes(x = min_date_month , y= counts)) + geom_line()
min_max_pilot_data %>% mutate(min_date_month = as.yearmon(min_date), max_date_month = as.yearmon(max_date)) %>%
  group_by(max_date_month)%>% summarise(counts = n()) %>% as.data.frame()%>%
  ggplot(aes(x = max_date_month , y= counts)) + geom_line()


df_1 %>% group_by(yearmon) %>% summarise(counts = n()) %>% as.data.frame() %>%  
  ggplot(aes(x = yearmon, y = counts)) + geom_line()

df_1[which(df_1[,'aircraft'] %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')),]%>% 
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

grouped_kills_date_theater %>% ggplot(aes(x = yearmonth, y = credits, group = Theater, color = Theater)) + geom_line()

grouped_kills_date_europe = usaaf_victory_credits_adj %>% filter(Theater %in% c('MTO', 'ETO')) %>% group_by(yearmonth) %>% summarise(credits = sum(as.numeric(as.character(Credit)), na.rm = TRUE))  %>% as.data.frame() 
usaaf_kills = usaaf_victory_credits_adj %>% filter(Theater %in% c('MTO', 'ETO'))
usaaf_kills = usaaf_kills %>% arrange(dates) %>% mutate(temp_index = 1) %>% group_by(SN) %>% 
  mutate(num_credits = cumsum(Credit)) %>% as.data.frame() %>% 
  select(-temp_index)


df_1 = df_1 %>% arrange(dates) %>% mutate(temp_index = 1) %>% group_by(full_name) %>% 
  mutate(num_credits = cumsum(temp_index)) %>% as.data.frame() %>% 
  select(-temp_index)


usaaf_victory_credits_adj %>% group_by(Unit_Echelon) %>% summarise(Credit = sum(as.numeric(Credit), na.rm = TRUE)) %>% arrange(Credit)

temp_date = as.Date('1944-03-01')
num_pilots_w_x_credits_and_recent_claim = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds = data %>% subset(num_credits  >= num_credit_value)
  subset_at_least_x_creds = subset_at_least_x_creds  %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 )
  pilot_names = subset_at_least_x_creds %>% pull(SN) %>% unique() 
  pilot_names_data = data %>% subset(SN %in% pilot_names) %>% group_by(SN) %>% filter(dates == min(dates)) %>% as.data.frame() %>% mutate(date_diff = temp_date - dates) %>% summarise(mean_diff_date = mean(date_diff), sd_diff_date = sd(date_diff), counts = n())
  
  return(pilot_names_data)
}

num_pilots_w_x_credits_and_recent_claim_luftwaffe = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds = data %>% subset(num_credits  >= num_credit_value)
  subset_at_least_x_creds = subset_at_least_x_creds %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 )
  pilot_names = subset_at_least_x_creds %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>%
    pull(full_name) %>% unique() 
  pilot_names_data = data %>% subset(full_name %in% pilot_names)  %>% group_by(full_name) %>% filter(dates == min(dates)) %>% as.data.frame() %>% mutate(date_diff = temp_date - dates) %>% summarise(mean_diff_date = mean(date_diff), sd_diff_date = sd(date_diff), counts = n())
  return(pilot_names_data)
}


num_pilots_w_x_credits_and_recent_claim_dists = function(data, temp_date, num_credit_value, time_lag_value){
  #subset_at_least_x_creds = data %>% subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
  #  filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
  #  mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
  #  summarise(counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds_pilots = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(SN) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds,subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  
  return(to_return)
}

num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds_pilots = data %>% filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,3, 5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  return(to_return)
}




aces_w_kills_last_30_days_usaaf = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim(usaaf_kills, temp_date, 5 , 30))
aces_w_kills_last_30_days_usaaf = do.call(rbind,aces_w_kills_last_30_days_usaaf) %>% as.data.frame()
aces_w_kills_last_30_days_usaaf[,'date'] = seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day')
aces_w_kills_last_30_days_usaaf_melt = melt(aces_w_kills_last_30_days_usaaf, id.vars = 'date')

aces_w_kills_last_30_days_usaaf_melt %>% ggplot(aes(x = date, y = value)) + geom_point() + facet_grid(variable~.,scale = 'free')

aces_w_kills_last_30_days_luftwaffe = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe(df_1, temp_date, 5, 30))
aces_w_kills_last_30_days_luftwaffe = do.call(rbind, aces_w_kills_last_30_days_luftwaffe)
aces_w_kills_last_30_days_luftwaffe[,'date'] = seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day')
aces_w_kills_last_30_days_luftwaffe_melt = melt(aces_w_kills_last_30_days_luftwaffe, id.vars = 'date')
aces_w_kills_last_30_days_luftwaffe_melt %>% ggplot(aes(x = date, y = value)) + geom_point() + facet_grid(variable~.,scale = 'free')


aces_w_kills_last_30_days_luftwaffe_dists = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists(df_1, temp_date, 5, 30))
aces_w_kills_last_30_days_luftwaffe_dists = do.call(rbind, aces_w_kills_last_30_days_luftwaffe_dists)
aces_w_kills_last_30_days_luftwaffe_dists = aces_w_kills_last_30_days_luftwaffe_dists %>% mutate(airforce = 'luftwaffe')

aces_w_kills_last_30_days_luftwaffe_dists %>% ggplot(aes(x = dates, y = num_claims_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity')
aces_w_kills_last_30_days_luftwaffe_dists %>% ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(credit_cuts))) + geom_line()

aces_w_kills_last_30_days_luftwaffe_dists %>% ggplot(aes(x = dates, y = num_pilots_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity')
aces_w_kills_last_30_days_luftwaffe_dists %>% ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(credit_cuts))) + geom_line()

aces_w_kills_last_30_days_usaaf_dists = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_dists(usaaf_kills, temp_date, 5, 30))
aces_w_kills_last_30_days_usaaf_dists = do.call(rbind, aces_w_kills_last_30_days_usaaf_dists)
aces_w_kills_last_30_days_usaaf_dists = aces_w_kills_last_30_days_usaaf_dists %>% mutate(airforce = 'usaaf')
aces_w_kills_last_30_days_usaaf_dists %>% ggplot(aes(x = dates, y = counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity')
aces_w_kills_last_30_days_usaaf_dists %>% ggplot(aes(x = dates, y = counts, colour = as.factor(credit_cuts))) + geom_line()
rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') + facet_grid(.~airforce)

rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')
rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')

rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_claims_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') +
  facet_grid(airforce~., scales = 'free')
rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_pilots_by_counts, fill = as.factor(credit_cuts))) + geom_bar(stat = 'identity') +
  facet_grid(airforce~., scales = 'free')


rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')
rbind(aces_w_kills_last_30_days_luftwaffe_dists,aces_w_kills_last_30_days_usaaf_dists) %>%
  ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(airforce))) + geom_line() + facet_grid(credit_cuts~., scales = 'free')

num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_by_front_type = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds_pilots = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0, 5, 10,25,50,500)) ) %>% group_by(credit_cuts, front, group_type) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% #filter(aircraft %in% c('P-51', 'P-47', 'P-38','B-17', 'B-24')) %>% 
    subset((temp_date - dates) < time_lag_value & (temp_date - dates) >= 0 ) %>% group_by(full_name) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts, front, group_type) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates', 'front', 'group_type') )
  
  return(to_return)
}

aces_w_kills_last_30_days_luftwaffe_dists_grouped = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_by_front_type(df_1, temp_date, 5, 30))
aces_w_kills_last_30_days_luftwaffe_dists_grouped = do.call(rbind, aces_w_kills_last_30_days_luftwaffe_dists_grouped)
aces_w_kills_last_30_days_luftwaffe_dists_grouped = aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% mutate(airforce = 'luftwaffe')


aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% ggplot(aes(x = dates, y = num_claims_by_counts, colour = as.factor(front))) +
  geom_line() + facet_grid(credit_cuts~group_type, scales = 'free')
aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% ggplot(aes(x = dates, y = num_pilots_by_counts, colour = as.factor(front))) +
  geom_line() + facet_grid(credit_cuts~group_type, scales = 'free')

aces_w_kills_last_30_days_luftwaffe_dists_grouped %>% subset(group_type == 'Jagdgeschwader') %>% 
  ggplot(aes(x = dates, y = num_pilots_by_counts, fill = as.factor(credit_cuts))) +
  geom_bar(stat = 'identity') + facet_grid(.~front, scales = 'free')

num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_killed = function(data, temp_date, num_credit_value, time_lag_value){
  subset_at_least_x_creds_pilots = data %>% filter(!is.na(status_date)) %>%  
    subset((temp_date - dates) >= 0 & status_date > temp_date) %>% group_by(full_name) %>%
    filter(num_credits == max(num_credits)) %>% as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_pilots_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  
  subset_at_least_x_creds = data %>% filter(!is.na(status_date)) %>%  
    subset((temp_date - dates) >= 0 & status_date > temp_date) %>% group_by(full_name) %>%
    as.data.frame() %>% subset(num_credits < 350) %>% 
    mutate(credit_cuts = cut(num_credits, c(0,5, 10,25,50,500)) ) %>% group_by(credit_cuts) %>% 
    summarise(num_claims_by_counts = n()) %>% as.data.frame() %>% mutate(dates = temp_date)
  
  to_return = merge(subset_at_least_x_creds, subset_at_least_x_creds_pilots, by=c('credit_cuts','dates') )
  
  return(to_return)
}


alive_aces_luftwaffe = lapply(seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), function(temp_date) num_pilots_w_x_credits_and_recent_claim_luftwaffe_dists_killed(df_1, temp_date, 5, 30))
alive_aces_luftwaffe = do.call(rbind, alive_aces_luftwaffe)



#df_1 %>% filter(!is.na(status_date)) %>% group_by(full_name) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>% head()
alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_pilots_by_counts, colour = credit_cuts)) + geom_point()
alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_claims_by_counts, colour = credit_cuts)) + geom_point()
#alive_aces_luftwaffe %>% ggplot(aes(x = dates, y = num_claims_by_counts, fill = credit_cuts)) + geom_bar(stat = 'identity')

usaaf_kills %>%subset(yearmonth > '1943-01-01' &yearmonth < '1945-04-01')%>% group_by(yearmonth) %>% summarise(credits = sum(num_credits, na.rm = TRUE)) %>% as.data.frame() %>%
  ggplot(aes(x = yearmonth , y=  credits)) +geom_point() +geom_line()


lw_aces_1%>%mutate(yearmon = as.yearmon(status_date)) %>% group_by(yearmon) %>% summarise(counts = n())%>%
  subset(yearmon > '1943-01-01' &yearmon < '1945-04-01') %>% ggplot(aes(x  = yearmon, y=  counts)) + geom_point() +geom_line()


df_1 %>%mutate(yearmon = as.yearmon(status_date))  %>% group_by(full_name) %>% filter(num_credits == max(num_credits)) %>% as.data.frame() %>%
  group_by(yearmon, front) %>% summarise(counts = n())%>%
  subset(yearmon > '1943-01-01' &yearmon < '1945-04-01') %>% ggplot(aes(x  = yearmon, y=  counts, colour = front)) + geom_point() +geom_line()


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
rbind(luftwaffe_dist_of_claims, us_dist_of_claims) %>% ggplot(aes(x = num_credits, y = cumsum_percentage_claims, colour = airforce)) + 
  geom_point()

luftwaffe_dist_of_claims %>% ggplot(aes(x = num_credits, y = cumsum_percentage_claims)) + geom_point()
data.frame(dates = seq.Date(as.Date('1943-01-01'), as.Date('1945-05-01'), by = 'day'), aces_w_kills_last_30_days_luftwaffe,aces_w_kills_last_30_days_usaaf) %>% 
  mutate(ratio = aces_w_kills_last_30_days_luftwaffe / aces_w_kills_last_30_days_usaaf) %>% 
  ggplot(aes(x = dates, y = log(ratio))) + 
  geom_point()



test = merge( df_1 %>% select(dates, aircraft, Location, victory_time, front, Unit, unit, first_name, last_name,gruppe,gruppe_unit,group_type,group_number,index) %>% mutate(date_start = dates, subgroup = gruppe) %>%
                mutate(subgroup = ifelse(subgroup =='', Unit, subgroup))%>% mutate(subgroup = ifelse(subgroup =='StSt.', 'Stab', subgroup)), 
  luftwaffe_locations  %>% select(lat, lon, locations, group_squadron, date_start, group_number, subgroup, country, group_type, JG), 
  on = c('group_type', 'date_start', 'group_number','subgroup'), all.x = TRUE
  )

test %>% subset(aircraft == 'P-51' & lon > 30)

temp_debug = (df_1 %>% subset(index == 59698)) %>% select(dates, aircraft, Location, victory_time, front, Unit, unit, first_name, last_name,gruppe,gruppe_unit,group_type,group_number,index) %>% mutate(date_start = dates, subgroup = gruppe) %>%
  mutate(subgroup = ifelse(subgroup =='', Unit, subgroup))

merge( (df_1 %>% subset(index == 59698)) %>% select(dates, aircraft, Location, victory_time, front, Unit, unit, first_name, last_name,gruppe,gruppe_unit,group_type,group_number,index) %>% mutate(date_start = dates, subgroup = gruppe) %>%
         mutate(subgroup = ifelse(subgroup =='', Unit, subgroup)) %>% mutate(subgroup = ifelse(subgroup =='StSt.', 'Stab', subgroup)),
  luftwaffe_locations %>% select(lat, lon, locations, group_squadron, date_start, group_number, subgroup, country, group_type, JG), 
  on = c('group_type', 'date_start', 'group_number','subgroup'), all.x = TRUE
) 
test


test_removed_nan_lat = test %>% subset(!(is.na(lat)))
test_nan_lat = test %>% subset((is.na(lat)))

test_nan_lat_night_fighters = merge( test_nan_lat %>% subset(group_type == 'Nachtjagdgeschwader')  %>% select(dates, aircraft, Location, victory_time, front, Unit, unit, first_name, last_name,gruppe,gruppe_unit,group_type,group_number,index) %>% mutate(date_start = dates, subgroup = gruppe) %>%
         mutate(subgroup = gruppe_unit) , 
       luftwaffe_locations %>% mutate(subgroup = squadron_number) %>% select(lat, lon, locations, group_squadron, date_start, group_number, subgroup, country, group_type, JG) , 
       on = c('group_type', 'date_start', 'group_number','subgroup'), all.x = TRUE
)

test_removed_nan_lat = rbind(test_removed_nan_lat, test_nan_lat_night_fighters %>% subset(!(is.na(lat))))

test = test %>% mutate(year_month = as.yearmon(as.Date(dates))) %>% mutate(lat_cut = cut(lat, seq(-100,100,2)), lon_cut = cut(lon, seq(-100,100,2))) %>% 
  mutate(lat_cut = gsub('\\(', '', lat_cut), lon_cut = gsub('\\(', '', lon_cut))

test[,'lat_cut'] = as.numeric(sapply(strsplit(test[,'lat_cut'], ','), function(x) x[[1]]))
test[,'lon_cut'] = as.numeric(sapply(strsplit(test[,'lon_cut'], ','), function(x) x[[1]]))
test = test %>% mutate(lat = lat_cut, lon = lon_cut)

  
  
  ggplot() + 
    geom_polygon(data = european_map_data %>% subset(date =='September_30_1944'), aes(x = long, y = lat, group = paste0(group, date_start)) ,
             color = "black", size = 0.1,linetype = 2, fill = "white") +
     geom_point(data =test  %>% group_by(aircraft, lat_cut, lon_cut) %>% summarise(counts = n()) %>% as.data.frame() %>% subset(aircraft == 'Il-2'), aes(x = lon_cut, y = lat_cut, size = counts)) 
  

araclocation_sizes = merge(
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

library(rvest)
temp_site = read.html('http://www.joebaugher.com/usaf_serials/1942_4.html')
