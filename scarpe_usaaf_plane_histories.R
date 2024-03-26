library(rvest)
library(dplyr)
library(gtools)
library(tidyr)

library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(toOrdinal)

links = c('http://www.joebaugher.com/usaf_serials/1938.html', 
          'http://www.joebaugher.com/usaf_serials/1940.html',
          paste0('http://www.joebaugher.com/usaf_serials/1941_', 1:5,'.html'),
          paste0('http://www.joebaugher.com/usaf_serials/1942_', 1:3,'a.html'),
          paste0('http://www.joebaugher.com/usaf_serials/1943_', 1:2,'.html'),
          paste0('http://www.joebaugher.com/usaf_serials/1944_', 1:5,'.html'),
          'http://www.joebaugher.com/usaf_serials/1945.html'
)

get_data_frames  = function(link){
  print(link)
  temp_site = read_html(link)
  strings = temp_site %>% html_elements('pre') %>%   html_text2()
  
  string_split <- strsplit(strings, "[\r\n]+")[[1]]

  test_df = do.call(smartbind,lapply(string_split, function(x) as.data.frame(t(as.matrix((strsplit(x, "[\t]")[[1]])))) ) )
  test_df[which(test_df[,'V1'] == ''),'V1'] = NA
  test_df[which(test_df[,'V3'] == ''),'V3'] = NA
  test_df[which(test_df[,'V5'] == ''),'V5'] = NA
  
  test_df = test_df %>% fill(V1, .direction = 'down')
  test_df = test_df %>% fill(V3, .direction = 'down')
  test_df = test_df %>% fill(V5, .direction = 'down')
  
  test_df =test_df%>% mutate(string_descriptions = ifelse(is.na(V6), V5, V6))
  test_df = test_df %>% mutate(site = link)
  return(test_df)
  
}
all_data = lapply(links, get_data_frames)
usaf_serial_data = do.call(smartbind,all_data)
usaf_serial_data = usaf_serial_data %>% mutate(dates = mark::str_extract_date(iconv(V5,"WINDOWS-1252","UTF-8"), format = '%b %d, %Y') )
usaf_serial_data[grep('g landing) at Honnington,Suffolk, Aug 27, 1944.  Pilot survive', usaf_serial_data[,'V3']),'V3'] = 'North American P-51D-5-NA Mustang'

usaf_serial_data_combined_text = usaf_serial_data %>% group_by(V1, V3, V5)%>% summarise(new_text = paste(string_descriptions, collapse = ';'))

for(x in 1:9){
  print(x)
  usaf_serial_data_combined_text[,'new_text'] = unlist(lapply(usaf_serial_data_combined_text[,'new_text'], function(q) gsub(paste0(' ',x,' '),paste0(' 0',x, ' '),q)))
  usaf_serial_data_combined_text[,'new_text'] = unlist(lapply(usaf_serial_data_combined_text[,'new_text'], function(q) gsub(paste0(' ',x,','),paste0(' 0',x, ','),q)))
}
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates = mark::str_extract_date(iconv(gsub(';', ' ',new_text),"WINDOWS-1252","UTF-8"), format = '%b %d, %Y') )
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates2 = mark::str_extract_date(iconv(gsub(';', ' ',new_text),"WINDOWS-1252","UTF-8"), format = '%b%d%y') - years(100))
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates3 = mark::str_extract_date(iconv(gsub(';', ' ',new_text),"WINDOWS-1252","UTF-8"), format = '%d%b%y') -years(100))
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates5 = mark::str_extract_date(iconv(gsub(';', ' ',new_text),"WINDOWS-1252","UTF-8"), format = '%B %d, %Y') )

usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% as.data.frame()
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates_combined = as.Date(ifelse(is.na(dates), as.Date(dates2), as.Date(dates))))
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates_combined = as.Date(ifelse(is.na(dates_combined), as.Date(dates3), as.Date(dates_combined))))
usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(dates_combined = as.Date(ifelse(is.na(dates_combined), as.Date(dates5), as.Date(dates_combined))))



usaf_serial_data_combined_text = usaf_serial_data_combined_text %>% mutate(plane_type = V3)
write.csv(usaf_serial_data_combined_text, file = '/users/sweiss/src/luftwaffe_locations/data/us_plane_histories.csv', row.names = FALSE)

usaf_serial_data_combined_text = read.csv('/users/sweiss/src/luftwaffe_locations/data/us_plane_histories.csv')
usaaf_victory_credits = read.csv('/Users/sweiss/Downloads/USAAF_Victories/USAAF Victory Credits WWII-Table 1.csv')
usaaf_victory_credits_adj = usaaf_victory_credits[-c(1:4),-1]
colnames(usaaf_victory_credits_adj) = usaaf_victory_credits[4,-1]
usaaf_victory_credits_adj[,'dates'] = as.Date(usaaf_victory_credits_adj[,'Date'], '%m/%d/%y') - years(100)
usaaf_victory_credits_adj = usaaf_victory_credits_adj %>% mutate(yearmonth = as.yearmon(usaaf_victory_credits_adj[,'dates']))

usaaf_kills = usaaf_victory_credits_adj %>% subset(Theater %in% c('ETO', 'MTO')) #%>% mutate(ordinal_nums = toOrdinal(as.numeric(gsub(' Fighter Squadron', '', Unit_Echelon )) ))

usaaf_kills[,'unit_number'] = toOrdinal(lapply(as.numeric(unlist(lapply(strsplit(usaaf_kills[,'Unit_Echelon'], split = ' '), function(x) x[1]))), function(x) if(is.na(x)){return(10000)} else{return(x)} ))
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

usaaf_kills_w_planes %>% subset(yearmonth> '1942-01-01') %>% group_by(yearmonth, fighter_type,Theater) %>% summarise(num_credits = sum(as.numeric(Credit), na.rm = TRUE)) %>% as.data.frame()%>%
  ggplot(aes(x = yearmonth, y=  num_credits, fill = fighter_type)) + geom_bar(stat = 'identity', position = 'stack') + facet_grid(.~Theater)+ theme_minimal()+
  theme(legend.position = 'bottom') +
  xlab('Date (Month)') + ylab('Number of USAAF Aerial Claims') + 
  ggtitle('Number of USAAF Aerial CLaims by Theater and Fighter Type')


grouped_kills_date_theater %>% ggplot(aes(x = yearmonth, y = credits, group = Theater, color = Theater)) + geom_line() + theme_minimal()+
  theme(legend.position = 'bottom')

usaaf_kills = usaaf_kills %>% arrange(dates) %>% mutate(temp_index = 1) %>% group_by(SN) %>% 
  mutate(num_credits = cumsum(Credit)) %>% as.data.frame() %>% 
  select(-temp_index)

usaaf_kills %>% mutate()
library(toOrdinal)
sapply(1:5,toOrdinal)

toOrdinal(353)


usaaf_kills %>% group_by(Unit_Echelon)%>% summarise(Credit = sum(as.numeric(Credit), na.rm = TRUE ))%>% arrange(-Credit) %>% head(20)


adjusted_df %>%subset(grepl('96th FS', new_text)) %>% head(20)
#adjusted_df = adjusted_df %>% mutate(dates2 = mark::str_extract_date(iconv(V5,"WINDOWS-1252","UTF-8"), format = '%b%d%Y') )

temp_site = read_html('http://www.joebaugher.com/usaf_serials/1943_2.html')
strings = temp_site %>% html_elements('pre') %>%   html_text2()

string_split <- strsplit(strings, "[\r\n]+")[[1]]
test_splits = string_split[1:20]
library(gtools)
library(tidyr)
test_df = as.data.frame(do.call(smartbind, do.call(smartbind,lapply(string_split[1:10], function(x) as.data.frame(t(as.matrix((strsplit(x, "[\t]")[[1]])))) ) )))
test_df = do.call(smartbind,lapply(string_split, function(x) as.data.frame(t(as.matrix((strsplit(x, "[\t]")[[1]])))) ) )
test_df[which(test_df[,'V1'] == ''),'V1'] = NA
test_df[which(test_df[,'V3'] == ''),'V3'] = NA
test_df[which(test_df[,'V5'] == ''),'V5'] = NA

test_df = test_df %>% fill(V1, .direction = 'down')
test_df = test_df %>% fill(V3, .direction = 'down')
test_df = test_df %>% fill(V5, .direction = 'down')

test_df =test_df%>% mutate(string_descriptions = ifelse(is.na(V6), V5, V6))
test_df %>% subset(grepl('P-51', V3)) %>% subset(grepl('76th', V5)) %>% head()

head(test_df)
p51_data = test_df %>% subset(V1 %in% (test_df%>% subset(grepl('North American P-51', test_df[,'V3'])) %>% pull(V1)))
