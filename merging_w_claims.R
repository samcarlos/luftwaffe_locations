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
claims_west = read.csv('/users/sweiss/downloads/luftwaffe_claims/claims_west.txt')

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
df_1[grep('NJG', df_1[,'unit']),'group_type'] = 'Nachtjagdgeschwader'
df_1[grep('JG', df_1[,'unit']),'group_type'] = 'Jagdgeschwader'
df_1[grep('ZG', df_1[,'unit']),'group_type'] = 'Zerstörergeschwader'

#df_1[,'JG'] = df_1[,'unit']
#df_1[,'JG'] = gsub('NJG', 'Nachtjagdgeschwader',df_1[,'JG'])
#df_1[,'JG'] = gsub('JG', 'Jagdgeschwader',df_1[,'JG'])
#df_1[,'JG'] = gsub('ZG', 'Zerstörergeschwader',df_1[,'JG'])

df_1[,'group_number'] = as.numeric(readr::parse_number(df_1[,'unit']))
load('data/luftwaffe_sizes_locations_2.rdata')

df_1[,'index'] = 1:nrow(df_1)
test = merge( df_1 %>% select(dates, aircraft, Location, victory_time, front, Unit, first_name, last_name,gruppe,gruppe_unit,group_type,group_number,index) %>% mutate(date_start = dates, subgroup = gruppe), 
  luftwaffe_locations %>% subset(disbanded == 0) %>% select(lat, lon, locations, group_squadron, date_start, group_number, subgroup, country, group_type, JG), 
  on = c('group_type', 'date_start', 'group_number','subgroup'), all.x = TRUE
  )

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

