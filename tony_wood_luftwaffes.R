library(dplyr)
library(zoo)
library(gtools)
library(ggplot2)
library(lubridate)
claims_east = read.csv('/users/sweiss/downloads/luftwaffe_claims/claims_east.txt')
claims_west = read.csv('/users/sweiss/downloads/luftwaffe_claims/claims_west.txt')

claims_east = mutate(claims_east, front = 'east')
claims_west = mutate(claims_west, front = 'west')

df = smartbind(claims_east, claims_west)
colnames(df)[c(3:4, 6, 7, 9)] = c('first_name','last_name','unit','aircraft','victory_time' )

df[,'dates'] = dmy(unlist(lapply(strsplit(df[,'Date'], ' '), function(x) x[[1]])))
df = unique(df)
df[,'yearmon'] = as.yearmon(df[,'dates'])
df = mutate(df, full_name = paste0(first_name, last_name))


claimed_kills_by_month = df %>%subset(full_name !='Unknown')%>% group_by(yearmon) %>% summarise(count = n()) %>% as.data.frame()
ggplot(subset(claimed_kills_by_month, count>10), aes(x = yearmon, y = count))+geom_line()+
  ggtitle('Number of Claimed aerial  kills over time')

claimed_kills_by_month_front = df %>%subset(full_name !='Unknown')%>% group_by(yearmon, front) %>% summarise(count = n()) %>% as.data.frame()
ggplot(subset(claimed_kills_by_month_front, count>10), aes(x = yearmon, y = count, group = front, colour = front))+geom_line()+
  ggtitle('Number of Claimed aerial  kills over time')


df = df %>% arrange(full_name,dates)
df[,'counts'] = 1:nrow(df)
df = df %>% group_by(full_name)%>% mutate(rank = dense_rank(counts)) %>% as.data.frame()
#df = df[-which(df[,'full_name'] == 'Unknown'),]

max_date = df %>% group_by(full_name) %>% 
  filter(dates == max(dates))%>% mutate(max_date = dates)%>%select(full_name, max_date)  %>% distinct() %>%as.data.frame()

df = merge(df, max_date, on = c('full_name'), all.x = TRUE)
df = mutate(df, date_diff = max_date - dates)

counts = lapply(seq.Date(as.Date("1939-09-01"),as.Date("1945-06-01"), by = 'month'), function(x){
  df[,'date_diff'] = x-df[,'max_date']
  get_counts_day = subset(df, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>% filter(rank == max(rank))%>%
    filter(date_diff < 7)  %>%
    as.data.frame()
  sum_counts = get_counts_day %>% group_by(front) %>% summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>5)*1))
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_by_group_month = lapply(seq.Date(as.Date("1939-09-01"),as.Date("1945-06-01"), by = 'month'), function(x){
  df[,'date_diff'] = x-df[,'max_date']
  get_counts_day = subset(df, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>% filter(rank == max(rank))%>%
    filter(date_diff < 7)  %>%
    as.data.frame()
  sum_counts = get_counts_day %>% group_by(front, unit) %>% summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>5)*1))
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_entries = lapply(seq.Date(as.Date("1939-09-01"),as.Date("1945-06-01"), by = 'month'), function(x){
  df[,'date_diff'] = x-df[,'max_date']
  get_counts_day = subset(df, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>% filter(rank == max(rank))%>%
    #filter(date_diff < 14)  %>%
    as.data.frame()
  sum_counts = get_counts_day %>% summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>5)*1))
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_exits = lapply(seq.Date(as.Date("1939-09-01"),as.Date("1945-06-01"), by = 'month'), function(x){
  df[,'date_diff'] = x-df[,'max_date']
  get_counts_day = df%>% group_by(full_name)%>% filter(dates == max(dates))%>% filter(rank == max(rank))%>%
    filter(date_diff > 7)  %>%
    as.data.frame()
  sum_counts = get_counts_day %>% summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>5)*1))
  sum_counts[,'date'] = x
  return(sum_counts)
})

ggplot(subset(counts, date > '1944-01-01'), aes(x = date, y= aces))+geom_line()

counts = do.call(rbind,counts)
counts_exits = do.call(rbind, counts_exits)
counts_entries = do.call(rbind,counts_entries)

colnames(counts_entries) = paste0(colnames(counts_entries), '_cum_entries')
colnames(counts_exits) = paste0(colnames(counts_exits), '_cum_exits')

plot(counts_entries[,c(4,1)])

plot(diff(counts_exits[,3])~ counts[-1,4], type = 'l')

plot(counts_entries[,1]-counts_exits[,1]~ counts[,4])
plot(counts[,c(4,1)])
plot(counts[,c(4,2)])
plot(counts[,c(4,3)])
plot(diff(counts_exits[,3])~ counts[-1,4], type = 'l', xlab= 'Date',ylab = 'Number of Pilots Exiting Sample',
     main = 'Number of Pilots Exiting Sample Each Month')
lines(diff(counts_entries[,3])~ counts[-1,4], type = 'l', col = 'blue')

(df %>% group_by(first_name, last_name)%>% filter(dates == max(dates)) %>% subset( dates>as.Date('1944-05-01') & dates< as.Date('1944-06-01')) %>%
    arrange(-rank) %>%
    as.data.frame())[1:10,]
counts_all = cbind(counts_exits, counts_entries, counts)
write.csv(counts_all, file = 'C:/Users/Larry/Google Drive/8th_air_force_data/luftwaffe_pilot_kill_data_aggregated.csv' ,row.names = FALSE)


#
group_by(full_name)%>% filter(dates == max(dates))
entry_exit_graph = df_1 %>% group_by(full_name) %>% filter(rank > 0) %>% filter(dates == min(dates)) %>% select(dates, max_date, front) %>% 
  ggplot(aes( x= dates , y= max_date, colour = front, group = front))+geom_point(size = .1)+facet_wrap(front~.)

ggExtra::ggMarginal(entry_exit_graph, type = "histogram")

library(ggplot2)

df_1 = df
df_1[is.na(df_1[,'date']),'date'] = as.Date('1945-06-01')

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

sort(table(df_1[,'aircraft']))
#subset( date_diff < 7)  %>%

counts_by_group = lapply(seq.Date(as.Date("1939-10-01"),as.Date("1945-05-08"), by = 'month'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    subset( date_diff < 7)  %>%
    as.data.frame()
  sum_counts = get_counts_day%>% group_by(unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1)) %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

ggplot(counts_by_group_month_rm_28, aes(x = date, y= aces, group = unit))+facet_grid(unit~.)+geom_line()


claims_by_unit = df_1 %>% group_by(unit) %>% summarise(n_claimed = n()) %>% as.data.frame()
claims_by_unit[,'unit'] = factor(claims_by_unit[,'unit'], claims_by_unit[order(claims_by_unit[,'n_claimed']),'unit'])

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
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "I"

df_1[,'reich_defense'] = 0
# njg 2, 3, 45, all west 
#NJG 4, 5, west
#jg 300, 301, 302 west wild sau 
#jg4 reich 
# ZG 26,76 , 1  heavy units disbanded 9/44
#need to work on jg 51,53, 3
#jg3 I france 6/6 - 8/8
#jg3 II france 6/6 - 10/10
#jg3 II france 6/6 - 9/11

unit = c('JG 54',  'JG 3',
         'JG 3', 'JG 3','JG 3', 
         "JG 53", "JG 27", 'JG 27', 'JG 27', 'JG 26', 'JG 2', 'NLG 1', 'JG 1',
         'JG 11', 'NJG 2', 'NJG 3', 'NJG 45', 'NJG 4', 'NJG 5', 'JG 300', 'JG 301', 'JG 302', 'JG 4', 'ZG 26','ZG 76', 'ZG 1',
         'JG 27', 'NJG 1', 'NJG 6', 'JG 106')
gruppe = c('III', 'I', 
           'II','III','IV', 
           'II', 'I', 'II', "III", 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA' ,
           'IV', 'NA','NA', 'NA')
enter_reich_defense = c('1943-06-01',  '1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01',
                        '1943-10-01', '1943-01-01', '1943-08-01', '1944-03-01', '1943-06-01',
                        '1943-06-01', '1943-06-01', '1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1944-03-23', '1943-06-01', '1943-06-01','1943-06-01')
exit_reich_defense =  c('1944-06-07', '1944-06-06', 
                        '1944-06-06', '1944-06-06', '1944-10-01',
                        '1944-06-06', '1944-06-06', '1944-10-01', '1944-06-06', '1944-06-06', 
                        '1944-06-06', '1944-10-01', '1944-06-06','1944-06-06', '1944-10-01', 
                        '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', 
                        '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01',
                        '1944-10-01','1944-10-01','1944-10-01','1944-10-01')

groups_enter_leave_reich_defense = data.frame(unit, gruppe, enter_reich_defense, exit_reich_defense)
groups_enter_leave_reich_defense = mutate(groups_enter_leave_reich_defense, 
                                          enter_reich_defense = as.Date(enter_reich_defense), 
                                          exit_reich_defense = as.Date(exit_reich_defense))

#jg 52 Russian
##jg54 III, 7,8,9, exited 7 Jun 1944 to France https://asisbiz.com/luftwaffe/jg54.html

##jg51 was med  ...
##jg51 II, 4,5,6  18 Aug 1943 https://asisbiz.com/luftwaffe/jg51.html. italian theater so will omit 
#j3 west 3 moved to france june 6
#jg 53 group II 16 Oct 1943 to austria then june 6 to lemans. 4,5,6
#JG 77 medit theater 
#JG 27  1943 jan  I./JG 27 1,2,3 was posted France 
#JG 27 august 1943 4,5,6 II./JG 27 was posted Germany
#njg 100 East
#jg4 reich 
##JG 2 all west than france. 
##NLG 1 night western  
# JG 5 norway finland
# JG 1 reich until June 1944 
# JG 11 reich until June 1944 
# njg 2, 3, 45, all west 
#NJG 4, 5, west
#jg 300, 301, 302 west wild sau 
# ZG 26,76 , 1  heavy units disbanded 9/44
#njg 100 East
#jg4 reich 
# SG2 east 
#JG 27 I, III, IV to go to france 
##JG 26 all west. focused on france after dday
#JG 27  7,8,9 III./JG 27 was posted Austria march 1944 t- june 6

#df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 27'& gruppe=='III'& dates >
#                                               as.Date('1944-03-01')  & dates < as.Date('1944-06-06') ,1,reich_defense))
#df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 27'& gruppe=='II'& dates >
#                                                 as.Date('1943-08-01')  ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 27'& gruppe=='I'& dates >
#                                                 as.Date('1943-01-01')  & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 54' & gruppe =='III' & dates < as.Date('1944-06-07') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 51' & gruppe =='II' & dates > as.Date('1943-08-18') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 3'  & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 53'& gruppe=='II'& dates >
#                                                 as.Date('1943-10-16')  & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 1' & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 1' & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NLG 1'  ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 2' & dates < as.Date('1944-06-06') ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NJG 2' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NJG 3' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NJG 45' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NJG 4' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='NJG 5' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 300' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 301' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 302' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='ZG 26' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='ZG 76' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='ZG 1' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 4' ,1,reich_defense))
# df_1 = df_1 %>% mutate(reich_defense = ifelse(unit =='JG 4' ,1,reich_defense))

df_1[,'unit_type'] = "JG"
df_1[grep('NJG',df_1[,'unit']),'unit_type'] = "NJG"
df_1[grep('JG 30',df_1[,'unit']),'unit_type'] = "wilde_sau"
df_1[grep('ZG',df_1[,'unit']),'unit_type'] = "ZG"


counts_by_group_month_rm_28 = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  df_1 = df_1 %>%mutate(last_kill_date_diff = x - dates)
  get_counts_day = subset(df_1, dates<x)%>%
    mutate(last_kill_date_diff = x - dates) %>% 
    group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    #subset( date_diff < 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  sum_counts = get_counts_day_1%>% group_by(unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_2_5 = sum((rank>2)*(rank<6)),
              gt_5_10 = sum((rank<11)*(rank>4)), gt_10_15 = sum((rank>9)*(rank>16)), gt_20 = sum((rank>19)*1) )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})
counts_by_group_month_rm_28 = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  df_1 = df_1 %>%mutate(last_kill_date_diff = x - dates)
  get_counts_day = subset(df_1, dates<x)%>%
    mutate(last_kill_date_diff = x - dates) %>% 
    group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    #subset( date_diff < 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  sum_counts = get_counts_day_1%>% group_by(unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_2_5 = sum((rank>2)*(rank<6)),
              gt_5_10 = sum((rank<11)*(rank>4)), gt_10_15 = sum((rank>9)*(rank>16)), gt_20 = sum((rank>19)*1) )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

jg_26_high_performers = subset(df_1, unit == "JG 26" & rank > 15 & dates > as.Date('1944-03-01') & dates< as.Date('1944-04-01')) %>% 
  select(full_name) %>% distinct()

subset(df_1, dates > as.Date('1944-03-01') & dates< as.Date('1944-0 n bhgng4-01')) %>%
  filter(full_name %in% jg_26_high_performers[,'full_name'])


counts_by_group_month_rm_28 = do.call(rbind,counts_by_group_month_rm_28)
ggplot(counts_by_group_month_rm_28, aes( x = date,y = aces))+geom_line()+facet_grid(unit~., scales = 'free')


counts_by_group_month_west = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    mutate(last_kill_date_diff = x-dates) %>%
    filter(rank == max(rank)) %>%
    subset( date_diff < 7)  %>%
    #subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  sum_counts = get_counts_day_1%>% group_by(unit_type,unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_2_5 = sum((rank>2)*(rank<6)),
              gt_5_10 = sum((rank<11)*(rank>4)), gt_10_15 = sum((rank>9)*(rank>16)), gt_20 = sum((rank>19)*1) )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})
counts_by_group_month_west = do.call(rbind,counts_by_group_month_west)
ggplot(counts_by_group_month_west, aes( x= date, y = aces ))+geom_line()+facet_wrap(unit~., scales = 'free')

counts_by_group_month_west_rm_28_days = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    mutate(last_kill_date_diff = x-dates) %>%
    filter(rank == max(rank)) %>%
    #subset( date_diff < 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  sum_counts = get_counts_day_1%>% group_by(unit_type) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_2_5 = sum((rank>2)*(rank<6)),
              gt_5_10 = sum((rank<11)*(rank>4)), gt_10_15 = sum((rank>9)*(rank>16)), gt_20 = sum((rank>19)*1) )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})
counts_by_group_month_west_rm_28_days_heavies = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    mutate(last_kill_date_diff = x-dates) %>%
    filter(rank == max(rank)) %>%
    #subset( date_diff < 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  sum_counts = get_counts_day_1%>% group_by(unit_type) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_2_5 = sum((rank>2)*(rank<6)),
              gt_5_10 = sum((rank<11)*(rank>4)), gt_10_15 = sum((rank>9)*(rank>16)), gt_20 = sum((rank>19)*1) )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

dists_enter = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'month'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    mutate(last_kill_date_diff = x-dates) %>%
    filter(rank == max(rank)) %>%
    #subset( date_diff > 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()
  

  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)

  min_dates = subset(df_1)%>% group_by(full_name)%>% filter(dates == min(dates)) %>% mutate(min_date = dates) %>% select(full_name, min_date) %>% 
    as.data.frame()
  
  get_counts_day_1 = merge(get_counts_day_1, min_dates, by = 'full_name') %>% select(min_date, rank)
  get_counts_day_1[,'temp_date'] = x
  return(get_counts_day_1)
})
dists_enter = do.call(rbind,dists_enter)
library(ggridges)

ggplot(dists_enter, aes( x = min_date, fill = min_date, fill = temp_date))+geom_histogram()+facet_grid(temp_date~.)
ggplot(subset(dists_enter, rank > 4), aes( x = min_date, fill = min_date, fill = temp_date))+geom_histogram()+facet_grid(temp_date~.)
dists_enter_agg = dists_enter %>% mutate(min_date_yearmon = as.yearmon(min_date)) %>%
  subset(rank > 4) %>% group_by(temp_date, min_date_yearmon) %>% summarise(counts = n()) %>% as.data.frame()
dists_enter_agg[,'year'] = unlist(lapply(strsplit(as.character(dists_enter_agg[,1]),'-'), function(x) x[1]))
dists_enter_agg[,'year'] = unlist(lapply(strsplit(as.character(dists_enter_agg[,1]),'-'), function(x) x[1]))

dists_enter_agg_2 = dists_enter_agg %>% group_by(temp_date, year) %>% summarise(counts = sum(counts))%>%as.data.
ggplot(dists_enter_agg_2, aes( x =  temp_date , y= counts, group = year,colour = as.factor(year)))+geom_line()


counts_by_group_month_west_entries = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = subset(df_1, dates<x)%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    #subset( date_diff < 7)  %>%
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  get_counts_day_1[,'unit_type'] = unlist(lapply(strsplit(get_counts_day_1[,'unit'], ' '), function(x) x[1]))
  sum_counts = get_counts_day_1%>% group_by(unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1)) %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_exits = lapply(seq.Date(as.Date("1939-09-01"),as.Date("1945-06-01"), by = 'month'), function(x){
  df[,'date_diff'] = x-df[,'max_date']
  get_counts_day = df%>% group_by(full_name)%>% filter(dates == max(dates))%>% filter(rank == max(rank))%>%
    filter(date_diff > 7)  %>%
    as.data.frame()
  sum_counts = get_counts_day %>% summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>5)*1))
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_by_group_month_west_exits = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  get_counts_day = df_1%>% group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    subset( date_diff > 7)  %>%
    as.data.frame()
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe') )
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  
  
  get_counts_day_1[,'unit_type'] = unlist(lapply(strsplit(get_counts_day_1[,'unit'], ' '), function(x) x[1]))
  sum_counts = get_counts_day_1%>% group_by(unit) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1)) %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_by_group_month_west = do.call(rbind,counts_by_group_month_west)
counts_by_group_month_west_rm_28_days = do.call(rbind,counts_by_group_month_west_rm_28_days)
counts_by_group_month_west_entries = do.call(rbind,counts_by_group_month_west_entries)
counts_by_group_month_west_exits = do.call(rbind,counts_by_group_month_west_exits)

counts_by_group_month_west_unit_type = counts_by_group_month_west %>% 
  group_by(unit_type, date) %>%
  summarise(total_kills = sum(total_kills), pilots_w_kills = sum(pilots_w_kills), aces = sum(aces)) %>%
  as.data.frame()
  
ggplot(subset(.\), aes(x = date, y= gt_10_15, fill=unit_type))+geom_bar(position = 'stack', stat = 'identity')
ggplot(counts_by_group_month_west[which(counts_by_group_month_west[,'unit_type'] == 'JG'),], 
       aes(x = date, y= gt_10_15, fill=unit)) + 
  geom_line() +
  facet_grid(unit~.)

ggplot(counts_by_group_month_west_rm_28_days, aes(x = date, y= gt_20))+geom_line()+facet_wrap(unit_type~.)
counts_by_group_month_west_melt = melt(counts_by_group_month_west, id.vars = c('unit_type','date'))
ggplot(subset(counts_by_group_month_west_melt, variable !='total_kills'), aes(x = date, y= value, color = variable))+geom_line()+facet_wrap(unit_type~.)

counts_by_group_month_west_exits_1 = counts_by_group_month_west_exits %>% 
  group_by(unit) %>% 
  mutate(total_kills_diff = c(0, diff(total_kills)),
         pilots_w_kills_diff = c(0, diff(pilots_w_kills)),
         aces_diff = c(0, diff(aces))) %>% as.data.frame()
counts_by_group_month_west_exits_agg_yearmon = counts_by_group_month_west_exits_1 %>% mutate(yearmon = as.yearmon(date))
counts_by_group_month_west_exits_agg_yearmon = counts_by_group_month_west_exits_agg_yearmon %>% group_by(yearmon) %>%
  summarise(total_kills_diff=sum(total_kills_diff) , pilots_w_kills_diff=sum(pilots_w_kills_diff), aces_diff=sum(aces_diff)) %>%
  as.data.frame()

ggplot(counts_by_group_month_west_exits_1, aes(x = date, y= aces_diff, color = unit))+geom_line()+facet_wrap(unit~., scale = 'free')
ggplot(subset(counts_by_group_month_west_exits_agg_yearmon, aces_diff > -1), 
       aes(x = yearmon, y= aces_diff))+geom_line()#+facet_wrap(unit~., scale = 'free')

ggplot(counts_by_group_month_west, aes(x = date, y= pilots_w_kills))+geom_line()+facet_wrap(unit_type~.)


ggplot(counts_by_group_month_west_exits, aes(x = date, y= pilots_w_kills, colour))+geom_line()+facet_wrap(unit~.)

counts_by_group_month_west_reshape = reshape(counts_by_group_month_west, )
ggplot(counts_by_group_month_west_unit_type, aes(x = date, y= aces))+geom_line()+facet_wrap(unit_type~.)

write.csv(counts_by_group_month_west, 'C:/Users/Larry/Google Drive/8th_air_force_data/luftwaffe_pilot_kill_data_aggregated_reich_defense.csv', row.names = FALSE)
write.csv(counts_by_group_month_west_rm_28_days, 'C:/Users/Larry/Google Drive/8th_air_force_data/luftwaffe_pilot_kill_data_aggregated_reich_defense_28_days.csv', row.names = FALSE)

heavy_kills_by_yearmon = df_1 %>% mutate(spitfire = (aircraft == 'Spitfire')*1+(aircraft == 'Typhoon')*1) %>% group_by(yearmon,unit,gruppe) %>% 
  summarise(heavy_kills = sum(heavy), spitfire = sum(spitfire)) %>%
  as.data.frame()
library(reshape)
heavy_kills_by_yearmon_subset = heavy_kills_by_yearmon %>% filter(unit %in% unique(counts_by_group_month_west[,1]))
heavy_kills_by_yearmon_melt = melt(heavy_kills_by_yearmon_subset, id.vars = c('yearmon','unit','gruppe'))
colnames(heavy_kills_by_yearmon_melt) = c('yearmon','unit','gruppe','ac_type','num_killed')

ggplot(subset(heavy_kills_by_yearmon_melt,yearmon > '1943-06-06' & yearmon <'1944-09-01'), 
       aes(x = yearmon, y = num_killed, colour = ac_type,))+
      geom_line()+facet_grid(unit~gruppe, scales = 'free')+geom_vline(xintercept = as.yearmon('Jun 1944'))


jg53_by_unit_front =df_1 %>% 
 # subset(aircraft == 'B-17' | aircraft == 'B-24') %>%
  group_by(unit, Unit, front, yearmon)%>%
  subset(yearmon > 'May 1943' & yearmon < 'Oct 1944') %>%
  summarise(counts = n())%>% 
  subset(unit == "JG 53")%>%
  arrange(Unit, front, yearmon) %>%as.data.frame()

ggplot(jg53_by_unit_front,
       aes(x = yearmon, y = counts, group = front, colour = front))+
  geom_point()+facet_wrap(Unit~.)

df[,'reich_defense'] =  

counts_by_group = do.call(rbind,counts_by_group)

counts_by_group_month = df_1 %>% group_by(unit,yearmon) %>% summarise(count = n()) %>% as.data.frame()
counts_by_group_month = do.call(rbind,counts_by_group_month)
counts_by_group_month = as.data.frame(counts_by_group_month)
counts_by_group_month[,'unit'] = factor(counts_by_group_month[,'unit'])
counts_by_group_month_subset = merge(counts_by_group_month, claims_by_unit , 
                                     on = 'unit')
counts_by_group_month_subset[,'unit'] = factor(counts_by_group_month_subset[,'unit'], (counts_by_group_month_subset %>% select(unit, n_claimed)%>% unique() %>% arrange(-n_claimed) %>% select(unit))[,1])
ggplot(subset(counts_by_group_month_subset, n_claimed > 100), aes(x = date, y= aces, colour = front))+geom_line()+facet_wrap(unit~.,scales = 'free')
ggplot(subset(counts_by_group_month_subset, n_claimed > 100 & date > '1943-06-01' & date < '1944-08-01'), aes(x = date, y= aces, colour = front))+geom_line()+facet_wrap(unit~.,scales = 'free')

#ggplot(counts_by_group, aes(x = date, y= total_kills, colour = unit))+geom_line()



par(mfrow=c(2,1))
plot(counts_aces[,c(4,3)])
claims_by_fighter_group = df_1 %>% group_by(unit,yearmon) %>% summarise(count = n()) %>% as.data.frame()



claims_by_fighter_group = df_1 %>% group_by(aircraft,unit,yearmon) %>% summarise(count = n()) %>% as.data.frame()
claims_by_fighter_group


ggplot(subset(claims_by_fighter_group, aircraft == 'B-17' | claims_by_fighter_group == 'B-24'| 
                claims_by_fighter_group == 'P-47'|claims_by_fighter_group == 'P-51'|claims_by_fighter_group == 'Typhoon'), 
       aes(x = yearmon, y = count, group = fighter_group_1  , colour = fighter_group_1  ))+geom_line()+
  facet_grid(aircraft_1~., scales = 'free')



ggplot(b_17_unit_monthly, 
       aes(x = yearmon, y = count, group = unit  , colour = unit  ))+geom_line()+
  facet_grid(aircraft~., scales = 'free')

claims_by_fighter_group_time = subset(df_1, yearmon > 'Jun 1943' & yearmon < 'Sep 1944') %>% group_by(yearmon, aircraft,unit,gruppe) %>% summarise(count = n()) %>% as.data.frame()

claims_by_fighter_group_wide = reshape(claims_by_fighter_group_time %>% 
                                      filter (aircraft %in% c("B-17","B-24",'P-51','P-47', 'Typhoon', 'Spitfire')), 
                                       timevar = 'aircraft', idvar = c('unit','yearmon','gruppe'),direction = 'wide')


claims_by_fighter_group_wide[is.na(claims_by_fighter_group_wide)] = 0
colnames(claims_by_fighter_group_wide) = gsub('count.','',colnames(claims_by_fighter_group_wide))


claims_by_fighter_group_wide_1 = claims_by_fighter_group_wide[-grep('NJG',claims_by_fighter_group_wide[,'unit']),]
claims_by_fighter_group_wide_1 = claims_by_fighter_group_wide_1[grep('JG',claims_by_fighter_group_wide_1[,'unit']),]

subset(claims_by_fighter_group_wide_1, unit == 'JG 27') %>% arrange(gruppe, yearmon)

yearmon_fighter_groups = t(claims_by_fighter_group_wide_1[,-c(1:2)])
colnames(yearmon_fighter_groups) = paste(claims_by_fighter_group_wide_1[,2],claims_by_fighter_group_wide_1[,1],sep = '_')
hc = hclust(as.dist(1-(cor(claims_by_fighter_group_wide[,-c(1:2)])+1)/2))
hc = hclust(as.dist(1-(cor(yearmon_fighter_groups)+1)/2))

plot(hc)

yearmon_fighter_groups_t = data.frame(t(yearmon_fighter_groups))
yearmon_fighter_groups_t[,'unit'] = claims_by_fighter_group_wide_1[,2]
yearmon_fighter_groups_t[,'yearmon'] = claims_by_fighter_group_wide_1[,1]
View(yearmon_fighter_groups_t %>% group_by(unit) %>% arrange(unit,yearmon) %>% as.data.frame())

##

lw_aces = read.csv('/users/sweiss/downloads/lw_aces_text.txt')
lw_aces = mutate(lw_aces, full_name = paste0(First.Name, Pilot.Name))

test = lapply(lw_aces[,'Remarks'], function(q) na.omit(do.call(c,(lapply(strsplit(q,' '), function(D) lubridate::dmy(D) )))))
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
