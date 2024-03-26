
#number of pilots (or aces) who had claims in the last x days 
#germns more effective at shooting down night planes than day planes 
#altitude of combat (probably higher in west)
#


sequences = seq(50,8300,50)
usaaf_sites = sapply(sequences, function(x) 
  paste0("http://www.aircrewremembered.com/AlliedLossesIncidents/?s=",x,"&q=&qand=USAAF&exc1=&exc2=&search_type=exact&search_only="))
usaaf_sites = c( "http://www.aircrewremembered.com/AlliedLossesIncidents/?s=1&q=&qand=USAAF&exc1=&exc2=&search_type=exact&search_only=", usaaf_sites)
website = "http://www.aircrewremembered.com/AlliedLossesIncidents/?s=50&q=&qand=USAAF&exc1=&exc2=&search_type=exact&search_only="


usaaf_sites_random_orderd = sample(usaaf_sites, length(usaaf_sites))
usaaf_downloaded_sites = c()

for(site in usaaf_sites_random_orderd){
  #Sys.sleep(rnorm(1,5,1)) 
  print(site)
  temp_site = read_html(site) 
  usaaf_downloaded_sites = c(usaaf_downloaded_sites, list(temp_site))
  
}




save(usaaf_downloaded_sites, file = '/users/sweiss/downloads/alliedlosses_usaaf.rdata')
lapply(usaaf_sites,  )
load('/users/sweiss/downloads/alliedlosses_usaaf.rdata')

library(rvest)
library(splitstackshape)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)


headers_path = 'th'
data_path = '//td'


temp_data = usaaf_downloaded_sites[[2]]
preprocess_data = function(temp_data){
  Name = html_nodes(temp_data, '.Name') %>% html_text()
  FirstNames = html_nodes(temp_data, '.FirstNames') %>% html_text()
  Title = html_nodes(temp_data, '.Title') %>% html_text()
  Rank = html_nodes(temp_data, '.Rank') %>% html_text()
  Born = html_nodes(temp_data, '.Born') %>% html_text()
  Nationality = html_nodes(temp_data, '.Nationality') %>% html_text()
  Role = html_nodes(temp_data, '.Role') %>% html_text()
  Awards = html_nodes(temp_data, '.Awards') %>% html_text()
  AirForce = html_nodes(temp_data, '.AirForce') %>% html_text()
  Command = html_nodes(temp_data, '.Command') %>% html_text()
  Unit = html_nodes(temp_data, '.Unit') %>% html_text()
  DateIncident = html_nodes(temp_data, '.DateIncident') %>% html_text()
  Aircraft = html_nodes(temp_data, '.Aircraft') %>% html_text()
  Type = html_nodes(temp_data, '.Type') %>% html_text()
  Serial = html_nodes(temp_data, '.Serial') %>% html_text()
  Code = html_nodes(temp_data, '.Code') %>% html_text()
  Victories = html_nodes(temp_data, '.Victories') %>% html_text()
  Base = html_nodes(temp_data, '.Base') %>% html_text()
  Incident = html_nodes(temp_data, '.Incident') %>% html_text()
  Fate = html_nodes(temp_data, '.Fate') %>% html_text()
  Notes = html_nodes(temp_data, '.Notes') %>% html_text()
  Service_NO = html_nodes(temp_data, '.ServiceNo') %>% html_text()
  
  
  df_to_return = data.frame(Name = Name, 
                            FirstNames = FirstNames,
                            Title = Title, 
                            Rank = Rank,
                            Nationality = Nationality, 
                            Role = Role, 
                            Awards = Awards, 
                            AirForce = AirForce, 
                            Command = Command, 
                            Unit = Unit, 
                            DateIncident = DateIncident, 
                            Aircraft = Aircraft, 
                            Type = Type, 
                            Serial = Serial, 
                            Code = Code, 
                            Victories = Victories, 
                            Base = Base, 
                            Incident = Incident, 
                            Fate = Fate, 
                            Notes = Notes, 
                            Service_NO = Service_NO)
  return(df_to_return)    
  
  
  
  
  
  
}

processed_data = lapply(usaaf_downloaded_sites, preprocess_data)


processed_data = do.call(rbind, processed_data)
save(processed_data, file = '/users/sweiss/downloads/alliedlosses_usaaf.rdata')

data <-html_nodes(temp_data, xpath = '//td')
  
library(pdftools)
us_kills = pdftools::pdf_text(pdf = "http://aces.safarikovi.org/victories/doc/usaf.aerial.victory.credits-wwii-by.name.pdf")

us_kills_df_list = lapply(us_kills, function(x) read.csv(text = x, header= FALSE))
us_kills_df = do.call(rbind,us_kills_df_list)
col_locs = c(1, 24, 43, 52, 63, 76, 87, 125, 129)
1:(length(col_locs) - 1)
temp_string = us_kills_df[2,]
lapply(1:(length(col_locs) - 1),  function(x) substr(temp_string, col_locs[x], col_locs[x+1])) 


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
grouped_kills_date_europe %>% ggplot(aes(x = yearmonth, y = credits)) + geom_line()


grouped_kills_date_unit = usaaf_victory_credits_adj %>% group_by(yearmonth,Theater,Unit_Echelon) %>% summarise(credits = sum(as.numeric(as.character(Credit)), na.rm = TRUE))  %>% as.data.frame() 
grouped_data %>% subset(Theater=='ETO' & Unit_Echelon == '335 Fighter Squadron') %>%
  ggplot( aes(x = yearmonth, y = credits, group = Unit_Echelon, colour = Unit_Echelon)) + geom_line() 


usaaf_victory_credits_adj %>% group_by(Theater,Unit_Echelon) %>% 
  summarise(credits = sum(as.numeric(as.character(Credit)), na.rm = TRUE))  %>% as.data.frame() %>% 
  subset(Theater=='ETO') %>% arrange(-credits)

temp_site = read_html('http://www.joebaugher.com/usaf_serials/1943_2.html') 

text = html_nodes(temp_site, 'pre') %>% html_text()
txtjson <- paste(sapply(text, jsonlite::toJSON, pretty = TRUE), collapse = "\n")


