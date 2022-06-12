library(ggplot2)
library(dplyr)
library(reshape)


ac_prod_30s = read.csv('data/additional_plots/annual_aircraft_production_germany_31_39.csv')
scaleFUN <- function(x) sprintf("%.2f", x)
library(ggplot2); library(scales)

ac_prod_30s_plot = ac_prod_30s %>% ggplot(aes(x = Year, y = round(aircraft_produced))) + geom_line()+
  xlab('Year') + ylab('Number of Aircraft Produced') +
  ggtitle('German Aircraft Production by Year') + theme_minimal()+
  scale_x_continuous(breaks= pretty_breaks(n=9))+ 
  theme(legend.position="bottom", text = element_text(size = 5), panel.grid.minor = element_blank())


ggsave(ac_prod_30s_plot, file="plots/ac_prod_30s.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)
##

library(RColorBrewer)

#define custom color scale
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00")

names(colorBlindBlack8) <- c( "Germany", "France","UK", "USA", "Italy", "USSR", "Austria")
custom_colors <- scale_colour_manual(name = "Country", values = colorBlindBlack8)


unemployment_gd = read.csv('data/additional_plots/unemployment_great_depression.csv')
colnames(unemployment_gd)[1] = "Year"
colnames(unemployment_gd)[5] = "USA"

unemployment_gd_melt = melt(unemployment_gd[,1:5], id.vars = c('Year'))
colnames(unemployment_gd_melt) = c('Year', 'Country', 'Unemployment_Rate')
unemployment_great_depression_plot = unemployment_gd_melt %>% ggplot(aes(x = Year, y = Unemployment_Rate, group = Country, colour = Country)) + 
  geom_line(size = 1) + theme_minimal() + scale_colour_manual(name = "Country", values = colorBlindBlack8[1:4]) + 
  ylab('Unemployment Rate (%)') + ggtitle("Unemployment During the Great Depression")+
  theme(legend.position="bottom", text = element_text(size = 5))


ggsave(unemployment_great_depression_plot, file="plots/unemployment_great_depression_plot.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)


gdp_capita_1938 = read.csv('data/additional_plots/gdp_capita_1938.csv')
colnames(gdp_capita_1938)[1] = 'Country'
gdp_capita_1938 = gdp_capita_1938 %>% mutate(Country = factor(Country, levels = gdp_capita_1938[order(-gdp_capita_1938[,'gdp_capita']), 'Country']))
gdp_capita_1938_plot = gdp_capita_1938 %>% ggplot(aes(x = Country, y = gdp_capita, fill = Country)) + geom_bar(stat = "identity")+
  scale_fill_manual(name = "Country", values = colorBlindBlack8[1:6])+
  theme(legend.position="None", text = element_text(size = 5)) +
  ggtitle('GDP / Capita Country Comparison 1938') + 
  ylab('GDP / Capita in $ (1990 prices)')
ggsave(gdp_capita_1938_plot, file="plots/gdp_capita_1938_plot.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)



gdp_countries_1939_1945 = read.csv('data/additional_plots/gdp_countries_1939_1945.csv')
colnames(gdp_countries_1939_1945)[2] = 'USA'
colnames(gdp_countries_1939_1945)[1] = 'Year'
gdp_countries_1939_1945_melt = melt(gdp_countries_1939_1945, id.vars = "Year")
colnames(gdp_countries_1939_1945_melt) = c("Year", "Country", "GDP")

gdp_countries_1939_1945_melt %>% ggplot(aes(x = Year, y = GDP, group = Country, colour = Country)) + 
  geom_line(size = 1.5) + theme_minimal() + scale_colour_manual(name = "Country", values = colorBlindBlack8[-2]) + 
  ylab('GDP $ (1990 prices)') + ggtitle("GDP")+
  theme(legend.position="bottom", text = element_text(size = 20), )

c(as.character(unemployment_gd_melt[,'Country']),
  as.character(gdp_capita_1938[,'Country']),
  as.character(gdp_countries_1939_1945_melt[,'Country'])) %>% unique()



plane_records = read.csv('data/additional_plots/plane_speed_records.csv')
plane_records = plane_records %>% mutate(date = as.Date(Date, '%m/%d/%Y'))
plane_records = do.call(rbind,lapply(plane_records[,'date'], function(x) plane_records %>% subset( date <= x) %>% top_n(1,mph)))
plane_records = unique(plane_records)
ggplot(plane_records, aes(x = date, y = as.numeric(mph), label = Country)) + geom_text_repel() +geom_step()
plane_records[which(plane_records[,'Country'] == 'US'),'Country'] = 'USA'


airplaine_speed_records = ggplot() + geom_text_repel(data = plane_records, aes(x = date, y = as.numeric(mph), label = Country, colour = Country), max.overlaps= 100, label.size = .5, size = 1) + 
  geom_step(data = plane_records, aes(x = date, y = as.numeric(mph)))+
  theme_minimal() + theme(text = element_text(size=5),legend.position = 'None') +
  ylab('Aircraft Speed Record (MPH)') +
  xlab('Date (Year)') +
  ggtitle('Aircraft Speed Records by Date and Country') + 
  scale_colour_manual(name = "Country", values = colorBlindBlack8[1:5]) 

ggsave(airplaine_speed_records, file="plots/airplaine_speed_records.png",width = 1000, height = 1000/((1+sqrt(5))/2), units = "px",limitsize = FALSE)




