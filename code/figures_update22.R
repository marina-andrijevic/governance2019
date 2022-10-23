
# # #         Governance in socio-economic pathways and its role          # # # 
# # #                  for future adaptive capacity                       # # # 
# # #                    (Andrijevic et al., 2019)                        # # # 
# # #                                                                     # # # 
# # #                          Figures                                    # # #

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(countrycode)
library(viridis)
library(ggthemes)
library(maps)
library(rworldmap)

setwd('/Users/marinaandrijevic/PhD/Governance Projections/GitHub')

# Figure 1 ----

# For maps: 

# Load the dataset with both observed and projected data for all variables projected in the paper: 
# governance, control of corruption, government effectiveness, ND GAIN readiness component

dat <- read.csv('data/master_proj_obs.csv') %>%
  filter(year == 2050 & scenario == 'SSP1') %>% # Choose the period and the scenario 
  rename(var = governance) # Choose the variable that is going to be shown on the map


# Merge the country data with maps

map <- joinCountryData2Map(dat, joinCode = "ISO3", nameJoinColumn = "countrycode")
map_poly <-  fortify(map) %>% 
  merge(map@data, by.x="id", by.y="ADMIN", all.x=T) %>%
  arrange(id, order) %>% 
  mutate(var %>% as.numeric())

ggplot(map_poly, aes( x = long, y = lat, group = group )) +
  coord_map(projection = 'mollweide', xlim = c(-180, 180), ylim = c(-60, 75))  + # Remove antarctica
  geom_polygon(aes(fill = var)) +
  scale_fill_viridis() +
  labs(fill = 'Value'
       ,title = 'Governance in 2050 - SSP1'   # Change the title of the map
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Helvetica', color = 'gray40')
        ,plot.title = element_text(size = 18)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'white')
        ,plot.background = element_rect(fill = 'white')
        ,legend.position = c(.08,.26)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = ''
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Helvetica'
           ,color = 'gray50'
           ,hjust = 'left'
  )


# Individual countries' panels:

#results <- read.csv('data/master_proj_obs.csv')

sample <- c("COL", "DEU", "JPN", "MEX", "NGA", "IND", "SOM", "SYR") 

ggplot() +
  geom_point(dat = filter(observed.yr, year < 2021 & countrycode %in% sample), aes(x = year, y = governance, color = scenario), size = 2) +
  geom_line(dat = filter(test, year >= 2015 & countrycode %in% sample), aes(x = year, y = gov, color = scenario), size = 1.2) +
  geom_line(dat = filter(test, year >= 2015 & countrycode %in% sample), aes(x = year, y = gov.const.fe, color = scenario), size = 1.2, linetype = 'dashed') +
  facet_wrap(~countrycode) +
  ylim(0,1) +
  labs(x = 'Year', y = 'WGI', title = 'Projections of governance by SSPs') + # Change the title based on the variable shown
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "#F6F7F6"),
        panel.border = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("SSP1" = "#00BA38", "SSP2" = "#F564E3", 
                                'SSP3' = '#FF8C00', 'SSP4' = '#00BFC4', 'SSP5' = '#AED602',"Observed" = "#F8766D")) 


test <- projections.ext %>% drop_na(gov)

View(test)

# Figure 2 ----

# a)

results <- read.csv('data/master_proj_obs.csv')

pres <- results %>% filter(year == 2020 & scenario == 'Observed') %>% select(governance)

q1 <- quantile(pres$governance, 0.25, na.rm = T) 
q2 <- quantile(pres$governance, 0.5, na.rm = T)
q3 <- quantile(pres$governance, 0.75, na.rm = T)
q4 <- quantile(pres$governance, 0.9, na.rm = T)

groups.graph <- results %>% 
  mutate(category = ifelse(governance < q1, "Very weak", 
                           ifelse(governance >= q1 & governance < q2, "Weak",
                                  ifelse(governance >= q2 & governance < q3, "Medium",
                                         ifelse(governance >= q3 & governance < q4, "Good", "Very good"))))) %>% 
  mutate(category = factor(category, levels = c("Very good", "Good", "Medium" ,"Weak", "Very weak"), ordered = T))

ggplot(groups.graph %>% filter(year == 2050)) +  #Select the year (2050 or 2095)
  geom_bar(aes(scenario, fill = category), width = 0.3) + 
  theme_hc() +
  scale_fill_viridis(discrete = T, direction = -1)

# b)

popsize <- read.csv('data/pop_size.csv', skip = 8) %>% 
  mutate(countrycode = countrycode(Area, 'country.name', 'iso3c')) %>% 
  rename(scenario = Scenario, year = Year, population = Population)

popweight <- groups.graph %>% 
  inner_join(popsize, by = c("countrycode", "year", "scenario")) %>% 
  mutate(category = ifelse(governance < q1, "Very weak", 
                           ifelse(governance >= q1 & governance < q2, "Weak",
                                  ifelse(governance >= q2 & governance < q3, "Medium",
                                         ifelse(governance >= q3 & governance < q4, "Good", "Very good")))))

prep <- popweight %>% filter(year == 2050) %>% # Select the year (2050 or 2095)
  group_by(scenario, category) %>% 
  mutate(popsum = sum(population)) %>% 
  select(scenario, category, popsum) %>% 
  mutate(ID = paste(category, scenario)) %>% 
  ungroup() %>% 
  mutate(category = factor(category, levels = c("Very good", "Good", "Medium" ,"Weak", "Very weak"), ordered = T)) %>%
  filter(!duplicated(ID)) %>% 
  arrange(popsum) %>% 
  mutate(popsum = popsum/1000000)


ggplot(data = prep, aes(factor(scenario), popsum, fill = category)) +
  geom_bar(stat = 'identity', width = 0.3) +
  labs(x = 'Scenario', y = 'Population size (billion)', title = 'Population in governance groups in 2050') +
  theme_hc() +
  scale_fill_viridis(discrete = T, option = 'plasma', direction = -1) +
  scale_y_continuous(breaks=seq(1, 12, 1)) +
  coord_cartesian(ylim=c(1,12))


# Figure 3 ----

roc <- groups.graph %>% # See Fig 2 for groups.graph
  filter(!category %in% c('Good', 'Very good') & (year%%5 == 0) & scenario %in% c('SSP1', 'SSP2', 'SSP3')) %>% 
  group_by(category, countrycode, scenario) %>% 
  select(category, countrycode, scenario, year, governance) %>% 
  mutate(ch = (governance - lag(governance))/lag(governance)*100) %>% 
  ungroup() %>% 
  filter(year%%10 == 0)

ggplot(roc, aes(as.factor(year), ch, fill = scenario)) +
  geom_boxplot(varwidth = FALSE, outlier.alpha = 0.3) +
  labs(x = 'Year', y = 'Rate of change', fill = 'Scenario') +
  ylim(-3,30) +
  scale_fill_brewer(palette="Set2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~category)


# Figure 4 ----

results <- read.csv('data/master_proj_obs.csv')

sample <- c("IND", "SOM", "SYR")

ggplot() +
  geom_point(data = filter(results, year < 2015 & countrycode %in% sample), aes(x = year, y = readiness, color = scenario), size = 1.7) +
  geom_line(data = filter(results, year >= 2015 & countrycode %in% sample), aes(x = year, y = readiness, color = scenario), size = 1.5) +
  facet_wrap(~countrycode) +
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.52, 0.80, 1)) +
  ylim(0,1) +
  labs(x = 'Year', y = 'ND GAIN readiness', title = 'Projections of the ND GAIN readiness component by SSPs', fill = 'Scenario') +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "#F6F7F6"),
        panel.border = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Observed" = "#F8766D", "SSP1" = "#00BA38", "SSP2" = "#F564E3",
                                'SSP3' = '#FF8C00', 'SSP4' = '#00BFC4', 'SSP5' = '#AED602'))



