
# 2022 update to Andrijevic et al. 2019

# # #         Governance in socio-economic pathways and its role              # # # 
# # #                  for future adaptive capacity                           # # # 
# # #                    (Andrijevic et al., 2019)                            # # # 
# # #                                                                         # # # 
# # #                           Data management                               # # #

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(broom)
library(countrycode)
library(readstata13)
library(sandwich)
library(lmtest)
library(zoo)
library(RCurl)
library(gdata) 
library(readxl)
library(wcde)

# Historical data

# World Governance Indicators (2022 update)

# Function to standardize the values from 0 to 1
range01 <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))} 

wgi <- read.dta13('data/wgidataset.dta') %>% 
  select(code, countryname, year, contains('e'), -ges, -gen, -ger, -gel, -geu) %>% 
  rename(voic.ac = vae,
         pol.stab = pve,
         gov.eff = gee,
         reg.qual = rqe,
         ru.law = rle,
         corr.cont = cce,
         countrycode = code) %>% 
  gather(var, value, -countrycode, -countryname, -year) %>% 
  group_by(var) %>% 
  mutate(value = range01(value)) %>% 
  ungroup() %>% 
  mutate(year = as.integer(year)) %>% 
  spread(var, value) %>% 
  mutate(governance = rowMeans(select(., voic.ac, pol.stab, gov.eff, reg.qual, ru.law, corr.cont))) %>% # # Governance variable as the arithmetic average of the six components of WGI
  mutate(scenario = 'Observed')


levels(wgi$year)[levels(wgi$year)== 1996] <- 1995
wgi$countrycode <- recode(wgi$countrycode, 
                          "ROM" = "ROU",
                          "ZAR" = "COD") #Romania and Democratic Republic of the Congo had outdated codes



#vdem.data <- readRDS('data/V-Dem-CY-Core-v12.rds')


# ND GAIN Readiness component

nd.readiness <- read.csv('data/ndgain_readiness.csv') %>% 
  rename(countrycode = ISO3, 
         country = Name) %>% 
  gather(year, readiness, -countrycode, -country) %>% 
  mutate(year = year %>% str_replace("X", "") %>% as.numeric,
         scenario = 'Observed') %>% 
  filter(year <= 2015)


# GDP from from Leimbach et al. (forth) (1998 - 2020) 

gdp.yearly <- read_excel("data/scenarios_for_navigate5.xlsx", sheet = "GDP_2005") %>% 
  select(-c(scenario, variable, unit)) %>% 
  gather(year, gdp, -countryCode) %>% 
  filter(year < 2021) %>% 
  rename(gdppc = gdp, countrycode = countryCode) %>% 
  mutate(scenario = 'Observed', 
         year = as.integer(year), 
         gdppc = as.numeric(gdppc)) %>% 
  drop_na(gdppc)


# Education data: share of population by educational attainment and gender gap in education measured by the difference in mean years of schooling (MYS) between women and men (source: Wittgenstein Centre for Demography and Global Human Capital)

edu.online <- get_wcde(indicator = 'prop', scenario = 1:3, include_scenario_names = T) %>% 
  filter(year > 1969 & age == "All" & sex == "Both") %>% 
  select(name, country_code, year, scenario_abb, education, prop)

edu.prep <- edu.online %>% 
  as.data.frame() %>% 
  spread(year, prop) %>% 
  rename(scenario = scenario_abb)

mys.gap.prep <- get_wcde(indicator = 'ggapmys15',  scenario = 1:3, include_scenario_names = T) %>% 
  as.data.frame() %>% 
  mutate(dummyvar = 1) %>% 
  spread(year, ggapmys15) %>% 
  select(-c(scenario, scenario_name)) %>% 
  rename(scenario = scenario_abb)


ipolate2 <- function(mat) {
  yrs <- 1970:2099
  ys <- as.numeric(unlist(names(mat)))
  mat1 <- array(dim = c(dim(mat)[1], length(yrs)))
  est <- seq(1970, 2100, 5)
  for (i in 1:length(yrs)) {
    y = yrs[i]
    if(y %in% names(mat) == T) {
      mat1[,i] <- as.numeric(mat[,which(names(mat) == y)])
    } else {
      z <- y-est
      yl <- est[which(z == min(z[z>0]))]
      y5 <- yl + 5
      el <- as.numeric(mat[,which(names(mat) == yl)])
      eu <- as.numeric(mat[,which(names(mat) == y5)])
      if(y > max(ys, na.rm = T)) { mat1[,i] <- el
      } else { mat1[,i] <- el + (eu-el)*(y-yl)/5}
    }
  }
  mat1 <- data.frame(mat[,1:4], mat1)
  names(mat1)[5:dim(mat1)[2]] <- yrs
  return(mat1)
}

edu.ipol <- ipolate2(edu.prep) %>% 
  gather(year, distribution, -name, -scenario, -country_code, -education) %>% 
  mutate(education = str_replace(str_to_lower(education), fixed(" "), ".")) %>% 
  spread(education, distribution)

mys.gap.ipol <- ipolate2(mys.gap.prep) %>% 
  gather(year, mys.gap, -name, -scenario, -country_code, -dummyvar)%>% 
  mutate(year = year %>% str_replace("X", "") %>% as.factor)

edu.master <- edu.ipol %>% 
  left_join(mys.gap.ipol) %>%
  filter(country_code < 900 & scenario == "SSP2") %>% 
  mutate(countrycode = countrycode(name, 'country.name', 'iso3c'), 
         year = as.integer(year), 
         scenario = recode(scenario, "SSP2" = "Observed")) 


# Merge into one dataset

observed.yr <- wgi %>% 
  left_join(gdp.yearly, by = c('countrycode', 'year', 'scenario')) %>% 
  left_join(edu.master, by = c('countrycode', 'year', 'scenario')) %>%
  #left_join(nd.readiness, by = c('countrycode', 'year', 'scenario')) %>%
  #select(-Area) %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate(year = as.integer(year),
         lngdp = log(gdppc))

observed.yr$countrycode <- recode(observed.yr$countrycode, "ROM" = "ROU", "ZAR" = "COD") # Fix outdated country names

#write.csv(observed.yr, 'data/observed_yr.csv')

# Projections data

gdp.proj <- read_excel("data/scenarios_for_navigate5.xlsx", sheet = "GDP_2005") %>% 
  select(-c(variable, unit)) %>% 
  gather(year, gdp, -countryCode, -scenario) %>% 
  filter(year > 2019 & !scenario == "WDI") %>% 
  rename(gdppc = gdp, countrycode = countryCode) %>% 
  mutate(year = as.integer(year), 
         gdppc = as.numeric(gdppc))


# Education projections

edu.proj <- get_wcde(indicator = 'prop', scenario = 1:3, include_scenario_names = T) %>% 
  filter(year > 2015 & age == "All" & sex == "Both") %>% 
  select(name, country_code, year, scenario_abb, education, prop) %>% 
  mutate(education = str_replace(str_to_lower(education), fixed(" "), ".")) %>% 
  spread(education, prop) %>% 
  rename(scenario = scenario_abb)

mys.gap.proj <- get_wcde(indicator = 'ggapmys15', scenario = 1:3, include_scenario_names = T) %>% 
  as.data.frame() %>% 
  rename(mys.gap = ggapmys15) %>% 
  select(-c(scenario, scenario_name)) %>% 
  rename(scenario = scenario_abb)


edu.proj.master <- inner_join(edu.proj, mys.gap.proj, by = c('name', 'country_code', 'year', 'scenario')) %>%
  filter(country_code < 900) %>% 
  mutate(countrycode = countrycode(name, 'country.name', 'iso3c')) %>% 
  rename(country = name,
         year = year) %>% 
  filter(year > 2015)

# Merge all data into a master dataset for projections

projections <- gdp.proj %>% 
  inner_join(edu.proj.master, by=c('countrycode', 'year', 'scenario')) %>% 
  mutate(countrycode = countrycode %>% as.factor) %>%
  mutate(lngdp = log(gdppc)) %>% 
  arrange(countrycode, year, scenario) %>% 
  mutate(ID = paste(countrycode, year, scenario))

projections <- projections[!duplicated(projections$ID), ] %>% 
 filter(year >= 2015)

#Interpolate for yearly values

ipolate3 <- function(mat) {
  yrs <- 2020:2099
  ys <- as.numeric(unlist(names(mat)))
  mat1 <- array(dim = c(dim(mat)[1], length(yrs)))
  est <- seq(2020, 2100, 5)
  for (i in 1:length(yrs)) {
    y = yrs[i]
    if(y %in% names(mat) == T) {
      mat1[,i] <- as.numeric(mat[,which(names(mat) == y)])
    } else {
      z <- y-est
      yl <- est[which(z == min(z[z>0]))]
      y5 <- yl + 5
      el <- as.numeric(mat[,which(names(mat) == yl)])
      eu <- as.numeric(mat[,which(names(mat) == y5)])
      if(y > max(ys, na.rm = T)) { mat1[,i] <- el
      } else { mat1[,i] <- el + (eu-el)*(y-yl)/5}
    }
  }
  mat1 <- data.frame(mat[,1:3], mat1)
  names(mat1)[4:dim(mat1)[2]] <- yrs
  return(mat1)
}

edu.projections.ipol <- edu.proj.master %>%
  as.data.frame() %>% 
  select(-country_code, -country) %>%
  gather(variable, value, -countrycode, -scenario, -year) %>%
  spread(year, value) %>%
  ipolate3()

 projections.yr <- edu.projections.ipol %>%
   gather(year, value, -countrycode, -scenario, -variable) %>%
   spread(variable, value) %>%
   mutate(year = year %>% as.numeric()) %>% 
   left_join(gdp.proj, by = c('countrycode', 'year', 'scenario'))


#write.csv(projections.yr, 'data/projections_yr.csv')
