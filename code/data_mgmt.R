
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
# library(RCurl)
# library(gdata) 

setwd('/Users/marinaandrijevic/PhD/Governance Projections/GitHub')

# Historical data

# World Governance Indicators

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

# ND GAIN Readiness component

nd.readiness <- read.csv('data/ndgain_readiness.csv') %>% 
  rename(countrycode = ISO3, 
         country = Name) %>% 
  gather(year, readiness, -countrycode, -country) %>% 
  mutate(year = year %>% str_replace("X", "") %>% as.numeric,
         scenario = 'Observed') %>% 
  filter(year <= 2015)


# GDP from Penn World Tables 7.0 (until 2010) and SSP projections (2010 - 2015) (Crespo Cuaresma, 2017)

gdp.pwt <- read.csv("data/pwt70.csv", header = T, sep = ",") %>% 
  select(isocode, year, rgdpl) %>% 
  rename(gdppc = rgdpl, countrycode = isocode) %>% 
  mutate(scenario = 'Observed')

gdp.pwt$countrycode <- recode(gdp.pwt$countrycode, "ROM" = "ROU", "ZAR" = "COD", "GER" = "DEU") #Correct for outdated country codes

gdp.15 <- read.csv("data/gdp_ssp_5year.csv", header = T, sep = ";", dec = ",") %>% 
  gather(scenario, gdppc, -iso3, -year) %>%
  filter((year == 2010 | year == 2015) & scenario == 'SSP2') %>% 
  select(-scenario) %>% 
  mutate(scenario = "Observed") %>% 
  spread(year, gdppc)

# Interpolation function from Burke et al. (2018)
ipolate <- function(mat) {
  yrs <- 2010:2015
  ys <- as.numeric(unlist(names(mat)))
  mat1 <- array(dim = c(dim(mat)[1], length(yrs)))
  est <- seq(2010, 2015, 5)
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
  mat1 <- data.frame(mat[,1:2], mat1)
  names(mat1)[3:dim(mat1)[2]] <- yrs
  return(mat1)
}

gdp.15.ipol <- ipolate(gdp.15) %>% 
  gather(year, gdppc, -iso3, -scenario) %>% 
  rename(countrycode = iso3) %>% 
  mutate(year = as.integer(year))

gdp.yearly <- gdp.pwt %>% 
  bind_rows(gdp.15.ipol) %>% 
  arrange(countrycode)


# Education data: share of population by educational attainment and gender gap in education measured by the difference in mean years of schooling (MYS) between women and men (source: Wittgenstein Centre for Demography and Global Human Capital)

edu.prep <- read.csv("data/wic_eduatt.csv", skip = 8, sep = ",") %>% 
  spread(Year, Distribution)

mys.gap.prep <- read.csv("data/wic_mys_gap.csv", skip = 8) %>% 
  spread(Year, Years)


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
  gather(year, distribution, -Area, -Scenario, -ISOCode, -Education) %>% 
  mutate(Education = str_replace(str_to_lower(Education), fixed(" "), ".")) %>% 
  spread(Education, distribution)

mys.gap.ipol <- ipolate2(mys.gap.prep) %>% 
  gather(year, mys.gap, -Area, -Scenario, -ISOCode) %>% 
  mutate(year = year %>% str_replace("X", "") %>% as.factor)

edu.master <- edu.ipol %>% 
  left_join(mys.gap.ipol) %>% 
  mutate(countrycode = countrycode(Area, 'country.name', 'iso3c')) %>%
  mutate(Scenario = recode(Scenario, 'SSP2' = 'Observed'),
         year = as.integer(year)) %>% 
  rename(scenario = Scenario) %>% 
  filter(scenario == 'Observed')


# Merge into one dataset

observed.yr <- wgi %>% 
  left_join(gdp.yearly, by = c('countrycode', 'year', 'scenario')) %>% 
  left_join(edu.master, by = c('countrycode', 'year', 'scenario')) %>%
  left_join(nd.readiness, by = c('countrycode', 'year', 'scenario')) %>%
  select(-Area) %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate(year = as.integer(year),
         lngdp = log(gdppc))

observed.yr$countrycode <- recode(observed.yr$countrycode, "ROM" = "ROU", "ZAR" = "COD") # Fix outdated country names

#write.csv(observed.yr, 'data/observed_yr.csv')

# Projections data

gdp15.2 <- read.csv("data/gdp_ssp_5year.csv", header = T, sep = ";", dec = ",") %>% 
  gather(scenario, gdppc, -iso3, -year) %>% 
  filter(year == 2015 & scenario == "SSP2") %>% 
  rename(countrycode = iso3) %>% 
  select(countrycode, year, gdppc)

gdp.proj <- read.csv("data/gdp_ssp_5year.csv", header = T, sep = ";", dec = ",") %>% 
  gather(scenario, gdppc, -iso3, -year) %>% 
  filter(year > 2015) %>% 
  rename(countrycode = iso3) 

prep <- gdp.proj %>%  
  select(countrycode, scenario) %>% 
  merge(gdp15.2, all.x = T)

gdp.proj <- gdp.proj %>%
  bind_rows(prep)

# Education projections

edu.proj <- read.csv("data/wic_eduatt.csv", skip = 8, sep = ",") %>% 
  select(-ISOCode) %>% 
  mutate(Education = str_replace(str_to_lower(Education), fixed(" "), ".")) %>% 
  spread(Education, Distribution)

mys.gap.proj <- read.csv("data/wic_mys_gap.csv", skip = 8, sep = ',') %>% 
  select(-ISOCode) %>%
  rename(mys.gap = Years)

edu.proj.master <- inner_join(edu.proj, mys.gap.proj, by = c('Area', 'Year', 'Scenario')) %>%
  mutate(countrycode = countrycode(Area, 'country.name', 'iso3c')) %>% 
  rename(scenario = Scenario,
         country = Area,
         year = Year) %>% 
  filter(year > 2005)

# Merge all data into a master dataset for projections

projections <- gdp.proj %>% 
  inner_join(edu.proj.master, by=c('countrycode', 'year', 'scenario')) %>% 
  mutate(countrycode = countrycode %>% as.factor) %>%
  mutate(lngdp = log(gdppc)) %>% 
  arrange(countrycode, year, scenario) %>% 
  mutate(ID = paste(countrycode, year, scenario))

projections <- projections[!duplicated(projections$ID), ] %>% 
  filter(year >= 2015)

# Interpolate for yearly values

ipolate3 <- function(mat) {
  yrs <- 2015:2099
  ys <- as.numeric(unlist(names(mat)))
  mat1 <- array(dim = c(dim(mat)[1], length(yrs)))
  est <- seq(2015, 2100, 5)
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

projections.ipol <- projections %>%
  select(-ID, -country) %>%
  gather(variable, value, -countrycode, -scenario, -year) %>%
  spread(year, value) %>%
  ipolate3()

projections.yr <- projections.ipol %>%
  gather(year, value, -countrycode, -scenario, -variable) %>%
  spread(variable, value) %>%
  mutate(year = year %>% as.numeric())


#write.csv(projections.yr, 'data/projections_yr.csv')
