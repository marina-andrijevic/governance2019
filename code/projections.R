
# # #         Governance in socio-economic pathways and its role              # # # 
# # #                  for future adaptive capacity                           # # # 
# # #                    (Andrijevic et al., 2019)                            # # # 
# # #                                                                         # # # 
# # #           Econometric model and projections of variables                # # #

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(plm)
library(broom)
library(WDI)
library(countrycode)
library(data.table)
library(readstata13)
library(viridis)
library(ggthemes)
library(sandwich)
library(lmtest)
library(RCurl)
library(gdata) 
library(zoo)


# Load the previously processed data (see code/data_mgmt.R)

observed.yr <- read.csv('data/observed_yr.csv')
projections.yr <- read.csv('data/projections_yr.csv')

# import the robust standard error function
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

# Fixed effects model with robust standard errors
fefit <- lm(governance ~ lngdp + post.secondary + abs(mys.gap) + factor(countrycode) + factor(year), data = observed.yr)
rob1 <- as.vector(summary(fefit,robust = T)$coefficients[,"Std. Error"])

# Between-variation
befit <- lm(governance ~ lngdp + post.secondary + mys.gap, data = observed.yr)
rob2 <- as.vector(summary(befit,robust = T)$coefficients[,"Std. Error"])

# With primary education
mod3 <- lm(governance ~ lngdp + mys.gap + primary + factor(countrycode) + factor(year), data = observed.yr)
rob3 <- as.vector(summary(mod3,robust = T)$coefficients[,"Std. Error"])

# With lower secondary education
mod4 <- lm(governance ~ lngdp + mys.gap + lower.secondary + factor(countrycode) + factor(year), data = observed.yr)
rob4 <- as.vector(summary(mod4,robust = T)$coefficients[,"Std. Error"])

# With upper secondary education
mod5 <- lm(governance ~ lngdp +  mys.gap + upper.secondary + factor(countrycode) + factor(year), data = observed.yr)
rob5 <- as.vector(summary(mod5,robust = T)$coefficients[,"Std. Error"])


# Prepare data for projections

# Extract the coefficients from the fixed effects model

fefit <- plm(governance ~ lngdp + post.secondary + mys.gap, index = c('countrycode','year'), model = 'within', effect = 'twoways', data = observed.yr)

summary(fefit, robust = T)

estimates <- tidy(fefit) #Broom package function for summarizing the components of a model

c1 <- estimates %>% 
  filter(term == "lngdp") %>% 
  select(estimate)

c2  <- estimates %>% 
  filter(term == "post.secondary") %>% 
  select(estimate)

c3 <- estimates %>% 
  filter(term == "mys.gap") %>% 
  select(estimate)

# Country fixed effects: they are expected to converge to the 75th percentile of the present-day distribution, in years in the future changing by the SSPs. We follow the approach of Crespo Cuaresma (2017), and let the fixed effects converge for SSP1: 2130, SSP2:2250, SSP3: No convergence, SSP4: 2250, SSP5: 2180 

years <- c(year = seq(1995, 2100, 1))

cntry.fe <- fixef(fefit) %>%
  as.data.frame() %>%
  rownames_to_column("countrycode") %>%
  rename(fe = ".") %>%
  merge(years, all.y = T) %>%
  rename(year = y)


# 1) Calculate the growth rates for each SSP:

# max(fe) = min(fe) * (1 + r)^t
# max(fe)/min(fe) = e^rt
# ln(max(fe)/min(fe)) = rt
# r = ln(max(fe)/min(fe))/t

conv.rates <- data.frame(years = c(114, 234, 3000, 234, 164),
                         scenario = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'))
target <- quantile(cntry.fe$fe, 0.75)
target.adj <- target + 0.3
min.fe <- min(cntry.fe$fe) + 0.3

# Thrid quantile is around 0.2, but 0.3 is added because the minimum value of the sample's fixed effects is negative, so to bring it to 0 and make the loop below runing (otherwise the function will attempt to take log from a negative value and will fail))

for (i in 1:length(conv.rates$years)) {
  conv.rates$r[i] = log(target.adj/min.fe)/conv.rates$years[i]  # Formula for convergence rates
}

# 2) Prepare the data frames for convergence calculations

years <- data.frame(year = seq(2016, 2100, 1))
ssps <- data.frame(scenario = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'),
                   rate = conv.rates$r)
df <- merge(ssps, years, all.y = T)

fe.conv <- cntry.fe %>%
  filter(fe < target) %>% 
  spread(countrycode, fe)

years2 <- data.frame(year = seq(2015, 2100, 1))

fe.const <- cntry.fe %>%
  filter(fe >= target) %>% 
  select(countrycode, fe) %>%
  merge(years2, all.y = T) %>%
  merge(ssps$scenario, all.y = T) %>%
  rename(scenario = y) %>%
  mutate(ID = paste(countrycode, year, scenario)) %>%
  filter(!duplicated(ID)) %>%
  select(-ID)

first.row <- fe.conv %>%
  filter(year == 2015) %>%
  merge(ssps, all.y = T)

dat1 <- data.frame(matrix(nrow = nrow(df), ncol = ncol(fe.conv)))

names(dat1) <- names(fe.conv)

fe.prep <- bind_cols(df, dat1) %>%
  bind_rows(first.row) %>%
  arrange(year) %>%
  select(-year1) %>%
  arrange(scenario)

# Function to iterate over countries and years to calculate future fixed effects

fe.fun <- function(df) {
  for (i in 2:nrow(df)) {
    for (j in 4:ncol(df)) {
      speed <- df$rate[i]
      df[i,j] = df[i-1,j] + speed*(target - df[i-1, j])
    }
  }
  return(df)
}

fe.convergence <- fe.prep %>% 
  group_by(scenario) %>% 
  do(fe.fun(.)) %>% 
  gather(countrycode, fe, -year, -scenario, -rate) %>% 
  select(-rate) %>% 
  bind_rows(fe.const)

# Check for a country of choice
fe.convergence %>%
  filter(countrycode == "RWA") %>%
  ggplot(aes(x = year, y = fe, color = scenario)) +
  geom_line(size = 1) +
  labs(y = "Fixed effect", x = 'Year')

# Add the converged fixed effects to the projections 

projections.yr <- projections.yr %>% 
  left_join(fe.convergence, by = c('scenario', 'year', 'countrycode'))%>% 
  mutate(ID = paste(countrycode, year, scenario)) 


# Results of governance projections

projections.ext <- projections.yr %>%  
  mutate(gov = log(gdppc)*(c1$estimate) + post.secondary*(c2$estimate) + mys.gap*(c3$estimate) + fe,
         gov = ifelse(gov>1, 1, gov))

# Replace South Sudan with values for Sudan, and Western Sahara with Morocco 

projections.ext$countrycode <- recode(projections.ext$countrycode, "SDN" = "SSD", "MAR" = "ESH")

# Check for duplicate values
projections.ext <- projections.ext %>% 
  mutate(ID = paste(countrycode, year, scenario)) %>% 
  filter(!duplicated(ID)) %>% 
  filter(!countrycode == 'TLS') # Remove East Timor as there was no data on governance for it


# Compositional analysis of contributions of individual covariates

library(compositions)
library(robustbase)

y <- observed.yr %>% 
  select(voic.ac, pol.stab, gov.eff, reg.qual, ru.law, corr.cont) %>% 
  acomp()

covariates <- observed.yr %>% 
  select(countrycode, year, gdppc, post.secondary, mys.gap)

x1 <- covariates$gdppc
x2 <- covariates$post.secondary
x3 <- covariates$mys.gap
x4 <- covariates$countrycode
x5 <- covariates$year

comp.model <- lm(ilr(y) ~ log(x1) + x2 + x3 + factor(x4) + factor(x5))

coefs <- ilrInv(coef(comp.model), orig = y)

#Isometric log-ratio transformation (ilr): Only one-dimensional Euclidean space 
#typically called R^D-1, up to an isometric mapping. Therefore, there exist an isometric linear mapping
#between the Aitchison simplex and R^D-1. This mapping is called the isometric log-ratio transformation.

coefs.new <- coefs[c(2:4),]

coefficients <- t(coefs.new) %>% 
  as.data.frame()

coefficients <- coefficients %>% 
  mutate(indicator = row.names(coefficients)) 

coefficients <- coefficients %>% 
  rename(LnGDP = 'log(x1)', Education = 'x2', GenderGap = 'x3') %>% 
  gather(covariate, value, -indicator)

coefficients


# # #  Bar plots for compositional coefficients
cbPalette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(scales)

show_col(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

ggplot(coefficients, aes(covariate, value, fill = factor(indicator))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Explanatory Variable', y = 'Value', title = 'Compositional analysis of regression coefficients') + 
  scale_fill_manual(name = 'Indicator', labels=c("Control of corruption", "Government effectiveness", "Political stability", 
                                                 "Regulatory quality", 'Rule of law', 'Voice and accountability'),
                    values = cbPalette) +
  theme_hc()



# Note: projections for the individual components of governance (control of corruption, government effectiveness) as well as the 
# readiness component of the ND GAIN indicators were done in the same way, simply by replacing the independent variable in regressions
