
#libraries
library("here") # file management
library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation 
library("ggpubr") # graphs
#library("viridis") # colours
library("patchwork") # combine graphs
library("ggforce") # graphs
library("lme4") # multilevel estimation
#library("geepack") # marginal models
library("easystats") # reporting
library("kableExtra") # tables
library("broom") # working with  models
library("broom.mixed") # mixed effects models
library("brms") # bayesian estimation
library("rstan") # backend brms
library("rstanarm") # graphing
#library("cmdstanr") # backend brms'
#library("ipw") # inverse probability weighting
library("tidybayes") # workign with posterior probability distributions
library("bayesplot") # graphs
library("ggokabeito")   # color palette
#library("gghalves")     #  half geoms
#library("ggbeeswarm")   # Special distribution-shaped point jittering
library("emmeans") # estimate marginal means
library("table1") # tables /now with latex
library("tidyverse") # data wrangling
library("sjstats")
library("magick")
library("simstudy")
library("future")
#library(splines)
# rstan options
library("brms") # bayesian estimation
#library("cmdstanr") # backend brms
rstan_options(auto_write = TRUE) # bayesian estimation
#options(mc.cores = parallel::detectCores ()) # use all core

#options(mc.cores = 2 ) # use all core
library("ggstatsplot")

## Prejudice all groups
# import data -------------------------------------------------------------

df <- readRDS(here::here("data", "df"))




# df 5 for estimating time trajectory in attack sample ---------------------------
df5<- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    Warm.Overweight,
    Warm.Elderly,
    Warm.MentalIllness,
    Warm.Muslims,
    Warm.Immigrants,
    Warm.Asians,
    Warm.Refugees,
    Wave,
    Warm.Maori,
    Warm.NZEuro,
    Warm.Indians,
    Warm.Chinese,
    Warm.Refugees,
    Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2016", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave==2017, TSCORE_b + 365,
    ifelse(YearMeasured == 0 & Wave==2018, TSCORE_b + 730,
           ifelse(YearMeasured == 0 & Wave==2019, TSCORE_b + 1094, # leap 
                  ifelse(YearMeasured == 0 & Wave==2020, TSCORE_b + 1459, TSCORE)))))%>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  dplyr::mutate(pol_bz = if_else(Wave == "2016", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2016", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2016", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2016", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2016", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2016", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2016", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2016", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2016", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2016", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2016", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 

levels(df5$Wave) <- c("Time8", "Time9", "Time10", "Time11","Time12")



# table

table1::table1(~ Warm.Overweight +
               Warm.Elderly +
               Warm.MentalIllness +
               Warm.Muslims +
               Warm.Immigrants +
               Warm.Asians +
               Warm.Refugees +
               Warm.Maori +
               Warm.NZEuro +
               Warm.Indians +
               Warm.Chinese +
               Warm.Refugees +
               Warm.Pacific|Wave, data = df5)

# visualise 2021
df21<- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    Warm.Overweight,
    Warm.Elderly,
    Warm.MentalIllness,
    Warm.Muslims,
    Warm.Immigrants,
    Warm.Asians,
    Warm.Refugees,
    Warm.Maori,
    Warm.NZEuro,
    Warm.Indians,
    Warm.Chinese,
    Warm.Refugees,
    Warm.Pacific,
    YearMeasured, 
    Id
  ) %>%
  dplyr::filter(Wave ==2020) 




table1::table1(~ Warm.Overweight +
                 Warm.Elderly +
                 Warm.MentalIllness +
                 Warm.Muslims +
                 Warm.Immigrants +
                 Warm.Asians +
                 Warm.Refugees +
                 Warm.Maori +
                 Warm.NZEuro +
                 Warm.Indians +
                 Warm.Chinese +
                 Warm.Refugees +
                 Warm.Pacific|Wave, data = df21, overall=FALSE)

# tw<-km_all5%>%