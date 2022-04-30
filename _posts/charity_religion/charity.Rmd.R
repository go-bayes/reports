# ---
#   title: "Does Religious Service Attendance Causally Affect Charitable Giving?"
# description:  
#   author:
#   - name: Joseph Bulbulia
# url: https://josephbulbulia.netlify.app
# affiliation: Victoria University of Wellington
# affiliation_url: https://www.wgtn.ac.nz
# orcid_id: 0000-0002-5861-2056
# date: 2022-FEB-10
# output:
#   distill::distill_article:
#   self_contained: false
# toc: true
# code_folding: true
# bibliography: bibliography.bib
# editor_options: 
#   chunk_output_type: console
# ---
#   
#   ```{r load_libraries, include=FALSE}
# setup
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  layout = "l-body-outset",
  fig.width = 16,
  fig.height = 9,
  collapse = TRUE,
  R.options = list(width = 60
  )
)


library("tidyverse")
library("dplyr")
library("tidyr")
library("Amelia")
library("Patchwork")
library("easystats")
library("ggeffects")
library("geepack")
library("easystats")
library("lme4")



# read data
df <- readRDS(here::here("data", "df"))

df$Religion.Church

library(LMest)
relwidelong <-  df %>%
  dplyr::select(Id, Religion.Church, Wave)
relwidelong
#out <- with(relwidelong, long2matrices(id = Id, X = Age, Y = Religious))

relwide <- spread(relwidelong, Wave, Religion.Church)
head(relwide)

#relwide<-relwide[complete.cases(relwide), ]

Relidwide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave !=2009) %>% # church not measured then
  dplyr::filter(!is.na(Religion.Church)) %>%
  dplyr::select(Id, Religion.Church, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  mutate(log_church = log(Religion.Church + 1 )) %>%
  select(-Religion.Church) %>%
  ungroup(Id) %>%
  spread(Wave, log_church)

#Relidwide <- Relidwide[complete.cases(Relidwide),]
dim(Relidwide)

x_var_list <- names(Relidwide[, 2:11])

Relidwide

library(lcsm)


plot_trajectories(
  data = Relidwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.10,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)


# charitable giving factets  ----------------------------------------------

  
wide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave !=2009) %>% # church not measured then
  dplyr::filter(!is.na(CharityDonate)) %>%
  dplyr::select(Id, CharityDonate, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  mutate(log_charity = log(CharityDonate + 1 )) %>%
  select(-CharityDonate) %>%
  ungroup(Id) %>%
  spread(Wave, log_charity)

#Relidwide <- Relidwide[complete.cases(Relidwide),]
dim(wide)

x_var_list <- names(wide[, 2:11])

wide

library(lcsm)


plot_trajectories(
  data = wide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "log_charity",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.10,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)



# volunteering ------------------------------------------------------------

str(df$HoursCharity)
wide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave !=2009) %>% # church not measured then
#  dplyr::filter(!is.na(HoursCharity)) %>%
  dplyr::select(Id, HoursCharity, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  mutate(log_vol = log(HoursCharity + 1 )) %>%
  select(-HoursCharity) %>%
  ungroup(Id) %>%
  spread(Wave, log_vol)
#Relidwide <- Relidwide[complete.cases(Relidwide),]
dim(wide)

x_var_list <- names(wide[, 2:11])

wide

library(lcsm)


plot_trajectories(
  data = wide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "log_vol",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.10,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)


## Better to look at loss of religion



#USEFUL

# sdf.0 <- df %>%
#   mutate(Wave = as.numeric(as.character(Wave))) %>%
#   arrange(Wave, Id) %>%
#   group_by(Id) %>%
#   mutate(first = {
#     YearMeasured == 1
#   } %>% {
#     . * !duplicated(.)
#   }) %>%
#   mutate(
#     value_tmp = if_else(first == 1, Wave, NA_real_),
#     firstwave  = mean(value_tmp, na.rm = TRUE) # this is a hack, but works
#   ) %>%
#   mutate(state  = ifelse(
#     YearMeasured == -1,
#     "deceased",
#     ifelse(
#       YearMeasured == 0 & Wave < firstwave,
#       "notyetmeasured",
#       ifelse(YearMeasured == 0 &
#                Wave > firstwave, "missing",
#              "measured")
#     )
#   )) %>%
#   dplyr::mutate(Wave = as.factor(Wave)) %>% # return Wave to a factor
#   dplyr::select(Wave, Id, state, YearMeasured, SampleOriginYear) %>%
#   dplyr::filter(state != "notyetmeasured") %>%
#   droplevels() %>%
#   arrange(Id, Wave)





tf <- df %>%
  dplyr::select(Id, Wave, TSCORE, Religious.Church, HourCharity, Charitydonate, 
                Male, GenCohort, ) %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::group_by(Id) %>% filter(n() > 2) %>%
  dplyr::mutate(
    days = as.integer(TSCORE),  # THIS GIVES DAY ZERO  THIS WON'T WORK
    # yr_0 = ((days - min(days)) / 365),
    # yrs = (days / 365),
    # yr_a = 3545 / 365,
    wave0 = as.numeric(as.character(Wave)) - min(as.numeric(as.character(Wave)))
  ) %>%   #
  ungroup() %>%
  # dplyr::mutate(Pre_PostATTACK = factor(ifelse(
  #   TSCORE >= 3545, "post_attack",
  #   "pre_attack"
  # ))) %>%
  dplyr::mutate(religious = as.numeric(Religious)-1) %>%
  # dplyr::mutate(Pre_PostATTACK = forcats::fct_relevel(Pre_PostATTACK, c("pre_attack", "post_attack"))) %>%
  dplyr::group_by(Id) %>% filter(n() > 1) %>%
  drop_na() %>%
  arrange(Id, wave0) %>%
  dplyr::mutate(across(
    c(religious,
      Env.ClimateChgReal, 
      Male, 
      Relid),
    list(lag = lag)
  )) %>% # get lag * 2
  dplyr::mutate(across( # sometimes useful
    c(religious_lag,
      Env.ClimateChgReal_lag,
      Male_lag,
      Relid),
    list(lag = lag)
  )) %>%
  dplyr::mutate(across(
    c(religious,
      Env.ClimateChgReal),
    list(lead = lead)
  )) %>%
  dplyr::ungroup(Id)  %>%
  droplevels() %>%
  #  dplyr::group_by(Id) %>% filter(n() > 1) %>%
  dplyr::arrange(Id,wave0)



df5 <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    Household.INC,
    HomeOwner,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious1,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    #Religious1,
    Believe.God, 
    Believe.Spirit,
    AGREEABLENESS, 
    CONSCIENTIOUSNESS, 
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    Religion.Church, 
    Religion.Prayer,
    SmokerEver,
    YearMeasured, 
    CharityDonate,
    HoursCharity,
  ) %>%
  dplyr::filter(Wave != 2009) %>%
  droplevels() %>%
  dplyr::mutate(org2010 =  ifelse(Wave == 2010 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2010, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeat of baseline in 2016 
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  # dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>%
  # dplyr::mutate(yrs =  (dys/365))%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
  dplyr::mutate(agr_bz = if_else(Wave == "2016", (as.numeric(AGREEABLENESS)), NA_real_)) %>%
  fill(agr_bz) %>%
  dplyr::mutate(cs_bz = if_else(Wave == "2010", (as.numeric(CONSCIENTIOUSNESS)), NA_real_)) %>%
  fill(cs_bz) %>%
  dplyr::mutate(op_bz = if_else(Wave == "2010", (as.numeric(OPENNESS)), NA_real_)) %>%
  fill(op_bz) %>%
  dplyr::mutate(h_bz = if_else(Wave == "2010", (as.numeric(HONESTY_HUMILITY)), NA_real_)) %>%
  fill(h_bz) %>%
  dplyr::mutate(ex_bz = if_else(Wave == "2010", (as.numeric(EXTRAVERSION)), NA_real_)) %>%
  fill(ex_bz)%>%
  dplyr::mutate(nr_bz = if_else(Wave == "2010", (as.numeric(NEUROTICISM)), NA_real_)) %>%
  fill(nr_bz) %>%
  dplyr::mutate(lch_bz = if_else(Wave == "2016", (as.numeric(log(Religion.Church+1))), NA_real_)) %>%
  fill(lch_bz) %>%
  dplyr::mutate(bs_bz = if_else(Wave == "2016", (as.numeric(Believe.Spirit)), NA_real_)) %>%
  fill(bs_bz) %>%
    dplyr::mutate(bg_bz = if_else(Wave == "2016", (as.numeric(Believe.God)), NA_real_)) %>%
  fill(bg_bz) %>%
  dplyr::mutate(pol_bz = if_else(Wave == "2010", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2010", (as.numeric(Religious1))/2, NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2010", (as.numeric(Partner))/2, NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2010", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  # dplyr::mutate(age_bz = if_else(Wave == "2016", (Age), NA_real_)) %>%
  # fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2010", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2010", (as.numeric(Male)), NA_real_)) %>%
  fill(male_2z)  %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2010", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  # dplyr::mutate(edu_bz = if_else(Wave == "2010", (Edu), NA_real_)) %>%
  # fill(edu_bz) %>%  Not measured
  dplyr::mutate(ubran_bz = if_else(Wave == "2010", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2010", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  mutate(bg1 = (as.numeric(Believe.God) )) %>%
  mutate(bs1 = (as.numeric(Believe.Spirit) ))%>%
 # mutate(log_vol = log(HoursCharity+ 1) ) %>%
 # mutate(log_char = log(CharityDonate + 1) )%>%
  #               lag_log_vol = lag(log_vol), 
  #               lag_log_char = lag(log_char), 
  #               lag_log_church = lag(lch_bz),
  #               lead_log_vol = lead(log_vol),
  #               lead_log_char = lead(log_char)) %>%
  arrange(Id, wave) %>%
   select(
     -c(
        Wave,
  #     Male,
  #     Urban,
  #     Edu,
  #     Employed,
  #     Male,
  #     NZdep,
  #     Parent,
  #     Partner,
  #     Pol.Orient,
  #    # Religion.Church,
  #     AGREEABLENESS,
  #     CONSCIENTIOUSNESS,
  #     OPENNESS,
  #     HONESTY_HUMILITY,
  #     EXTRAVERSION,
  #     NEUROTICISM,
  #     YearMeasured,
  #     EthnicCats,
        hold
  #    # GenCohort,
  #     Believe.God,
  #     Believe.Spirit
  #   )
  ))%>%
  arrange(Id,wave)


head(df5)
countid <-length(unique(df5$Id))
countid

## impute church
match("Religion.Church", names(df5))
# bounds for imp god belief 
bdsR <- matrix(c(25, 0, 5), nrow = 1, ncol = 3)
#bdsR
head(df5)
df5 = as.data.frame(df5)

## SEE THIS FOR DATA WRANGLING
## https://uw-pols503.github.io/2016/missing_data_imputation.html
## https://github.com/andrewheiss/amelia-tidy-melding


df5_a <- amelia(
  set.seed=1234,
  df5, 
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  logs = c("Religion.Church", "Household.INC", "Religion.Prayer","CharityDonate"),
  #ords = "Religious1",
  noms = c(
    "EthnicCats_b","GenCohort", "EthnicCats", "Male", "Believe.God","Believe.Spirit", "Employed"
  ),
 # idvars=c("log_char, log_vol"), # yrs not working
  lags= c("Religion.Church", "CharityDonate", "HoursCharity"),
  leads= c("Religion.Church", "CharityDonate","HoursCharity"),
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bdsR, 
  empri = .01*nrow(df5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(df5_a, here::here("_posts","religious_simpsons_paradox","mods", "df5_a"))








df_o <- transform(df5_a)


df_o$imputations$imp1$lag_log_char
library(lme4)

# Standard growth model

# model
m<-10
model_char <-NULL
library(splines)
for(i in 1:m) {
  model_char[[i]] <- lmer(lead_log_vol ~ (lch_bz) + lag_log_church + lag_log_char + (1|Id), 
                          data = df5_a$imputations[[i]])
}

# table
tab<-pool_parameters(model_char)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)
model_char

plot(ggeffects::ggemmeans(model_char[[1]], terms=c("lch_bz[all]")), add.data=TRUE, dot.alpha =.01)


## Causal estimation

mod1 <- lmer(Env.ClimateChgReal_lead ~ # outcome (after exposure)
               religious + #exposure 
               religious_lag + # control for previous exposure + 
               # religious_lag_lag + 
               Env.ClimateChgReal_lag + #Control for previous outcome
               # Env.ClimateChgReal_lag_lag + 
               Male_lag_lag+ # baseline confounder control
               GenCohort + # baseline confounder control
               yrs + # year trend
               (1|Id),  # dependencies from the repeated measures 
             data = tf)


#We find no causal effect 


mod1%>%
  parameters() %>%
  print_html()

plot(ggeffects::ggemmeans(mod1, terms=c("religious")), add.data=TRUE, dot.alpha =.005)



##
# 
# dfn <- df %>%
#   dplyr::select(
#     Id,
#     Age,
#     Wave,
    Household.INC,
    HomeOwner,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious1,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    #Religious1,
    Believe.God,
    Believe.Spirit,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    Religion.Church,
    Religion.Prayer,
    SmokerEver,
    YearMeasured,
    CharityDonate,
    HoursCharity,
#   ) %>%
#   dplyr::filter(Wave != 2009) %>%
#   droplevels() %>%
#   dplyr::mutate(org2010 =  ifelse(Wave == 2010 & YearMeasured ==1,1,0 ))%>%
#   group_by(Id) %>%
#   dplyr::mutate(hold = mean(org2010, na.rm = TRUE)) %>%  # Hack
#   filter(hold>0) %>% # hack to enable repeat of baseline in 2016 
#   dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
#   ungroup(Id) %>%
#   dplyr::mutate(Edu = as.numeric(Edu))%>%
#   arrange(Id,Wave) %>%
#   group_by(Id) %>%
#   # dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>%
#   # dplyr::mutate(yrs =  (dys/365))%>%
#   dplyr::mutate(wave = as.numeric(Wave)-1) %>%
#   dplyr::mutate(agr_bz = if_else(Wave == "2016", (as.numeric(AGREEABLENESS)), NA_real_)) %>%
#   fill(agr_bz) %>%
#   dplyr::mutate(cs_bz = if_else(Wave == "2010", (as.numeric(CONSCIENTIOUSNESS)), NA_real_)) %>%
#   fill(cs_bz) %>%
#   dplyr::mutate(op_bz = if_else(Wave == "2010", (as.numeric(OPENNESS)), NA_real_)) %>%
#   fill(op_bz) %>%
#   dplyr::mutate(h_bz = if_else(Wave == "2010", (as.numeric(HONESTY_HUMILITY)), NA_real_)) %>%
#   fill(h_bz) %>%
#   dplyr::mutate(ex_bz = if_else(Wave == "2010", (as.numeric(EXTRAVERSION)), NA_real_)) %>%
#   fill(ex_bz)%>%
#   dplyr::mutate(nr_bz = if_else(Wave == "2010", (as.numeric(NEUROTICISM)), NA_real_)) %>%
#   fill(nr_bz) %>%
#   dplyr::mutate(lch_bz = if_else(Wave == "2016", (as.numeric(log(Religion.Church+1))), NA_real_)) %>%
#   fill(lch_bz) %>%
#   dplyr::mutate(bs_bz = if_else(Wave == "2016", (as.numeric(Believe.Spirit)), NA_real_)) %>%
#   fill(bs_bz) %>%
#   dplyr::mutate(bg_bz = if_else(Wave == "2016", (as.numeric(Believe.God)), NA_real_)) %>%
#   fill(bg_bz) %>%
#   dplyr::mutate(pol_bz = if_else(Wave == "2010", (Pol.Orient), NA_real_)) %>%
#   fill(pol_bz) %>%
#   dplyr::mutate(rel_bz = if_else(Wave == "2010", (as.numeric(Religious1))/2, NA_real_)) %>%
#   fill(rel_bz) %>%
#   dplyr::mutate(partner_bz = if_else(Wave == "2010", (as.numeric(Partner))/2, NA_real_)) %>%
#   fill(partner_bz) %>%
#   dplyr::mutate(parent_bz = if_else(Wave == "2010", (as.numeric(Parent)), NA_real_)) %>%
#   fill(parent_bz) %>%
#   # dplyr::mutate(age_bz = if_else(Wave == "2016", (Age), NA_real_)) %>%
#   # fill(age_bz) %>%
#   dplyr::mutate(nzdep_bz = if_else(Wave == "2010", (NZdep), NA_real_)) %>%
#   fill(nzdep_bz) %>%
#   dplyr::mutate(male_2z = if_else(Wave == "2010", (as.numeric(Male)), NA_real_)) %>%
#   fill(male_2z)  %>%
#   dplyr::mutate(employed_bz = if_else(Wave == "2010", (as.numeric(Employed)), NA_real_)) %>%
#   fill(employed_bz) %>%
#   # dplyr::mutate(edu_bz = if_else(Wave == "2010", (Edu), NA_real_)) %>%
#   # fill(edu_bz) %>%  Not measured
#   dplyr::mutate(ubran_bz = if_else(Wave == "2010", (as.numeric(Urban)), NA_real_)) %>%
#   fill(ubran_bz) %>%
#   dplyr::mutate(EthnicCats_b = if_else(Wave == "2010", as.numeric(EthnicCats), NA_real_)) %>%
#   fill(EthnicCats_b) %>%
#   mutate(bg1 = (as.numeric(Believe.God) )) %>%
#   mutate(bs1 = (as.numeric(Believe.Spirit) ))%>%
#   # mutate(log_vol = log(HoursCharity+ 1) ) %>%
#   # mutate(log_char = log(CharityDonate + 1) )%>%
#   #               lag_log_vol = lag(log_vol), 
#   #               lag_log_char = lag(log_char), 
#   #               lag_log_church = lag(lch_bz),
#   #               lead_log_vol = lead(log_vol),
#   #               lead_log_char = lead(log_char)) %>%
#   arrange(Id, wave) %>%
#   select(
#     -c(
#       Wave,
#       #     Male,
#       #     Urban,
#       #     Edu,
#       #     Employed,
#       #     Male,
#       #     NZdep,
#       #     Parent,
#       #     Partner,
#       #     Pol.Orient,
#       #    # Religion.Church,
#       #     AGREEABLENESS,
#       #     CONSCIENTIOUSNESS,
#       #     OPENNESS,
#       #     HONESTY_HUMILITY,
#       #     EXTRAVERSION,
#       #     NEUROTICISM,
#       #     YearMeasured,
#       #     EthnicCats,
#       hold
#       #    # GenCohort,
#       #     Believe.God,
#       #     Believe.Spirit
#       #   )
#     ))%>%
#   arrange(Id,wave)
# 
# 
# 
# # Target trial

## SEE HEISS FOR DATA WRANGLING https://github.com/andrewheiss/amelia-tidy-melding
#library(tidyverse)

all_imputations <- bind_rows((unclass(df5_a$imputations)), .id = "m") %>%
  group_by(m) %>%
  nest()
    



# data wrangle ------------------------------------------------------------

    
m <- 10
new_data <- NULL

for (i in 1:m) {
  new_data$data[[i]] <- all_imputations$data[[i]] %>%
    dplyr::mutate(church = log(Religion.Church+1),
           vol = as.numeric(HoursCharity),
           charity = as.integer(round(CharityDonate,1)),
           charity_l = log(CharityDonate+1)) %>%
    dplyr::arrange("Id", "wave") %>%
    dplyr::mutate(across(
             c(#church,
               #vol,
               #charity
               Household.INC,
               HomeOwner,
              # EthnicCats,
              # Employed,
              # Urban,
               Edu,
              # Male,
               Pol.Orient,
               NZdep,
              # Religious1,
              # GenCohort,
              # Urban,
               # Partner,
               # Parent,
              # Believe.God,
              # Believe.Spirit,
               AGREEABLENESS,
               CONSCIENTIOUSNESS,
               OPENNESS,
               HONESTY_HUMILITY,
               EXTRAVERSION,
               NEUROTICISM),
             list(scale = scale)
           )) %>%
  dplyr::mutate(across(
    c( church,
       vol,
       charity,
       charity_l,
       SmokerEver,
      Household.INC_scale,
      HomeOwner_scale,
       EthnicCats,
       Employed,
       Urban,
      Edu_scale,
      Male,
      Pol.Orient_scale,
      NZdep_scale,
      #Religious1,
      GenCohort,
      Urban,
      Partner,
      Parent,
      Believe.God,
      Believe.Spirit,
      AGREEABLENESS_scale,
      CONSCIENTIOUSNESS_scale,
      OPENNESS_scale,
      HONESTY_HUMILITY_scale,
      EXTRAVERSION_scale,
      NEUROTICISM_scale),
    list(lag = lag)
  )) %>%
    dplyr::mutate(across(
      c(church_lag,
       vol_lag,
       charity_l_lag,
       charity_lag),
       list(lag = lag)
       ))%>%
    dplyr::mutate(across(
      c(church,
        vol,
        charity,
        charity_l),
      list(lead = lead)
    ))%>%
    # group_by(Id) %>%
    # mutate(always  = mean(church, na.rm = TRUE)) %>% # this is a hack, but works
    # filter(!always > 0) %>% # Get rid of people who were always going to church
    # mutate(convert = {
    #   church > 0 &
    #     church_lag == 0 & church_lag_lag == 0 & church_lead != 0
    # } %>% {
    #   . * !duplicated(.)
    # }) %>%
    # dplyr::mutate(trt = ifelse(convert == 1,
    #                            1,
    #                            0)) %>%
    # dplyr::mutate(trt_lead2 = lead(trt,  n = 2),
    #               trt_lag = lag(trt)) %>%
    # dplyr::filter(!trt_lead2 == 1)  %>%  ##
     ungroup() %>%
    arrange(Id, wave)
}

head(new_data$data[[1]]$charity_l_lag)
head(new_data$data[[1]]$charity_l)

m = 10
mod = NULL

library(splines)

for(i in 1:m) {
  mod[[i]] <-lmer(charity ~  
                   church + 
                   church_lag +
                   charity_l_lag  +
                    SmokerEver_lag + 
                   Household.INC_scale_lag +
                   HomeOwner_scale_lag +
                   EthnicCats +
                   Employed_lag +
                   Urban_lag +
                   Edu_scale_lag +
                   Male_lag +
                   Pol.Orient_scale_lag +
                   NZdep_scale_lag +
                   #Religious1,
                   GenCohort +
                   Urban_lag  +
                   Partner_lag +
                   Parent_lag +
                   Believe.God_lag +
                   Believe.Spirit_lag +
                   AGREEABLENESS_scale_lag +
                   CONSCIENTIOUSNESS_scale_lag +
                   OPENNESS_scale_lag +
                   HONESTY_HUMILITY_scale_lag +
                   EXTRAVERSION_scale_lag +
                   NEUROTICISM_scale_lag +
                     (1|Id),
                  # family = poisson,
                   data = new_data$data[[i]])
}

# 
# # table
tab<-pool_parameters(mod)
tab %>%
  print_html()
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = F)


plot(ggeffects::ggpredict(mod[[1]], terms=c("church [0,3]")), 
     add.data=TRUE, dot.alpha =.01) + scale_y_continuous(limits = c(0,10000))




m <- 10
new_data2 <- NULL

for (i in 1:m) {
  new_data2$data[[i]] <- all_imputations$data[[i]] %>%
    dplyr::arrange("Id", "wave") %>%
    dplyr::mutate(church = log(Religion.Church+1),
                  vol = as.numeric(HoursCharity),
                  charity = as.integer(round(CharityDonate,1)),
                  charity_l = log(CharityDonate+1)) %>%
    dplyr::arrange("Id", "wave") %>%
    dplyr::mutate(across(
      c(#church,
        #vol,
        #charity
        Household.INC,
        HomeOwner,
        # EthnicCats,
        # Employed,
        # Urban,
        Edu,
        # Male,
        Pol.Orient,
        NZdep,
        # Religious1,
        # GenCohort,
        # Urban,
        # Partner,
        # Parent,
        # Believe.God,
        # Believe.Spirit,
        AGREEABLENESS,
        CONSCIENTIOUSNESS,
        OPENNESS,
        HONESTY_HUMILITY,
        EXTRAVERSION,
        NEUROTICISM),
      list(scale = scale)
    )) %>%
    dplyr::mutate(across(
      c( church,
         vol,
         charity,
         charity_l,
         SmokerEver,
         Household.INC_scale,
         HomeOwner_scale,
         EthnicCats,
         Employed,
         Urban,
         Edu_scale,
         Male,
         Pol.Orient_scale,
         NZdep_scale,
         #Religious1,
         GenCohort,
         Urban,
         Partner,
         Parent,
         Believe.God,
         Believe.Spirit,
         AGREEABLENESS_scale,
         CONSCIENTIOUSNESS_scale,
         OPENNESS_scale,
         HONESTY_HUMILITY_scale,
         EXTRAVERSION_scale,
         NEUROTICISM_scale),
      list(lag = lag)
    )) %>%
    dplyr::mutate(across(
      c(church_lag,
        vol_lag,
        charity_l_lag,
        charity_lag),
      list(lag = lag)
    ))%>%
    dplyr::mutate(across(
      c(church,
        vol,
        charity,
        charity_l),
      list(lead = lead)
    ))%>%
    group_by(Id) %>%
    mutate(convert = {
      church > 0 & church_lag ==0 & church_lag_lag == 0 & church_lead !=0
    } %>% {
      . * !duplicated(.)
    })%>%
    dplyr::mutate(trt = ifelse(convert == 1,
      1,
      0))%>%
    dplyr::mutate(
      trt_lead2 = lead(trt,  n=2),
      trt_lag = lag(trt)) %>%
    mutate(long_religious = church > 0 & church_lag > 0 & church_lag_lag > 0) %>%
    dplyr::filter(!trt_lead2 == 1)  %>%  ## 
    dplyr::filter(long_religious ==1 ) %>%
    ungroup() %>%
    arrange(Id, wave)
}

# 


table(new_data2$data[[1]]$trt)
length(new_data2$data[[1]]$Id)


log(4+1)
## model 2
m<- 10
mod2<-NULL
library(splines)
for(i in 1:m) {
  mod2[[i]] <-glm(charity_lead ~ trt +
                    #  charity_l_lag  +
                    #  Household.INC_scale_lag +
                    #  HomeOwner_scale_lag +
                    #  #EthnicCats_lag +
                    # # Employed_lag +
                    # # Urban_lag +
                    #  Edu_scale_lag +
                    #  Male_lag +
                    #  #Pol.Orient_scale_lag +
                    #  NZdep_scale_lag +
                    #  #Religious1,
                    #  GenCohort_lag +
                    # # Urban_lag  +
                    #  Partner_lag +
                    #  Parent_lag,
                    # # Believe.God_lag +
                    # # Believe.Spirit_lag +
                    # # AGREEABLENESS_scale_lag +
                    # # CONSCIENTIOUSNESS_scale_lag +
                    # # OPENNESS_scale_lag +
                    # # HONESTY_HUMILITY_scale_lag +
                    # # EXTRAVERSION_scale_lag +
                    # # NEUROTICISM_scale_lag +
                    # # (1|Id),
                  # family = poisson,
                   data = new_data2$data[[i]])
}


# table
tab<-pool_parameters(mod2)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)
model_char

new_data$data[[1]]

plot(ggeffects::ggpredict(mod2[[2]], terms=c("trt [0] ")))#,
     add.data=TRUE, dot.alpha =.01) + scale_y_continuous(limits = c(0,5000))

# 
# 
# 
# #        
#   #        %>% # get lag * 2
#   #          dplyr::mutate(across( # sometimes useful
#   #            c(religious_lag,
#   #              Env.ClimateChgReal_lag,
#   #              Male_lag,
#   #              Relid),
#   #            list(lag = lag)
#   #          )) %>%
#   #          dplyr::mutate(across(
#   #            c(religious,
#   #              Env.ClimateChgReal),
#   #            list(lead = lead)
#   #          )) %>%
#   #          dplyr::ungroup(Id)  %>%
#   #          droplevels() %>%
#   # mutate(always  = mean(Religious.Church, na.rm = TRUE) ) %>%# this is a hack, but works
#   # filter(!always > 0) %>% # Get rid of people who were always going to church 
#   # mutate(convert = {
#   #   Religious.Church > 3 & religious_lag ==0 & religious_lag_lag == 0 & religious_lead !=0
#   # } %>% {
#   #   . * !duplicated(.)
#   # })%>%
#   # mutate(trt = ifelse(
#   #   convert == 1,
#   #   1,
#   #   0))%>%
#   # dplyr::mutate(
#   #   trt_lead2 = lead(trt,  n=2),
#   #   trt_lag = lag(trt)) %>%
#   # dplyr::filter(!trt_lead2 == 1)  %>%  ## 
#   # ungroup() %>%
#   # arrange(Id, wave) %>%
#   # group_by(m) %>%
#   # nest()
# 
# 
# 
# library(broom)
# 
# ## CONDITIONS
# tf1<- tf %>%
#   arrange(Id,wave0) %>%
#   group_by(Id) %>%
#   mutate(always  = mean(religious, na.rm = TRUE) ) %>%# this is a hack, but works
#   filter(!always == 1) %>% # Get rid of people who were always religious 
#   mutate(convert = {
#     religious == 1 & religious_lag ==0 & religious_lag_lag == 0 & religious_lead !=0
#   } %>% {
#     . * !duplicated(.)
#   })%>%
#   mutate(trt = ifelse(
#     convert == 1,
#     1,
#     0))%>%
#   dplyr::mutate(
#     trt_lead2 = lead(trt,  n=2),
#     trt_lag = lag(trt)) %>%
#   dplyr::filter(!trt_lead2 == 1)  %>%  ## 
#   ungroup() %>%
#   arrange(Id, wave0)
# 
# 
# #table(tf1$trt)
# #sum(is.na(tf1$trt_lead2))
# 
# # how many Id's
# #length(unique(tf1$Id))
# 
# # How many trt vs contols? 
# #table(tf1$trt)
# 
# #   mixed effects
# mod2 <- lmer(Env.ClimateChgReal_lead ~ # outcome (after exposure)
#                trt + #exposure 
#                # religious_lag + # control for previous exposure
#                Env.ClimateChgReal_lag + #Control for previous outcome
#                Male_lag + # baseline confounder control
#                GenCohort + # baseline confounder control
#                yrs + # year trend
#                (1|Id),  # dependencies from the repeated measures 
#              data = tf1)
# 
# # Table
# mod2%>%
#   parameters() %>%
#   print_html()
# ```
# 
# No effect
# 
# ```{r}
# plot(ggeffects::ggemmeans(mod2, terms=c("trt")), add.data=TRUE, dot.alpha =.05) 
# ```
# 
# 
# 
# 
# Similarly, we do not find an effect with a GEE model
# 
# ```{r, eval=FALSE}
# library(geepack)
# # need to drop nas
# ts<-tf1%>%
#   drop_na()
# 
# ## Nothing
# m0 <- geeglm(
#   Env.ClimateChgReal_lead ~ # outcome (after exposure)
#     trt + #exposure 
#     # religious_lag + # control for previous exposure
#     Env.ClimateChgReal_lag + #Control for previous outcome
#     Male_lag + # baseline confounder control
#     GenCohort + # baseline confounder control
#     yrs,
#   data = ts,
#   id = Id,
#   family = "gaussian",
#   # weights = ipw,
#   waves = wave0,
#   corstr = "ar1"
# )
# 
# parameters(m0) %>%
#   print_html()
# ```
# 
# 
# ## Does loss of religion improve environmental attitudes
# 
# Another target trial emulation
# 
# ```{r}
# tf2<- tf %>%
#   arrange(Id,wave0) %>%
#   group_by(Id) %>%
#   mutate(always  = mean(religious, na.rm = TRUE) ) %>%# this is a hack, but works
#   filter(!always == 0) %>% # Get rid of people who were always secular 
#   mutate(deconvert = {
#     religious == 0 & religious_lag ==1 & religious_lag_lag == 1 & religious_lead ==0
#   } %>% {
#     . * !duplicated(.)
#   })%>%
#   mutate(trt = ifelse(
#     deconvert == 1,
#     1,
#     0))%>%
#   dplyr::mutate(
#     trt_lead2 = lead(trt,  n=2),
#     trt_lag = lag(trt)) %>%
#   dplyr::filter(!trt_lead2 == 1)  %>%
#   
#   ungroup() %>%
#   arrange(Id, wave0)
# 
# 
# #table(tf2$trt)
# #sum(is.na(tf2$trt_lead2))
# 
# # how many Id's
# #length(unique(tf2$Id))
# 
# # How many trt vs contols? 
# #table(tf2$trt)
# ```
# 
# We do not find a preservative effect of deconversion on environmental attitudes. 
# 
# ```{r}
# modD<- lmer(Env.ClimateChgReal_lead ~ # outcome (after exposure)
#               trt + #exposure 
#               # religious_lag + # control for previous exposure
#               Env.ClimateChgReal_lag + #Control for previous outcome
#               Male_lag + # baseline confounder control
#               GenCohort + # baseline confounder control
#               yrs + # year trend
#               (1|Id),  # dependencies from the repeated measures 
#             data = tf2)
# 
# ## Here we recover a stronger effect. 
# modD%>%
#   parameters() %>%
#   print_html()
# ```
# 
# GEE: same result
# 
# ```{r}
# ts2<-tf2%>%
#   drop_na()
# 
# ## Nothing
# m0d <- geeglm(
#   Env.ClimateChgReal_lead ~ # outcome (after exposure)
#     trt + #exposure 
#     Env.ClimateChgReal_lag + #Control for previous outcome
#     Male_lag + # baseline confounder control
#     GenCohort + # baseline confounder control
#     yrs,
#   data = ts2,
#   id = Id,
#   family = "gaussian",
#   # weights = ipw,
#   waves = wave0,
#   corstr = "exchangeable"
# )
# parameters(m0d) %>%
#   print_html()
# ```
# 
# 
# ## Conclusion
# 
# There find no evidence that religion causally affects beliefs in the reality of climate change
# 
# Need to investigate the causal effects of religion on other environmental attitudes and behaviours.  
# 
# Warning to those who read causation from associations
# 


