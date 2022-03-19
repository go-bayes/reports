
# Causal estimate of long term effect of terrorist attacks on muslim prejudice
# joseph.bulbulia@gmail.com


#libraries
library("here") # file management
library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation 
library("ggpubr") # graphs
library("viridis") # colours
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
library("cmdstanr") # backend brms
library("ipw") # inverse probability weighting
library("tidybayes") # workign with posterior probability distributions
library("bayesplot") # graphs
library("ggokabeito")   # color palette
library("gghalves")     #  half geoms
library("ggbeeswarm")   # Special distribution-shaped point jittering
library("emmeans") # estimate marginal means
library("table1") # tables /now with latex
library("tidyverse") # data wrangling

# rstan options
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme


# cite packages
cite_packages()


# import data -------------------------------------------------------------

df <- readRDS(here::here("data", "df"))


# timeline ----------------------------------------------------------------

# create timeline
df%>%
  dplyr::filter(Wave==2019) %>%
  select(TSCORE)%>%
  summarise(max(TSCORE, na.rm=TRUE))
# 3665 - min = 13 July 2019
# 4125 max = 15 October 2021

rarep<-df%>%
  dplyr::filter(YearMeasured == 1) %>%
  #dplyr::filter(Wave == 2018 | Wave == 2019) %>%
  # dplyr::group_by(Id) %>% filter(n() == 2) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020 ) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack!
  ungroup(Id)%>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30)+ TSCORE)%>%
  dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Condition = factor(
    ifelse(day >="2018-06-18" & day < "2019-03-15", 0,
           ifelse( day >="2019-03-15" & day < "2019-06-18",1,
                   ifelse(day >="2019-06-18" & day < "2020-10-15", 2, 3))),
    labels= c("Baseline","Post-attack", "Post-attack one year","Post-attack two years")))%>%
  arrange(day,Condition)


# get data of change of baseline
#dates_vline1<- as.Date("2018-06-18")

# get attack date
dates_vline2<- as.Date("2019-03-15")
dates_vline3<- as.Date("2019-06-18")
dates_vline4<-as.Date("2021-06-18")
#3665 - min = 13 July 2019
# 4125 max = 15 October 2021

# for line in a graph
dates_vline2b<-which(rarep$day %in% dates_vline2)
dates_vline3b<-which(rarep$day %in% dates_vline3)
dates_vline4b<-which(rarep$day %in% dates_vline4)

#rarep$Condition <- ordered(rarep$Condition, c("Baseline","Post-attack", "Post-attack one year","Post-attack two years"))


lds2 <- ggplot(rarep, aes(day, n)) + 
  geom_col(aes(fill = Condition)) + 
  scale_x_date(date_labels = "%b/%Y", limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2018 - 2021 daily counts by condition") + ylab("Count of Responses")+
  theme_classic()  + 
  # annotate(
  #   "rect",
  #   xmin = as.Date("2017-08-25"),
  #   xmax = dates_vline1,
  #   ymin = 0,
  #   ymax = 900,
  #   alpha = 0
  # ) +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 2000,
    alpha = .3,
    fill = "darkred"
  ) +
  # annotate(
  #   "rect",
  #   xmin = dates_vline1,
  #   xmax = dates_vline2,
  #   ymin = 0,
  #   ymax = 900,
  #   alpha = .3
  # ) +
  annotate(
    "rect",
    xmin = dates_vline3,
    xmax = as.Date("2020-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-10-15"),
    xmax = as.Date("2021-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "yellow"
  ) +
  annotate(
    "text",
    x = as.Date("2018-10-15"),
    y = 2600,
    label = "Time 10\npre-attacks"
  ) +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 2950,
           label = "**attack**") +
  annotate(
    "text",
    x = as.Date("2019-06-15"),
    y = 2600,
    label = "Time 10\npost-attacks"
  ) +
  annotate("text",
           x = as.Date("2020-03-01"),
           y = 2600,
           label = "Time 11 Year\nFollowing") +
  annotate("text",
           x = as.Date("2021-03-01"),
           y = 2600,
           label = "Time 12 2 years \nFollowing") +
  annotate(
    geom = "curve",
    x = as.Date("2018-11-15"),
    y = 2000,
    xend = as.Date("2020-02-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2300,
    xend = as.Date("2018-10-15"),
    yend = 2300,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2000,
    xend = as.Date("2019-01-01"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2019-01-01"),
    y = 2000,
    xend = as.Date("2020-10-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2020-03-15"),
    y = 2000,
    xend = as.Date("2021-06-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 6),
    legend.title = element_text(color = "Black", size = 8)
  ) +  scale_fill_viridis_d(option = "plasma") + 
  scale_y_continuous(limits=c(0,2950))
#theme(legend.position="none")


### USE GRAPH OF TIMELINE
lds2

# save graph
ggsave(
  lds2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




# time trend --------------------------------------------------------------


### Impute all waves of MUS PRED

# table1::table1(~ Partner|Wave, data = df)%>%
#   kbl()%>%
#   kable_paper("hover", full_width = F)

all_d <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    Partner,
    Parent,
    EthnicCats,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    GenCohort,
    Urban,
    TSCORE,
    EthnicCats,
    Employed,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    Muslim,
    TSCORE,
    WSCORE,
    YearMeasured,
    Religious,
    GenCohort
  ) %>%
  dplyr::filter(TSCORE < 3545) %>%
  dplyr::filter(
    Wave == 2012 &
      YearMeasured == 1 |
      Wave == 2013 &
      YearMeasured == 1 |
      Wave == 2014 &
      YearMeasured == 1 |
      Wave == 2016 &
      YearMeasured == 1 |
      Wave == 2017 &
      YearMeasured == 1 |
      Wave == 2018 &
      YearMeasured == 1 
  ) %>% 
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  droplevels() %>%
  arrange(Wave,Id) %>%
  # group_by(Id) %>%
  # dplyr::group_by(Id) %>% filter(n() > 6) %>%
  # dplyr::add_tally() %>%
  # filter(n == 7) %>%
  # arrange(Wave,Id) %>%
  dplyr::mutate(Warm.Muslims_lag = lag(Warm.Muslims))%>%
  droplevels() %>%
  ungroup() %>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = ifelse(TSCORE <= 3545, 0, 1)) %>% # 
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(yrs =  (dys/365))%>% 
  arrange(Id,Wave)%>%
  #   dplyr::filter(Wave == 2012 | Wave== 2013 | Wave == 2014 | Wave == 2015| Wave == 2016 | Wave ==2017 | Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  #   droplevels() %>%
  #   dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  #   group_by(Id) %>%
  #   dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  #   filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  #     ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2012", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2012", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2012", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2012", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2012", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2012", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2012", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2012", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2012", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2012", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
#  dplyr::mutate(As = as.factor(As)) %>%
  ungroup()%>%
  arrange(Id,Wave) 
levels(all_d$Wave) <- c("Time4", "Time5", "Time6","Time7", "Time8","Time9", "Time10")

str(all_d$As)

all_d_selected <-all_d%>%
  dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b, GenCohort)


# table 
t13<-table1::table1(~ Ys|Wave, data = all_d_selected, overall=FALSE)
kable(t13, format ="markdown", booktabs = TRUE)


# Amelia will center and scale continuous variables during the MI process. 

library(Amelia)



# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 

# assume Y^0|A=2018 = Y^0 2019
all_d_selected <- as.data.frame(all_d_selected)
imputed_m <- amelia(
  set.seed = 1234,
  all_d_selected,
  #dataset to impute
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  lags="Ys",
  leads="Ys",
  noms = c("EthnicCats_b","GenCohort"),
  idvars = c("Wave","As")
)


### IPW

# library(ipw)
# # 
# w_ipw <- ipwtm(
#   exposure = wave,
#   family = "gaussian",
#   # Time invariant
#   numerator =  ~ Male + GenCohort + Edu + EthnicCats,
#   # Time invariant and non-invariant confounders
#   denominator = ~ Male + GenCohort + Edu + EthnicCats + Religious + Pol.Orient + Urban+NZdep,
#   id = Id,
#  # timevar = Wave,
#   corstr = "exchangeable",
#   type = "all",
#   data = as.data.frame(km2)
# )

# km2 <- km2 %>% 
#   mutate(ipw = w_ipw$ipw.weights)
# #check no weights greater than 10
# max(w_ipw$ipw.weights) # max is 1.189481

library(splines)
m<-10
model_m<-NULL
for(i in 1:m) {
  model_m[[i]] <- lmer(Ys ~ bs(wave)  + (1|Id), data = imputed_m$imputations[[i]])
}

m<-10
model_ml<-NULL
for(i in 1:m) {
  model_ml[[i]] <- lmer(Ys ~ wave  + (1|Id), data = imputed_m$imputations[[i]])
}



tab<-pool_parameters(model_m)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

tab<-pool_parameters(model_ml)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

pa <- ggeffects::ggemmeans(model_m[[3]], terms = c("wave[all]")) 
ma <- ggeffects::ggemmeans(model_ml[[3]], terms = c("wave[all]")) 

plot_trend_spline <-plot(pa)+labs(title="Growth in Muslim Acceptance") +  scale_y_continuous(limits=c(3,5))#coord_flip() 
plot_trend_spline 

plot_trend_linear <-plot(ma)+labs(title="Growth in Muslim Acceptance") +  scale_y_continuous(limits=c(3,5))#coord_flip() 
plot_trend_linear 



# df for estimating time trend in attack sample ---------------------------
km_all3 <- df %>%
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
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 
levels(km_all3$Wave) <- c("Time10", "Time11","Time12")

# check N
km_all3
length(unique(km_all3$Id))
# correct
km_all3
t13<-table1::table1(~ Warm.Muslims|Wave * as.factor(Attack), data = km_all3, overall=FALSE)
t13
kable(t13, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all3, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)

km_2018 <- km_all3 %>% 
  dplyr::filter(Wave =="Time10")%>%
  droplevels()

x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = km_all3, overall = FALSE)
x

t1kable(x, format ="latex")


km_all3$As
# create new data set
library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", 0, 
                      ifelse(Wave=="Time10" & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", NA, 
                     ifelse(Wave=="Time10" & Attack == 0, NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id) 
# dat_all N
length(unique(kf3$Id))
str(kf3$As)
# correct


# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf3, overall=F)



## Bind - double dataset to creat missing values
ka3 <- km_all3%>%
  bind_rows(kf3)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka3$Ys)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
t2

t1kable(t2, format ="latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link dfs for zero estimate -----------------------------------------

km_zero <- ka3 %>%
  filter((As == 0))%>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort
  )%>%
  arrange(Wave,Id)

str(km_zero$As)
head(km_zero)
dim(km_zero)
# works
table1::table1(~ Ys|Wave*As, data = km_zero, overall=FALSE)


# remove wave 10 from the pre-attack sample (above)

km_pre<- all_d_selected %>%
  dplyr::filter(Wave != "Time10")%>%
  dplyr::select(-wave)%>% # wrong 
  dplyr::mutate(As = as.factor(As))%>%
  arrange(Wave,Id)

dim(km_pre)
head(km_pre)


str(km_pre$As)
length(unique(km_pre$Id))
str(km_pre$As)

# bind rows and arrange
bind_zero <-full_join(km_pre,km_zero)%>%
  arrange(Wave,Id)


# relevel
levels = c("Time4","Time5","Time6","Time7","Time8","Time9",
           "Time10","Time11","Time12")
bind_zero$Wave <- fct_relevel(bind_zero$Wave, levels)
levels(bind_zero$Wave)
str(bind_zero$Wave)

## Make numeric var

bind_zero1 <-bind_zero %>%
  mutate(wave = as.numeric(Wave) -1)

max(bind_zero1$wave)
# correct 
table1::table1(~ Ys|Wave * As, data = bind_zero, overall=FALSE)


# Impute 0s ------------------------------------------------------------


bind_zero1 <- as.data.frame(bind_zero1)

library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7
# find col number
head(bind_zero)
# create bounds
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0<- amelia(
  set.seed=1234,
  bind_zero1, 
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .01*nrow(bind_zero2)
  ) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0, here::here("_posts","mus","mods", "imputed0"))


# check means, looks good!

table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp10, overall=FALSE)


# impute 1s ---------------------------------------------------------------

km_one <- ka3 %>%
  filter((As == 1))%>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    wave # include wave
  )%>%
  arrange(Wave,Id)

length(unique(km_one$Id))

#check looks good
table1::table1(~ Ys|Wave * As, data = km_one, overall=FALSE)

# make data frame
km_one<-as.data.frame(km_one) 

# create bounds for Ys
head(km_one)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1<- amelia(
  set.seed=1234,
  km_one, 
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As"),
  lags="Ys",
  leads="Ys",
 # polytime = 2, #  polynomials perhaps not sensible given 
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .01*nrow(km_one)) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1, here::here("_posts","mus","mods", "imputed1"))

# check means, looks good
table1::table1(~ Ys|Wave * As, data = imputed1l$imputations$imp3, overall=FALSE)



# make frames compatible --------------------------------------------------


imp0 <- transform(imputed0, Wave = as.character(Wave))
imp1 <- transform(imputed1, Wave = as.character(Wave))


# bind data frames --------------------------------------------------------
levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
  "Time10","Time11","Time12")
newlevels = c("Time10","Time11","Time12")

m <- 10
zero <- NULL
for (i in 1:m) {
  zero$imputations$imp[[i]] <- imp0$imputations[[i]] %>%
    dplyr::mutate(Wave = as.factor(Wave)) %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, levels_old)) %>%
  droplevels()%>%
  arrange(Wave, Id)
}

one <- NULL

for(i in 1:m) {
  one$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = as.factor(Wave)) %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    arrange(Id) %>%  droplevels()
}


m <- 10
imps_bind <- NULL
for (i in 1:m) {
  imps_bind$imputations$imp[[i]] <- 
    dplyr::bind_rows(zero$imputations$imp[[i]],
                     one$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    droplevels()%>%
    arrange(Wave,Id) 
}



# Works!
table(imps_bind$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind, here::here("_posts", "mus", "mods", "imps_bind"))

# make list for bayesian models
listbayes <-imps_bind$imputations$imp

# save list for bayesian models
saveRDS(listbayes, here::here("_posts", "mus", "mods", "listbayes"))



# make wave continuous and as continuous ----------------------------------
m <- 10
imps_bindC <- NULL
for (i in 1:m) {
  imps_bindC$imputations$imp[[i]] <- 
    dplyr::bind_rows(zero$imputations$imp[[i]],
                     one$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    droplevels()%>%
    dplyr::mutate(As = as.numeric(As)-1) %>%
    dplyr::mutate(Wave = as.numeric(Wave)-1) %>%
    arrange(Wave,Id) 
}

str(imps_bindC$imputations$imp[[1]])
    
# make list for bayesian models
listbayesC <-imps_bindC$imputations$imp

# save list for bayesian models
saveRDS(listbayesC, here::here("_posts", "mus", "mods", "listbayesC"))


# ML model ----------------------------------------------------------------

# model
m<-10
model_all<-NULL
for(i in 1:m) {
  model_all[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_bind$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

pl_ml <- ggeffects::ggemmeans(model_all[[2]], terms = c("Wave","As")) 


mus_plot_model_all <-plot(pl_ml)+ 
  scale_y_continuous(limits=c(4.10,4.5))+
  labs(subtitle="Effect of attack on acceptance of Muslims") + 
  #scale_x_discrete(limits=rev) +
  coord_flip() 
mus_plot_model_all


estimate_contrasts(
  model_all[[4]],
  contrast = "As",
  at = c("As","Wave"),
  # fixed = NULL,
  # transform = "none",
  ci = 0.95,
  adjust = "holm",
  length = 2
)

estimate_contrasts( model_all[[1]],
                    contrast = "As",
                    at = c("Wave","As") )
%>%
  kbl("latex",booktabs = TRUE,digits=2)



# as and wave continuous --------------------------------------------------

# model
m<-10
model_allC_nl<-NULL
for(i in 1:m) {
  model_allC_nl[[i]] <- lmer(Ys ~  as.factor(As) * as.factor(Wave) + (1|Id),
                         data = imps_bindC$imputations$imp[[i]])
}

# table

tab<-pool_parameters(model_allC_nl)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)
q


estimate_contrasts(
  model_allC_nl[[4]],
  contrast = "As",
  at = c("As","Wave"),
  # fixed = NULL,
  # transform = "none",
  ci = 0.95,
  adjust = "holm",
  length = 2
)

estimate_contrasts( model_allC_nl[[1]],
                    contrast = "As",
                    at = c("Wave","As"),
                    length=2)
%>%
  kbl("latex",booktabs = TRUE,digits=2)



# ML model separate wave estimates ----------------------------------------
#  not needed
# 
# m <- 10
# imps_bind19 <- NULL
# for (i in 1:m) {
#   imps_bind19$imputations$imp[[i]] <- 
#     dplyr::bind_rows(zero$imputations$imp[[i]],
#                      one$imputations$imp[[i]])%>%
#     dplyr::select(-wave) %>%
#     dplyr::filter(Wave =="Time10"| Wave =="Time11") %>%
#     droplevels()%>%
#     arrange(Wave,Id) 
# }
# 
# 
# 
# # Works!
# table(imps_bind19$imputations$imp[[1]]$Wave)
# 
# # save
# saveRDS(imps_bind19, here::here("_posts", "mus", "mods", "imps_bind19"))
# 
# # make list for bayesian models
# listbayes19 <-imps_bind19$imputations$imp
# 
# # save list for bayesian models
# saveRDS(listbayes19, here::here("_posts", "mus", "mods", "listbayes19"))
# 
# # 20 
# 
# m <- 10
# imps_bind20 <- NULL
# for (i in 1:m) {
#   imps_bind20$imputations$imp[[i]] <- 
#     dplyr::bind_rows(zero$imputations$imp[[i]],
#                      one$imputations$imp[[i]])%>%
#     dplyr::select(-wave) %>%
#     dplyr::filter(Wave =="Time10"|  Wave == "Time12") %>%
#     droplevels()%>%
#     arrange(Wave,Id) 
# }
# 
# 
# 
# # Works!
# table(imps_bind20$imputations$imp[[1]]$Wave)
# 
# # save
# saveRDS(imps_bind20, here::here("_posts", "mus", "mods", "imps_bind20"))
# 
# # make list for bayesian models
# listbayes20 <-imps_bind20$imputations$imp
# 
# # save list for bayesian models
# saveRDS(listbayes20, here::here("_posts", "mus", "mods", "listbayes"))
# 
# 
# # ml 19 wave --------------------------------------------------------------
# 
# 
# # model
# m<-10
# model_all19<-NULL
# for(i in 1:m) {
#   model_all19[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_bind19$imputations$imp[[i]])
# }
# 
# # table
# tab19<-pool_parameters(model_all19)
# tab19
# tab19[,c(1:5)]%>%
#   # print_md()%>%
#   kbl("latex",booktabs = TRUE,digits=2)
# 
# plot(tab19, show_labels = TRUE)
# 
# pl_ml19 <- ggeffects::ggemmeans(model_all19[[2]], terms = c("Wave","As")) 
# 
# 
# mus_plot_model_all <-plot(pl_ml19)+ 
#   scale_y_continuous(limits=c(4.10,4.5))+
#   labs(subtitle="Effect of attack on acceptance of Muslims") + 
#   #scale_x_discrete(limits=rev) +
#   coord_flip() 
# mus_plot_model_all
# 
# 
# estimate_contrasts(
#   model_all19[[4]],
#   contrast = "As",
#   at = c("As","Wave"),
#   # fixed = NULL,
#   # transform = "none",
#   ci = 0.95,
#   adjust = "holm",
#   length = 2
# )
# 
# 
# 
# 
# # ml 20 -------------------------------------------------------------------
# 
# 
# # model
# m<-10
# model_all20<-NULL
# for(i in 1:m) {
#   model_all20[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_bind20$imputations$imp[[i]])
# }
# 
# # table
# tab20<-pool_parameters(model_all20)
# tab20
# tab20 [,c(1:5)]%>%
#   # print_md()%>%
#   kbl("latex",booktabs = TRUE,digits=2)
# 
# plot(tab, show_labels = TRUE)
# 
# pl_ml <- ggeffects::ggemmeans(model_all20[[2]], terms = c("Wave","As")) 
# 
# 
# mus_plot_model_all <-plot(pl_ml)+ 
#   scale_y_continuous(limits=c(4.10,4.5))+
#   labs(subtitle="Effect of attack on acceptance of Muslims") + 
#   #scale_x_discrete(limits=rev) +
#   coord_flip() 
# mus_plot_model_all
# 
# 
# estimate_contrasts(
#   model_all20[[4]],
#   contrast = "As",
#   at = c("As","Wave"),
#   # fixed = NULL,
#   # transform = "none",
#   ci = 0.95,
#   adjust = "holm",
#   length = 2
# )


# bayesian 20  ------------------------------------------------------------
# 
# # NO DIFF
# b_m20 <- brms::brm( 
#   bf(Ys ~ As  *  Wave + (1|Id)),#, 
#   # sigma ~ As + Wave + (1|Id)),
#   data = listbayes20,
#   # prior = c(prior(normal(.04, .5), class = b, coef = "WaveTime11"),
#   #           prior(normal(.08, .5), class = b, coef = "WaveTime12"),
#   #           prior(normal(0,  1), class = b, coef = "As1"),
#   #           prior(normal(0,.25), class=b, coef= "As1:WaveTime11"),
#   #           prior(normal(0,.25), class=b, coef= "As1:WaveTime12")), 
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "b_m20"))
# 
# tab20b<- lazerhawk::brms_SummaryTable(b_m20, panderize=F)
# tab20b
# tab20b %>%
#   kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
#   print()
# 
# 
# 
# 
# # bayesian 19 -------------------------------------------------------------
# 
# # no point
# 
# b_m19 <- brms::brm( 
#   bf(Ys ~ As  *  Wave + (1|Id)),#, 
#   # sigma ~ As + Wave + (1|Id)),
#   data = listbayes20,
#   # prior = c(prior(normal(.04, .5), class = b, coef = "WaveTime11"),
#   #           prior(normal(.08, .5), class = b, coef = "WaveTime12"),
#   #           prior(normal(0,  1), class = b, coef = "As1"),
#   #           prior(normal(0,.25), class=b, coef= "As1:WaveTime11"),
#   #           prior(normal(0,.25), class=b, coef= "As1:WaveTime12")), 
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "b_m19"))
# 
# tab19b<- lazerhawk::brms_SummaryTable(b_m19, panderize=F)
# tab19b
# tab19b
# b %>%
#   kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
#   print()
# 
# 


# bayesian model  ---------------------------------------------------------


b_m1 <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayes,
  prior = c(prior(normal(.04, .5), class = b, coef = "WaveTime11"),
            prior(normal(.08, .5), class = b, coef = "WaveTime12"),
            prior(normal(0,  1), class = b, coef = "As1"),
            prior(normal(0,.25), class=b, coef= "As1:WaveTime11"),
            prior(normal(0,.25), class=b, coef= "As1:WaveTime12")), 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_m1"))

tab <- lazerhawk::brms_SummaryTable(b_m1, panderize=F)
tab
tab %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
  print()


# default prior -----------------------------------------------------------
b_m_df <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayes,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_m_df"))

tab <- lazerhawk::brms_SummaryTable(b_m_df, panderize=F)

tab


# bayesian wave cont fact -------------------------------------------------------------

b_m_dfC <- brms::brm( 
  bf(Ys ~ as.factor(As)  *  as.factor(Wave) + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayesC,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  save_pars=save_pars(group=FALSE),
  file = here::here("_posts", "mus", "mods", "b_m_dfC"))

lazerhawk::brms_SummaryTable(b_m_dfC, panderize=F)

tabC <- model_parameters(b_m_dfC, test = c("pd"))

plot(tabC, show_labels = TRUE)


pl_bfC <- ggeffects::ggemmeans(b_m_dfC, terms = c("Wave","As")) 

mus_plot_pl_bfC <-plot(pl_bfC) + 
  #scale_y_continuous(limits=c(4.10,4.5)) +
  labs(subtitle="Effect of attack on acceptance of Muslims") + 
  # scale_x_continuous(limits=rev)
  coord_flip() 
mus_plot_pl_bfC


# bayesian cont wave  -------------------------------------------------------------
listbayesC<- readRDS(here::here("_posts", "mus", "mods", "listbayesC"))

b_m_dfCw <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayesC,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE),
  file = here::here("_posts", "mus", "mods", "b_m_dfCw"))

lazerhawk::brms_SummaryTable(b_m_dfCw, panderize=F)

tabCw <- model_parameters(b_m_dfCw, test = c("pd"))

plot(tabCw, show_labels = TRUE)
pl_bfCw <- ggeffects::ggemmeans(b_m_dfCw, terms = c("Wave","As")) 

mus_plot_pl_bfC <-plot(pl_bfC) + 
  #scale_y_continuous(limits=c(4.10,4.5)) +
  labs(subtitle="Effect of attack on acceptance of Muslims") + 
  # scale_x_continuous(limits=rev)
  coord_flip() 
mus_plot_pl_bfC



# cont wave /as strg prior ------------------------------------------------


b_m_dfC <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayesC,
  prior = c(prior(lognormal(log(1), .5), class = b, coef = "WaveTime11"),
            prior(lognormal(log(1), .5), class = b, coef = "WaveTime12"),
            prior(normal(0,  1), class = b, coef = "As1"),
            prior(normal(0,.25), class=b, coef= "As1:WaveTime11"),
            prior(normal(0,.25), class=b, coef= "As1:WaveTime12")), 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_m_dfC"))

tabC <- lazerhawk::brms_SummaryTable(b_m_dfC, panderize=F)

tabC



# nonlinear ---------------------------------------------------------------

library(splines)

b_m_dfC_nl <- brms::brm( 
  bf(Ys ~ bs(As)  +  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = listbayesC,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_m_dfC_nl"))

tabC <- lazerhawk::brms_SummaryTable(b_m_dfC_nl, panderize=F)

tabC


# sensitivity anlaysis ----------------------------------------------------
# imagine strong time effect

b_sens <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = listbayes,
  prior = c(set_prior("constant(0.1)", class = "b", coef = "WaveTime11"),
            set_prior("constant(0.2)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_sens")
)

lazerhawk::brms_SummaryTable(b_sens, panderize=F)


# sens analysis extrapolate -----------------------------------------------

b_sens_s <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = listbayes,
  prior = c(set_prior("constant(0.4)", class = "b", coef = "WaveTime11"),
            set_prior("constant(0.8)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_sens")
)

lazerhawk::brms_SummaryTable(b_sens, panderize=F)



# model graphs ------------------------------------------------------------

library(tidybayes)
library(emmeans)

marg_eff_bf_df <- b_m_df %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time10", "Time11","Time12")),
              re_formula = NA)
marg_eff_bf_df
#saveRDS(marg_eff_bf_df,  here::here("_posts", "mus", "mods", "marg_eff_bf_df"))
#marg_eff_bf_df <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_bf_df"))

marg_eff_sens <- b_sens %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time10", "Time11","Time12")),
              re_formula = NA)
saveRDS(marg_eff_sens,  here::here("_posts", "mus", "mods", "marg_eff_sens"))

# sensitivity
marg_eff_attack_0s <- m_use_pr_sens %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
#saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s"))

marg_eff_attack_0s
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))



marg_eff_attack_0s2 <- m_use_pr_sens2 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s2"))

marg_eff_attack_0s2
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

marg_eff_attack_0s3 <- m_use_pr_sens3 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)

saveRDS(marg_eff_attack_0s3,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s3"))



plot_bf_df <- ggplot(
  marg_eff_bf_df,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nModel estimates"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_bf_df

# marg_eff_m_use_pr <- m_use_pr %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# saveRDS(marg_eff_m_use_pr,  here::here("_posts", "mus", "mods", "marg_eff_m_use_pr"))
# 
#  <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

## plot all
# marg_eff_attack_0
plot_baseline_constr <- ggplot(
  marg_eff_attack_0,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: constrain baseline to Time 10"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_constr

# 
# plot_baseline_sens <- ggplot(
#   marg_eff_attack_0s,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) + 
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes: Time 10,11,12.\nSensitivity analysis: strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens


# plot_baseline_sens2 <- ggplot(
#   marg_eff_attack_0s2,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) + 
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens2


plot_baseline_sens <- ggplot(
  marg_eff_sens,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_sens

# ML Graph 

d_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) + labs(subtitle="maximu")

pl_ml <-plot(d_ml) +  
  scale_y_reverse() +
  coord_flip() +
  scale_y_continuous(limits=c(4,4.6))+
  labs(x = "Predicted Warmth Response",
       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
       title = '',
       fill = "Conterfactual Contrasts",
       subtitle = "Frequentist posterior locations of potential outcomes:\nSensitivity analysis: assume strong time effect") +
  theme_pubclean() + theme(legend.position = "bottom")
pl_ml

plot_baseline_constr
outplot <- (pl_ml + plot_baseline_unconstr ) /
  (plot_baseline_constr + plot_baseline_sens2 )  + 
  plot_annotation(tag_levels = 'a', title = "Estimation of  predicted marginal effects of the attack under different assumptions")
outplot
ggsave(
  outplot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "outplot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)







# old ---------------------------------------------------------------------



# filter only 0s and only 1s for the As
d0 <- ka_amelia %>%
  filter(As==0 )

d18_only0 <- ka_amelia %>%
  filter(As==0 & Wave == "Time 10")


d18_only1 <- ka_amelia %>%
  filter(As==1 & Wave =="Time 10")






# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19_only1 <- ka_amelia %>%
  filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))

d20_only1 <- ka_amelia %>%
  filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))




# old ---------------------------------------------------------------------




# df for ones -------------------------------------------------------------


# create new data set
# library(tidyverse)
# kf3  <- km_all3 %>%
#   group_by(Id) %>%  # All ZEROS
#   # mutate(Attack == as.numeric(Attack))%>%
#   mutate(As = (ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", 0, 
#                       ifelse(Wave=="Time 10" & Attack == 0, 1, Attack)))) %>%
#   mutate(Ys = ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", NA, 
#                      ifelse(Wave=="Time 10" & Attack == 0, NA, Warm.Muslims)))%>%
#   ungroup() %>%
#   arrange(Wave,Id) 



# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =dat_all_star, overall=F)



## Bind - double dataset to creat missing values
ka3 <- dat_all%>%
  bind_rows(dat_all_star)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka3$Ys)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
t2

t1kable(t2, format ="latex")
# impute missing data
# avoid collineraity

ka2_selected <-ka3%>%
  dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b,GenCohort)

ka_amelia<-as.data.frame(ka2_selected) 


# filter only 0s and only 1s for the As
d0 <- ka_amelia %>%
  filter(As==0 )

d18_only0 <- ka_amelia %>%
  filter(As==0 & Wave == "Time 10")


d18_only1 <- ka_amelia %>%
  filter(As==1 & Wave =="Time 10")






# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19_only1 <- ka_amelia %>%
  filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))

d20_only1 <- ka_amelia %>%
  filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))


library(Amelia)

# data needs to be a data frame if passed to Amelia
d0 <-as.data.frame(d0)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 
# assume Y^0|A=2018 = Y^0 2019





imputed_0<- amelia(
  set.seed=1234,
  d0, #dataset to impute
  # cs= c("Id"),
  #ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As", "Id","wave")) 

saveRDS(imputed_0, here::here("_posts","mus","mods", "imputed_0"))

imputed_18_only0<- amelia(
  set.seed=1234,
  d18_only0, #dataset to impute
  # cs= c("Id"),
  #ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As", "Id","wave")) 

saveRDS(imputed_18_only0, here::here("_posts","mus","mods", "imputed_18_only0"))


d18_only1<-data.frame(d18_only1)

## For 2018 v 2020 contrast
imputed_18only1<- amelia(
  set.seed=1234,
  d18_only1, #dataset to impute
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("As", "Wave", "Id","wave")) 

saveRDS(imputed_18only1, here::here("_posts","mus","mods", "imputed_18only1"))


# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
d19_only1<-as.data.frame(d19_only1)


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

imputed_19only1<- amelia(
  d19_only1, #dataset to impute
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("As", "Wave"),
  lags="Ys",
  leads="Ys") 

saveRDS(imputed_19only1, here::here("_posts","mus","mods", "imputed_19only1"))


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

d20_only1<-as.data.frame(d20_only1)
imputed_20only1 <- amelia(
  set.seed=14321,
  d20_only1, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As"),
  lags="Ys",
  leads="Ys")  # correct

saveRDS(imputed_20only1, here::here("_posts","mus","mods", "imputed_20only1"))


## 18 one
m<-10
imputed_18one<-NULL

for(i in 1:m) {
  imputed_18one$imputations$imp[[i]] <- imputed_18only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 10" & As == 1) %>%
    arrange(Wave, Id)
}

## 18 Zeros
m<-10
imputed_18zero<-NULL

for(i in 1:m) {
  imputed_18zero$imputations$imp[[i]] <- imputed_18_only0$imputations[[i]] %>% # diff
    dplyr::filter(Wave == "Time 10" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2019 Y^1s
m<-10
imputed_19one<-NULL

# use 20 wave: King says leads give better estimates (Amelia documentation)
for(i in 1:m) {
  imputed_19one$imputations$imp[[i]] <- imputed_19only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19one$imputations$imp[[1]]

## 19 Zeros
m<-10
imputed_19zero<-NULL

for(i in 1:m) {
  imputed_19zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2020 Y^1s
m<-10
imputed_20one<-NULL

for(i in 1:m) {
  imputed_20one$imputations$imp[[i]] <- imputed_20only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 1) %>%
    arrange(Wave, Id)
}

m<-10
imputed_20zero<-NULL

for(i in 1:m) {
  imputed_20zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 0) %>%
    arrange(Wave, Id)
}



# check

imputed_20one$imputations$imp[[1]]


# combine the data and arrange by wave

m<-10
imps_18 <-NULL
for(i in 1:m) {
  imps_18$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18zero$imputations$imp[[i]], 
                                                   imputed_18one$imputations$imp[[i]])
}

m<-10
imps_19 <-NULL
for(i in 1:m) {
  imps_19$imputations$imp[[i]] <- dplyr::bind_rows(imputed_19zero$imputations$imp[[i]], 
                                                   imputed_19one$imputations$imp[[i]])
}

m<-10
imps_20 <-NULL
for(i in 1:m) {
  imps_20$imputations$imp[[i]] <- dplyr::bind_rows(imputed_20zero$imputations$imp[[i]], 
                                                   imputed_20one$imputations$imp[[i]])
}


## ALL 1s


m<-10
imps_all <-NULL
for(i in 1:m) {
  imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
    imputed_18zero$imputations$imp[[i]],
    imputed_18one$imputations$imp[[i]],
    imputed_19zero$imputations$imp[[i]],
    imputed_19one$imputations$imp[[i]],
    imputed_20zero$imputations$imp[[i]],
    imputed_20one$imputations$imp[[i]]
  ) %>%
    arrange(Wave, Id)
}


# ## ALL 1s
# # m<-10
# # imps_trunc <-NULL
# # for(i in 1:m) {
# #   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
# #     imputed_18zero$imputations$imp[[i]],
# #     imputed_18one$imputations$imp[[i]],
# #    # imputed_19zero$imputations$imp[[i]],
# #     imputed_19one$imputations$imp[[i]],
# #   #  imputed_20zero$imputations$imp[[i]],
# #     imputed_20one$imputations$imp[[i]]
# #   ) %>%
# #     arrange(Wave, Id)
# # }
# 
# saveRDS(imps_trunc, here::here("_posts","mus","mods", "imps_all"))
# 
# # Save data
# #saveRDS(imps_all, here::here("_posts","mus","mods", "imps_all"))
# 
# 
# #mod 2018
# m<-10
# model_18<-NULL
# for(i in 1:m) {
#   model_18[[i]] <- lm(Ys ~ As, data = imps_18$imputations$imp[[i]])
#   }
# 
# pool_parameters(model_18)
# pred_18<- ggeffects::ggemmeans(model_18[[6]], terms = c("As"))
# mus_plot_18 <-plot(pred_18)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_18
# 
# 
# # mod 19
# #mod 2018
# m<-10
# model_19<-NULL
# for(i in 1:m) {
#   model_19[[i]] <- lm(Ys ~ As, data = imps_19$imputations$imp[[i]])
#   }
# 
# pool_parameters(model_19)
# pred_19<- ggeffects::ggemmeans(model_19[[6]], terms = c("As"))
# mus_plot_19 <-plot(pred_19)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_19
# 
# # mod 20
# m<-10
# model_20<-NULL
# for(i in 1:m) {
#   model_20[[i]] <- lm(Ys ~ As, data = imps_20$imputations$imp[[i]])
#   }
# 
# 
# pool_parameters(model_18)
# pool_parameters(model_19)
# pool_parameters(model_20)
# 
# 
# model_20<- ggeffects::ggemmeans(model_20[[6]], terms = c("As"))
# mus_plot_20 <-plot(model_20)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_18 + mus_plot_19 + mus_plot_20


# all one


imps_all$imputations$imp[[1]]$Wave
# #for brms model
dat_l<-imps_all$imputations$imp
#saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# aveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))


#USE
m<-10
model_all<-NULL
for(i in 1:m) {
  model_all[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_all$imputations$imp[[i]])
}



# USE
tab<-pool_parameters(model_all)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

pl_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) 


mus_plot_model_all <-plot(pl_model_all)+ scale_y_continuous(limits=c(4.10,4.5))+labs(subtitle="Effect of attack on acceptance of Muslims") +  coord_flip() 
mus_plot_model_all




## Another approach a little better for writing code, identical results
library(merTools)
#imps_all<- readRDS(here::here("_posts", "mus", "mods","imps_all"))

modList <- lmerModList(Ys ~  Wave*As + (1|Id), data =  imps_all$imputations$imp)
fixef(modList) # model with dropped missing

modelFixedEff(modList)[,c(1:5)]%>%
  kbl("latex",booktabs = TRUE,digits=3)




estimate_means(
  model_all[[1]],
  contrast = "As",
  at = c("As","Wave"),
  # fixed = NULL,
  # transform = "none",
  ci = 0.95,
  adjust = "holm",
  length = 2
)

estimate_contrasts( model_all[[1]],
                    contrast = "As",
                    at = c("Wave","As") )%>%
  kbl("latex",booktabs = TRUE,digits=2)





mus_plot_18 + mus_plot_19 + mus_plot_20 



# str(dat4)
# str(dat3)
#saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # saveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
#  dat_l <- readRDS(here::here("_posts", "mus", "mods", "dat_l"))
# 
# tscsPlot(imputed_20only1, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
#   obscol = "black")
# 

## 2018-2020 contrast
# m<-10
# imps_base2020 <-NULL
# 
# for(i in 1:m) {
#   imps_base2020$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18b$imputations[[i]], 
#                                       imputed_18_3$imputations[[i]],
#                                       imputed_20s_3$imputations$imp[[i]]%>%
#                                        droplevels())
#   imps_base2020$imputations$imp[[i]] <- imps_base2020$imputations$imp[[i]] %>% arrange(Wave,Id)
# }
# 
# m<-10
# imps_d <-NULL
# dat_d <- NULL
# 
# for(i in 1:m) {
#   imps_d$imputations[[i]]<- as.data.frame(imps_base2020$imputations$imp[[i]])
# }
# 
# dat_d<-imps_d$imputations
# m<-10
# models_d<-NULL
# for(i in 1:m) {
#   models_d[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=imps_d$imputations[[i]])
# }
# 
# pool_parameters(models_d)
# pred__base20202<- ggeffects::ggemmeans(models_d[[6]], terms = c("Wave","As"))
# plot(pred__base20202)
# mus_plot_base20202 <-plot(pred__base20202)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot





# df three waves ----------------------------------------------------------


library(tidyverse)
df_raw<-df%>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020 ) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack!
  ungroup(Id)%>%
  # filter(n == 2 ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Attack = ifelse(TSCORE >= 3545, 1, 0)) %>%
  ungroup()

# table 
table1::table1(~ Warm.Muslims |Wave*as.factor(Attack), data = df_raw, overall=FALSE)

raw<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = df_raw, overall=FALSE)
raw
kable(raw, format ="latex", booktabs = TRUE)


# data frame for three waves imputation -----------------------------------

km_all <- df %>%
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
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE >= 3545 & Wave == 2018)|(Wave==2019), 1, 0)))) %>% # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  select(Id, Wave, Ys, As, Warm.Muslims, Attack, Age, EthnicCats, Edu, Employed, Urban, Male, GenCohort, Religious, Pol.Orient, Parent, Partner, Urban, NZdep,wave) %>%
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 

# check N
length(unique(km_all$Id))
# correct

km_all_baseline<- km_all%>%
  filter(Wave==2018)

t1<-table1::table1(~ Warm.Muslims|Wave*as.factor(Attack), data = km_all, overall=FALSE)
kable(obtl, format ="latex", booktabs = TRUE)

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)


x <- table1::table1(~ Warm.Muslims + Age + Male + GenCohort + Edu + Employed + EthnicCats  + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave*Attack, data = km_all, overall=FALSE)

t1kable(x, format ="latex")


km_all$As
# create new data set
library(tidyverse)
kf  <- km_all %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave==2018 & Attack == 1|Wave == 2019, 0, 
                      ifelse(Wave==2018 & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave==2018 & Attack == 1|Wave ==2019, NA, 
                     ifelse(Wave==2018 & Attack == 0, NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id) 



# Test  = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf, overall=F)



## Bind - double dataset to creat missing values
ka <- km_all %>%
  bind_rows(kf)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) %>%
  select(-c(Warm.Muslims,Attack))

head(ka)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka, overall=F)

# impute missing data
# avoid collineraity
ka2<-ka%>%
  select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)

ka2<-as.data.frame(ka2) 


### WITHOUT JOINT ASSUMPTION 
# filter only 0s and only 1s for the As
d0 <- ka2 %>%
  filter(As==0 )

d18 <- ka2 %>%
  filter(As==1 & Wave ==2018)

# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19 <- ka2 



#ka3$Wave<-as.numeric(ka3$Wave)

# Amelia will center and scale continuous variables during the MI process. 

library(Amelia)

# data needs to be a data frame if passed to Amelia
d0 <-as.data.frame(d0)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 

# assume Y^0|A=2018 = Y^0 2019

imputed_0<- amelia(
  set.seed=1234,
  d0, #dataset to impute
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) 

head(d0)



# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
d18<-as.data.frame(d18)

imputed_18<- amelia(
  d18, #dataset to impute
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As","Id","wave")) #, # do 


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

d19<-as.data.frame(d19)
imputed_19<- amelia(
  set.seed=14321,
  d19, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) # correct


# Here filter only the 2019 Y^1s

m<-10
imputed_19s<-NULL

for(i in 1:m) {
  imputed_19s$imputations$imp[[i]] <- imputed_19$imputations[[i]] %>%
    dplyr::filter(Wave == 2019 & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19s$imputations$imp[[1]]


m<-10
imps <-NULL
for(i in 1:m) {
  imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]], 
                                                imputed_18$imputations[[i]],
                                                imputed_19s$imputations$imp[[i]])
  imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
}

# 
# m<-20
# imps<-NULL
# for(i in 1:m) {
#   imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]], 
#                                      imputed_1$imputations[[i]])
#   imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
#   
# }



m<-10
impsL <-NULL
dat <- NULL

for(i in 1:m) {
  impsL$imputations[[i]]<- as.data.frame(imps$imputations$imp[[i]])
}

# dat <- impsL$imputations
# saveRDS(dat, here::here("_posts", "mus", "mods", "dat"))
# saveRDS(impsL, here::here("_posts", "mus", "mods", "impsL"))
# saveRDS(imps, here::here("_posts", "mus", "mods", "imps"))
impsL <- readRDS(here::here("_posts", "mus", "mods", "impsL"))

tscsPlot(imputed, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
         obscol = "black")


m<-10
models1<-NULL
for(i in 1:m) {
  models1[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=impsL$imputations[[i]])
}

pool_parameters(models1)
pred1<- ggeffects::ggemmeans(models1[[1], terms = c("Wave","As"))
plot(pred1)


```

Graph


```{r}

#library(Amelia)
# see: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/, who writes:
# unclass() is necessary because bind_rows() will complain when dealing with
# lists with the "amelia" class, which is what amelia() returns

all_imputations <- bind_rows(unclass(imputed$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

all_imputations <- bind_rows(unclass(imps$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

unnest(all_imputations) %>%
  select(m, Wave, As,Ys )%>%
  arrange((m), Wave) %>%
  group_by(m,Wave,As) %>%
  rename( Dataset = m,
          Muslim_Warmth = Ys,
          Exposure = As) %>%
  summarise(mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
  pivot_wider(names_from = c("Exposure","Wave"), values_from = c(mean,sd))%>%
  mutate( across(where(is.numeric), round, 3)) 
%>%
  kbl("latex",booktabs =T, caption = "Imputed mean/sd for warmth to Muslims by condition and wave")


unnest(all_imputations) %>%
  select(m, Wave, As,Ys )%>%
  group_by(Wave,As) %>%
  rename( Dataset = m,
          Muslim_Warmth = Ys,
          Exposure = As) %>%
  summarise(n = n(), mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
  mutate( across(where(is.numeric), round, 3)) %>%
  kbl("latex",booktabs =T, caption = "Overall imputed mean/sd for warmth to Muslims by condition and wave")


# This doesn't work
# all_imputations_d <- bind_rows(unclass(imputed_d$imputations), .id = "m") %>%
#   group_by(m) %>%
#   nest()
# 
# unnest(all_imputations_d) %>%
#   select(m, Wave, As,Ys )%>%
#    group_by(Wave,As) %>%
#   rename( Dataset = m,
#           Muslim_Warmth = Ys,
#           Exposure = As) %>%
#    summarise(n = n(), mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
#   mutate( across(where(is.numeric), round, 3)) %>%
#    kbl("latex",booktabs =T, caption = "Overall imputed mean/sd for warmth to Muslims by condition and wave")

unnest(all_imputations_d) %>%
  select(m, Wave, As,Ys )%>%
  arrange((m), Wave) %>%
  group_by(m,Wave,As) %>%
  rename( Dataset = m,
          Muslim_Warmth = Ys,
          Exposure = As) %>%
  summarise(mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
  pivot_wider(names_from = c("Exposure","Wave"), values_from = c(mean,sd))%>%
  mutate( across(where(is.numeric), round, 3)) 
```

## FULL BAYES



```{r}
dat <-readRDS(here::here("_posts","mus","mods","dat"))

## Do not save residuals
# m_bayes <- brms::brm( 
#   Ys ~ As *  Wave + (1|Id), 
#   data = dat, 
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#  # backend = "cmdstanr",
#   save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_bayes.rds"))


m_bayesR <- brms::brm( 
  Ys ~ As *  Wave + (1|Id), 
  data = dat, 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_bayesR.rds"))

lazerhawk::brms_SummaryTable(m_bayesR)
prior_summary(m_bayesR)

# Print Table
ouput <- tidy(m_us)
str(ouput)
ouput
#output<- as.data.frame(ouput)


library(lazerhawk)
blh_tab <- brms_SummaryTable(m_us, panderize=F)
blh_tab
blh_tab %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
  print()


# smaller file
save_pars=save_pars(group=FALSE)


## prior = c(prior(normal(0, 1), class = b)),

#amelia.list<-readRDS(here::here("_posts","mus", "mods", "amelia.list"))

```


### Distributional model

```{r}
m_dis <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id), 
     sigma ~ As * Wave + (1|Id)),
  data = dat,
  prior = c(prior(lognormal(0, 0.25), class = b, coef = "Wave")), # constrain wave to be positive
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_dis"))
```


### Graphs 

```{r}
#### GRAPH
library(tidybayes)
library(emmeans)
marg_eff_attack_0a <- m_bayesR %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("2018", "2019")),
              re_formula = NA)

#saveRDS(marg_eff_attack_0a,  here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
#marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

#marg_eff_attack_0a
# Obtain contrasts
# marg_eff_attack_0a<- marg_eff_attack_0a%>%
#   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)

scale_y_discrete(limits=rev) 


# marg_eff_attack_0
plot_all <- ggplot(
  marg_eff_attack_0a,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  #  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )
plot_all

ggsave(
  plot_all,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 8,
  height = 8,
  units = "in",
  filename = "plot_all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

#plot_all

# 
# # 
# grand_mean_ame_a <- m_bayesR %>%
#   emmeans(~ As*Wave,
#           epred = TRUE, re_formula = NA) %>%
#   contrast() %>%
#   gather_emmeans_draws() 
# 
# grand_mean_ame_a
# #   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# 
# saveRDS(grand_mean_ame_a,  here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# #grand_mean_ame_a <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# 
# # remove erro contrasts / we don't observe 0 in year two 
# 

# does not

# plot_all__ame <- ggplot(grand_mean_ame_a,
#                                      aes(x = .value)) +
#   stat_halfeye(slab_alpha = 0.75, fill = "grey") +
#   scale_fill_okabe_ito(order = c(3, 4)) +
#   labs(x = "Average marginal effect of attack on warmth to Muslims",
#        y = "Density") +
#   #facet_wrap(vars(region)) +
#   theme_classic() + 
#   theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
# plot_all__ame 
# Try
library(patchwork)
bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
bayes_plots_a

# ggsave(
#   bayes_plots_a,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "bayes_plots_a.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )


# 
# blh_tab %>%
#    kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
#     print()
```

## MI Bayes

```{r}
## Run on my other mac, cmdstanr was ausing problems
gc()

plot_bayes <- m_us %>%
  emmeans( ~ As * Wave) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye()



marg_eff_attack_0ab <- m_us %>%
  epred_draws(newdata = expand.grid(As = c(0, 1),Wave = c("2018", "2019")),
              re_formula =NA)

saveRDS(marg_eff_attack_0ab,  here::here("_posts", "mus", "mods", "marg_eff_attack_0ab.rds"))
#marg_eff_attack_0ab <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0ab.rds"))


# Obtain contrasts

marg_eff_attack_0ab
plot_all <- ggplot(
  marg_eff_attack_0ab,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  #  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    y = "w2018(years 2018-2019); w2019 (years 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )
plot_all
#plot_all


grand_mean_ame_ab <- m_us %>% 
  emmeans(~ As*Wave,
          epred = TRUE, re_formula = NA) %>% 
  gather_emmeans_draws() 

grand_mean_ame_ab
grand_mean_ame_ab
saveRDS(grand_mean_ame_ab,  here::here("_posts", "mus", "mods", "grand_mean_ame_ab.rds"))
grand_mean_ame_ab <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_ab.rds"))
grand_mean_ame_ab%>%
  slice(10000)
# remove erro contrasts / we don't observe 0 in year two 


plot_all__ame_ab <- ggplot(grand_mean_ame_ab,
                           aes(x = .value)) +
  stat_halfeye(slab_alpha = 0.75, fill = "grey") +
  scale_fill_okabe_ito(order = c(3, 4)) +
  labs(x = "Average marginal effect of attack on warmth to Muslims",
       y = "Density") +
  #facet_wrap(vars(region)) +
  theme_classic() + 
  theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
plot_all__ame_ab 
library(patchwork)
bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
bayes_plots_a

ggsave(
  bayes_plots_a,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "bayes_plots_a.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

```




## Weights graph

```{r}

# set up data
kn <- km_all
km_all$Attack
kn$attack <- km_all$Attack


# set up of data
imputed1<-imps$imputations$imp[[1]]
im$attack <- as.numeric(im$As)-1
im$warmth <- as.numeric(im$Ys)
kn$warmth <- as.numeric(kn$Warm.Muslims)
im$attack


# check
kn%>%
  filter(Wave ==2019 & As ==1) %>%
  select(Warm.Muslims) %>%
  count(is.na(.))


# kna<-km_all%>%filter(Wave==2019)
# hist(kna$Warm.Muslims)

plot1 <- ggplot() +
  geom_histogram(data = filter(im, attack == 1 & Wave == 2018),
                 #  bins = 10,
                 aes(x = warmth),
                 fill = colorspace::lighten("deepskyblue4", 0.55)) +
  geom_histogram(data = filter(im, attack == 0 & Wave == 2018),
                 # bins = 10,
                 aes(x = warmth, y = -..count..),
                 fill = colorspace::lighten("chocolate", 0.55))  +
  geom_histogram(data = filter(kn, attack == 1& Wave == 2018),
                 #bins = 10,
                 aes(x = warmth),
                 fill = colorspace::lighten("deepskyblue4", 0.1))+
  geom_histogram(data = filter(kn, attack == 0 & Wave == 2018),
                 # bins = 10,
                 aes(x = warmth, y = -..count..),
                 fill = colorspace::lighten("chocolate", 0.1)) +
  annotate(geom = "label", x = 1.5, y = 3000, label = "Exposed\nobserved",
           fill = colorspace::lighten("deepskyblue4",.1), color = "white", hjust = 1) +
  annotate(geom = "label", x = 3.5, y = 7000, label = "Exposed\nimputed",
           fill = colorspace::lighten("deepskyblue4",.35), color = "white", hjust = 1)+
  annotate(geom = "label", x = 1.5, y = -5000, label = "Not Exposed\nobserved",
           fill = colorspace::lighten("chocolate", 0.1), color = "white", hjust = 1) +
  annotate(geom = "label", x = 3.5, y = -10000, label = "Not Exposed\nimputed",
           fill = colorspace::lighten("chocolate",  .35), color = "white", hjust = 1) +
  geom_hline(yintercept = 0, color = "white", size = 0.25) +
  scale_y_continuous(label = abs) +
  scale_y_continuous(limits=c(-20000,15000)) +
  # coord_cartesian(xlim = c(0.1, 1)) +
  labs(x = "Warmth to Muslim", y = "Counts") + labs(title= "Time 10")




plot1

length(unique(kn$Id))
length(unique(imputed1$Id))

hist(kn$Warm.Muslims)

plot2 <- ggplot() +
  geom_histogram(data = filter(im, attack == 1 & Wave == 2019),
                 # bins = 10,
                 aes(x = warmth),
                 fill = colorspace::lighten("deepskyblue4", 0.55)) +
  geom_histogram(data = filter(im, attack == 0 & Wave == 2019),
                 # bins = 10,
                 aes(x = warmth, y = -..count..),
                 fill = colorspace::lighten("chocolate", 0.55))  +
  geom_histogram(data = filter(kn, attack == 1 & Wave == 2019),
                 #bins = 10,
                 aes(x = warmth),
                 fill = colorspace::lighten("deepskyblue4", 0.1))+
  geom_histogram(data = filter(kn, attack == 0 & Wave == 2019),
                 #  bins = 10,
                 aes(x = warmth, y = -..count..),
                 fill = colorspace::lighten("chocolate", 0.1)) +
  annotate(geom = "label", x = 2.5, y = 5000, label = "Exposed\nobserved",
           fill = colorspace::lighten("deepskyblue4",.1), color = "white", hjust = 1) +
  annotate(geom = "label", x = 3.5, y = 14000, label = "Exposed\nimputed",
           fill = colorspace::lighten("deepskyblue4",.35), color = "white", hjust = 1)+
  annotate(geom = "label", x = 2.5, y = -500, label = "Not Exposed is\nfully imputed",
           fill = colorspace::lighten("chocolate",  .35), color = "white", hjust = 1) +
  geom_hline(yintercept = 0, color = "white", size = 0.25) +
  scale_y_continuous(label = abs) +
  # coord_cartesian(xlim = c(0.1, 1)) +
  labs(x = "Warmth to Muslim", y = "Counts") + labs(title= "Time 11") +
  scale_y_continuous(limits=c(-20000,15000))
plot2
imp_frq<- plot1 + plot2 + plot_annotation(tag_levels = "a", title = "Comparison of multiply-imputed responses and observed\nresponse distributions in Wave 10 and Wave 11")


imp_frq



ggsave(
  imp_frq,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "imp_frq.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



model_treatment_freq <- glm(attack ~    Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,  data = kn,family = binomial(link = "logit"))


# Step 2: Use the treatment model to calculate propensity scores, and
# Step 3: Use the propensity scores to calculate inverse probability of treatment weights
km_with_weights <- augment(model_treatment_freq, kn,
                           type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))


max(km_with_weights$iptw)


# Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
model_outcome_freq <- lm(Warm.Muslims ~ Attack   ,
                         data = km_with_weights,
                         weights = iptw)

# Coefficient
tidy(model_outcome_freq)




library(MetBrewer)
# ggplot() +
#   geom_histogram(data = filter(km, Attack == 1),
#                  bins = 50, aes(x = ipw),
#                  fill = colorspace::lighten("darkorange", 0.5)) +
#    geom_histogram(data = filter(km, Attack == 0),
#                  bins = 50, aes(x = ipw, y = -..count..),
#                  fill = colorspace::lighten("darkgreen", 0.5)) +
#   geom_hline(yintercept = 0) +
#   annotate(geom = "label", x = 0.8, y = 2000, label = "Treated",
#            fill = colorspace::lighten("darkorange", 0.2), color = "white", hjust = 0) +
#   annotate(geom = "label", x = 0.8, y = -2000, label = "Untreated",
#            fill = colorspace::lighten("darkgreen", 0.2), color = "white", hjust = 0) +
#   scale_y_continuous(label = abs) +
#  #coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-10000, 10000)) +
#   labs(x = "Propensity", y = "Count")


#
imputed
ipwplot <- ggplot() +
  geom_histogram(data = filter(km_with_weights, attack == 1),
                 bins = 50, aes(x = propensity, weight = iptw),
                 fill = colorspace::lighten("chocolate", 0.55)) +
  geom_histogram(data = filter(km_with_weights, attack == 0),
                 bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.55)) +
  geom_histogram(data = filter(km_with_weights, attack == 1),
                 bins = 50, aes(x = propensity),
                 fill = colorspace::lighten("chocolate", 0.1)) +
  geom_histogram(data = filter(km_with_weights, attack == 0),
                 bins = 50, aes(x = propensity, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.1)) +
  annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
           fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
  annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (imputed\npseudo-population)",
           fill = colorspace::lighten("chocolate",.35), color = "white", hjust = .5)+
  annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
           fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
  annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (imputed\npseudo-population)",
           fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
  geom_hline(yintercept = 0, color = "white", size = 0.25) +
  scale_y_continuous(label = abs) +
  # coord_cartesian(xlim = c(0.1, 1)) +
  labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")
ipwplot



model_treatment_freq <- glm(attack ~
                              Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,
                            data = kp18,family = binomial(link = "logit"))


# Step 2: Use the treatment model to calculate propensity scores, and
# Step 3: Use the propensity scores to calculate inverse probability of treatment weights
km_with_weights <- augment(model_treatment_freq, kp18,
                           type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))


max(km_with_weights$iptw)


# Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
model_outcome_freq <- lm(Warm.Muslims ~ Attack ,
                         data = km_with_weights,
                         weights = iptw)

# Coefficient
tidy(model_outcome_freq)




library(MetBrewer)
# ggplot() +
#   geom_histogram(data = filter(km, Attack == 1),
#                  bins = 50, aes(x = ipw),
#                  fill = colorspace::lighten("darkorange", 0.5)) +
#    geom_histogram(data = filter(km, Attack == 0),
#                  bins = 50, aes(x = ipw, y = -..count..),
#                  fill = colorspace::lighten("darkgreen", 0.5)) +
#   geom_hline(yintercept = 0) +
#   annotate(geom = "label", x = 0.8, y = 2000, label = "Treated",
#            fill = colorspace::lighten("darkorange", 0.2), color = "white", hjust = 0) +
#   annotate(geom = "label", x = 0.8, y = -2000, label = "Untreated",
#            fill = colorspace::lighten("darkgreen", 0.2), color = "white", hjust = 0) +
#   scale_y_continuous(label = abs) +
#  #coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-10000, 10000)) +
#   labs(x = "Propensity", y = "Count")


#
imputed
ipwplot <- ggplot() +
  geom_histogram(data = filter(km_with_weights, attack == 1),
                 bins = 50, aes(x = propensity, weight = iptw),
                 fill = colorspace::lighten("chocolate", 0.55)) +
  geom_histogram(data = filter(km_with_weights, attack == 0),
                 bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.55)) +
  geom_histogram(data = filter(km_with_weights, attack == 1),
                 bins = 50, aes(x = propensity),
                 fill = colorspace::lighten("chocolate", 0.1)) +
  geom_histogram(data = filter(km_with_weights, attack == 0),
                 bins = 50, aes(x = propensity, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.1)) +
  # annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
  #         fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
  # annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (imputed\npseudo-population)",
  #           fill = colorspace::lighten("chocolate",.35), color = "white", hjust = .5)+
  # annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
  #         fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
  # annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (imputed\npseudo-population)",
  #          fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
  # geom_hline(yintercept = 0, color = "white", size = 0.25) +
  # scale_y_continuous(label = abs) +
  # coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")
ipwplot





model_treatment_freq <- glm(attack ~
                              Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,
                            data = kp19,family = binomial(link = "logit"))


# Step 2: Use the treatment model to calculate propensity scores, and
# Step 3: Use the propensity scores to calculate inverse probability of treatment weights
km_with_weights <- augment(model_treatment_freq, kp19,
                           type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))


min(km_with_weights$iptw)


# Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
model_outcome_freq <- lm(Warm.Muslims ~ Attack ,
                         data = km_with_weights,
                         weights = iptw)

# Coefficient
tidy(model_outcome_freq)





ind <- imputed1
ind$propensity<- rep(1,nrow(ind))
ind$iptw<- rep(1,nrow(ind))
ind$attack <- ind$As

ipwplot <- ggplot() +
  geom_histogram(data = filter(ind, attack == 1),
                 bins = 50, aes(x = propensity, weight = iptw),
                 fill = colorspace::lighten("chocolate", 0.55)) +
  geom_histogram(data = filter(ind, attack == 0),
                 bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.55)) +
  geom_histogram(data = filter(km_with_weights, attack == 1),
                 bins = 50, aes(x = propensity),
                 fill = colorspace::lighten("chocolate", 0.1)) +
  geom_histogram(data = filter(km_with_weights, attack == 0),
                 bins = 50, aes(x = propensity, y = -..count..),
                 fill = colorspace::lighten("deepskyblue4", 0.1)) +
  #  annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
  #          fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
  #  annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (IPTW\npseudo-population)",
  #            fill = colorspace::lighten("chocolate",.35), color = "white", hjust = 1)+
  #  annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
  #          fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
  #  annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (IPTW\npseudo-population)",
  #           fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
  #  geom_hline(yintercept = 0, color = "white", size = 0.25) +
  #  scale_y_continuous(label = abs) +
  # # coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")#+ scale_y_continuous(limits=c(-15000,15000))
ipwplot



ggsave(
  ipwplot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "ipweightplot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


```


## 3 waves approach 1

```{r}

# select variables
library(tidyverse)
scale_y_discrete(limits=rev) 
# restrict to only thise who compleated all response
df_raw<- df%>%
  dplyr::filter( Wave == 2018 & YearMeasured ==1 | Wave == 2019 & YearMeasured ==1 | Wave == 2020 & YearMeasured ==1) %>%
  droplevels() %>%
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
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  group_by(Id)%>%
  filter(n() == 3) %>%
  dplyr::add_tally() %>%
  filter(n == 3 ) %>%3
dplyr::ungroup() %>%
  dplyr::mutate(Attack = ifelse(TSCORE >= 3545, 1, 0)) %>%
  ungroup()%>%
  drop_na()

t13<-table1::table1(~ Warm.Muslims|Wave*as.factor(Attack), data = df_raw, overall=FALSE)
t13
kable(t13, format ="latex", booktabs = TRUE)

library(tidyverse)
km_all3 <- df %>%
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
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 
km_all3


levels(km_all3$Wave) <- c("Time 10", "Time 11","Time 12")
# check N
km_all3
length(unique(km_all3$Id))
# correct
km_all3
t13<-table1::table1(~ Warm.Muslims|Wave * as.factor(Attack), data = km_all3, overall=FALSE)
t13
kable(t13, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all3, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)

km_2018 <- km_all3 %>% 
  dplyr::filter(Wave =="Time 10")%>%
  droplevels()

x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = km_all3, overall = FALSE)
x

t1kable(x, format ="latex")


km_all3$As
# create new data set
library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", 0, 
                      ifelse(Wave=="Time 10" & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", NA, 
                     ifelse(Wave=="Time 10" & Attack == 0, NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id) 



# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf3, overall=F)



## Bind - double dataset to creat missing values
ka3 <- km_all3%>%
  bind_rows(kf3)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka3$Ys)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
t2

t1kable(t2, format ="latex")
# impute missing data
# avoid collineraity

ka2_selected <-ka3%>%
  dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)

ka_amelia<-as.data.frame(ka2_selected) 


### WITHOUT JOINT ASSUMPTION 
# filter only 0s and only 1s for the As
d0 <- ka_amelia %>%
  filter(As==0 )

d18_only0 <- ka_amelia %>%
  filter(As==0 & Wave == "Time 10")


d18_only1 <- ka_amelia %>%
  filter(As==1 & Wave =="Time 10")

# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19_only1 <- ka_amelia %>%
  filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))

d20_only1 <- ka_amelia %>%
  filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))


library(Amelia)

# data needs to be a data frame if passed to Amelia
d0 <-as.data.frame(d0)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 
# assume Y^0|A=2018 = Y^0 2019

imputed_0<- amelia(
  set.seed=1234,
  d0, #dataset to impute
  # cs= c("Id"),
  #ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As", "Id","wave")) 

saveRDS(imputed_0, here::here("_posts","mus","mods", "imputed_0"))


imputed_18_only0<- amelia(
  set.seed=1234,
  d18_only0, #dataset to impute
  # cs= c("Id"),
  #ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As", "Id","wave")) 

saveRDS(imputed_18_only0, here::here("_posts","mus","mods", "imputed_18_only0"))


d18_only1<-data.frame(d18_only1)

## For 2018 v 2020 contrast
imputed_18only1<- amelia(
  set.seed=1234,
  d18_only1, #dataset to impute
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("As", "Wave", "Id","wave")) 

saveRDS(imputed_18only1, here::here("_posts","mus","mods", "imputed_18only1"))


# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
d19_only1<-as.data.frame(d19_only1)


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

imputed_19only1<- amelia(
  d19_only1, #dataset to impute
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("As", "Wave"),
  lags="Ys",
  leads="Ys") 

saveRDS(imputed_19only1, here::here("_posts","mus","mods", "imputed_19only1"))


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

d20_only1<-as.data.frame(d20_only1)
imputed_20only1 <- amelia(
  set.seed=14321,
  d20_only1, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As"),
  lags="Ys",
  leads="Ys")  # correct

saveRDS(imputed_20only1, here::here("_posts","mus","mods", "imputed_20only1"))


## 18 one
m<-10
imputed_18one<-NULL

for(i in 1:m) {
  imputed_18one$imputations$imp[[i]] <- imputed_18only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 10" & As == 1) %>%
    arrange(Wave, Id)
}

## 18 Zeros
m<-10
imputed_18zero<-NULL

for(i in 1:m) {
  imputed_18zero$imputations$imp[[i]] <- imputed_18_only0$imputations[[i]] %>% # diff
    dplyr::filter(Wave == "Time 10" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2019 Y^1s
m<-10
imputed_19one<-NULL

# use 20 wave: King says leads give better estimates (Amelia documentation)
for(i in 1:m) {
  imputed_19one$imputations$imp[[i]] <- imputed_19only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19one$imputations$imp[[1]]

## 19 Zeros
m<-10
imputed_19zero<-NULL

for(i in 1:m) {
  imputed_19zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2020 Y^1s
m<-10
imputed_20one<-NULL

for(i in 1:m) {
  imputed_20one$imputations$imp[[i]] <- imputed_20only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 1) %>%
    arrange(Wave, Id)
}

m<-10
imputed_20zero<-NULL

for(i in 1:m) {
  imputed_20zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 0) %>%
    arrange(Wave, Id)
}



# check

imputed_20one$imputations$imp[[1]]


# combine the data and arrange by wave

m<-10
imps_18 <-NULL
for(i in 1:m) {
  imps_18$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18zero$imputations$imp[[i]], 
                                                   imputed_18one$imputations$imp[[i]])
}

m<-10
imps_19 <-NULL
for(i in 1:m) {
  imps_19$imputations$imp[[i]] <- dplyr::bind_rows(imputed_19zero$imputations$imp[[i]], 
                                                   imputed_19one$imputations$imp[[i]])
}

m<-10
imps_20 <-NULL
for(i in 1:m) {
  imps_20$imputations$imp[[i]] <- dplyr::bind_rows(imputed_20zero$imputations$imp[[i]], 
                                                   imputed_20one$imputations$imp[[i]])
}


## ALL 1s


m<-10
imps_all <-NULL
for(i in 1:m) {
  imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
    imputed_18zero$imputations$imp[[i]],
    imputed_18one$imputations$imp[[i]],
    imputed_19zero$imputations$imp[[i]],
    imputed_19one$imputations$imp[[i]],
    imputed_20zero$imputations$imp[[i]],
    imputed_20one$imputations$imp[[i]]
  ) %>%
    arrange(Wave, Id)
}


# ## ALL 1s
# # m<-10
# # imps_trunc <-NULL
# # for(i in 1:m) {
# #   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
# #     imputed_18zero$imputations$imp[[i]],
# #     imputed_18one$imputations$imp[[i]],
# #    # imputed_19zero$imputations$imp[[i]],
# #     imputed_19one$imputations$imp[[i]],
# #   #  imputed_20zero$imputations$imp[[i]],
# #     imputed_20one$imputations$imp[[i]]
# #   ) %>%
# #     arrange(Wave, Id)
# # }
# 
# saveRDS(imps_trunc, here::here("_posts","mus","mods", "imps_all"))
# 
# # Save data
# #saveRDS(imps_all, here::here("_posts","mus","mods", "imps_all"))
# 
# 
# #mod 2018
# m<-10
# model_18<-NULL
# for(i in 1:m) {
#   model_18[[i]] <- lm(Ys ~ As, data = imps_18$imputations$imp[[i]])
#   }
# 
# pool_parameters(model_18)
# pred_18<- ggeffects::ggemmeans(model_18[[6]], terms = c("As"))
# mus_plot_18 <-plot(pred_18)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_18
# 
# 
# # mod 19
# #mod 2018
# m<-10
# model_19<-NULL
# for(i in 1:m) {
#   model_19[[i]] <- lm(Ys ~ As, data = imps_19$imputations$imp[[i]])
#   }
# 
# pool_parameters(model_19)
# pred_19<- ggeffects::ggemmeans(model_19[[6]], terms = c("As"))
# mus_plot_19 <-plot(pred_19)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_19
# 
# # mod 20
# m<-10
# model_20<-NULL
# for(i in 1:m) {
#   model_20[[i]] <- lm(Ys ~ As, data = imps_20$imputations$imp[[i]])
#   }
# 
# 
# pool_parameters(model_18)
# pool_parameters(model_19)
# pool_parameters(model_20)
# 
# 
# model_20<- ggeffects::ggemmeans(model_20[[6]], terms = c("As"))
# mus_plot_20 <-plot(model_20)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot_18 + mus_plot_19 + mus_plot_20


# all one


imps_all$imputations$imp[[1]]$Wave
# #for brms model
dat_l<-imps_all$imputations$imp
#saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# aveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))


#USE
m<-10
model_all<-NULL
for(i in 1:m) {
  model_all[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_all$imputations$imp[[i]])
}



# USE
tab<-pool_parameters(model_all)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

pl_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) 


mus_plot_model_all <-plot(pl_model_all)+ scale_y_continuous(limits=c(4.10,4.5))+labs(subtitle="Effect of attack on acceptance of Muslims") +  coord_flip() 
mus_plot_model_all




## Another approach a little better for writing code, identical results
library(merTools)
#imps_all<- readRDS(here::here("_posts", "mus", "mods","imps_all"))

modList <- lmerModList(Ys ~  Wave*As + (1|Id), data =  imps_all$imputations$imp)
fixef(modList) # model with dropped missing

modelFixedEff(modList)[,c(1:5)]%>%
  kbl("latex",booktabs = TRUE,digits=3)




estimate_means(
  model_all[[1]],
  contrast = "As",
  at = c("As","Wave"),
  # fixed = NULL,
  # transform = "none",
  ci = 0.95,
  adjust = "holm",
  length = 2
)

estimate_contrasts( model_all[[1]],
                    contrast = "As",
                    at = c("Wave","As") )%>%
  kbl("latex",booktabs = TRUE,digits=2)





mus_plot_18 + mus_plot_19 + mus_plot_20 



# str(dat4)
# str(dat3)
#saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # saveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
#  dat_l <- readRDS(here::here("_posts", "mus", "mods", "dat_l"))
# 
# tscsPlot(imputed_20only1, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
#   obscol = "black")
# 

## 2018-2020 contrast
# m<-10
# imps_base2020 <-NULL
# 
# for(i in 1:m) {
#   imps_base2020$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18b$imputations[[i]], 
#                                       imputed_18_3$imputations[[i]],
#                                       imputed_20s_3$imputations$imp[[i]]%>%
#                                        droplevels())
#   imps_base2020$imputations$imp[[i]] <- imps_base2020$imputations$imp[[i]] %>% arrange(Wave,Id)
# }
# 
# m<-10
# imps_d <-NULL
# dat_d <- NULL
# 
# for(i in 1:m) {
#   imps_d$imputations[[i]]<- as.data.frame(imps_base2020$imputations$imp[[i]])
# }
# 
# dat_d<-imps_d$imputations
# m<-10
# models_d<-NULL
# for(i in 1:m) {
#   models_d[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=imps_d$imputations[[i]])
# }
# 
# pool_parameters(models_d)
# pred__base20202<- ggeffects::ggemmeans(models_d[[6]], terms = c("Wave","As"))
# plot(pred__base20202)
# mus_plot_base20202 <-plot(pred__base20202)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# 
# mus_plot
```

## Bayesian model 3 x waves

```{r}
## Do not save residuals
#   save_pars=save_pars(group=FALSE))
dat_l<- readRDS(here::here("_posts","mus","mods","dat_l"))
# 
# m_b <- brms::brm( 
#   Ys ~ As *  Wave + (1|Id), 
#   data 
#   = dat_l, 
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_b"))
# 
# lazerhawk::brms_SummaryTable(m_b)
# prior_summary(m_b)



# Print Table
# ouput <- tidy(m_b)
# str(ouput)
# ouput
# #output<- as.data.frame(ouput)
# 
# test <-tidybayes::summaries_draws.grouped_df(m_b)
# 
# library(lazerhawk)
# blh_tab <- lazerhawk::brms_SummaryTable(m_b, panderize=F)
# blh_tab
# blh_tab %>%
#    kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
# print()


pred3<- ggeffects::ggemmeans(m_b, terms = c("Wave","As"))
mus_plot3 <-plot(pred3)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

p <-mcmc_intervals(m_b)
ps<- brms::posterior_summary(m_b) 

```

### lognormal prior


```{r}
# m_lgn <- brms::brm( 
#   bf(Ys ~ As  *  Wave + (1|Id)),#, 
#   # sigma ~ As + Wave + (1|Id)),
#   data = dat_l,
#   prior = c(prior(lognormal(0, 1), class = b, coef = "WaveWave11"),
#             prior(lognormal(0, 1), class = b, coef = "WaveWave12")), 
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_lgn"))
# 
# lazerhawk::brms_SummaryTable(m_lgn, panderize=F)



# USE?

m_prior <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = dat_l,
  prior = c(prior(normal(0, .25), class = b, coef = "WaveTime11"),
            prior(normal(0, .25), class = b, coef = "WaveTime12")), 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_prior"))

blh_tab_nat <- lazerhawk::brms_SummaryTable(m_prior, panderize=F)
blh_tab_nat %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
  print()

# Same


plot(m_prior)


# 

# m_prior_t <- brms::brm( 
#   bf(Ys ~ 0 + As:Wave + (1|Id)),#, 
#   # sigma ~ As + Wave + (1|Id)),
#   data = dat_l,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_prior_t"))
# 
# lazerhawk::brms_SummaryTable(m_prior_t, panderize=F)
# plot(m_prior_t)

#  will not work 
# m_prior_c <- brms::brm( 
#   bf(Ys ~ As + Wave + (1|Id)),#, 
#   # sigma ~ As + Wave + (1|Id)),
#   prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
#             set_prior("constant(0)", class = "b", coef = "WaveTime12")),
#   data = dat_l,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_prior_c"))

lazerhawk::brms_SummaryTable(m_prior_c, panderize=F)
plot(m_prior_c)


m_prior_s <- brms::brm(
  bf(Ys ~ As  *  Wave + (1|Id)),#,
  # sigma ~ As + Wave + (1|Id)),
  data = dat_l,
  prior = c(set_prior("constant(0)", class = "b", coef = "WaveWave11"),
            set_prior("constant(0)", class = "b", coef = "WaveWave12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_prior_s"))

# # Same
lazerhawk::brms_SummaryTable(m_prior_s, panderize=F)

plot(m_prior_s)
plot


# 
# m_use <- brms::brm(
#   bf(Ys ~  Wave  +  As:Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(
#     prior(normal(0, .25), class = b, coef = "WaveTime11"),
#     prior(normal(0, .25), class = b, coef = "WaveTime12")
#   ),
#   # prior = c(set_prior("constant(0)", class = "b", coef = "WaveWave11"),
#   #          set_prior("constant(0)", class = "b", coef = "WaveWave12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use")
#   )

lazerhawk::brms_SummaryTable(m_use, panderize=F)


m_use_pr <- brms::brm(
  bf(Ys ~  Wave  +  As:Wave + (1 | Id)),
  data = dat_l,
  prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
            set_prior("constant(0)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_use_pr")
)

lazerhawk::brms_SummaryTable(m_use_pr, panderize=F)

plot(m_use_pr)


## interaction
.06
m_use_pr_int <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = dat_l,
  prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
            set_prior("constant(0)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_use_pr_int")
)

lazerhawk::brms_SummaryTable(m_use_pr_int, panderize=F)

plot(m_use_pr_int)





# m_use_pr_sens <- brms::brm(
#   bf(Ys ~  As * Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
#            set_prior("constant(0.06)", class = "b", coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use_pr_sens")
#   )

# sense 2
m_use_pr_sens2 <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = dat_l,
  prior = c(set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
            set_prior("constant(0.12)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_use_pr_sens2")
)

lazerhawk::brms_SummaryTable(m_use_pr_sens2, panderize=F)

plot(m_use_pr_int)
prior_summary(m_use_pr_int)

m_use_pr_sens3 <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = dat_l,
  prior = c(
    set_prior("constant(4.20)", class = "Intercept"),
    #set_prior("constant(1.47)", class = "sd", coef = "Intercept"),
    set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
    set_prior("constant(0.12)", class = "b", coef = "WaveTime12")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_use_pr_sens3")
)

lazerhawk::brms_SummaryTable(m_use_pr_sens3, panderize=F)

plot(m_use_pr_int)

## Sensitivity analysis


```

Marginal means

### Graphs 

```{r}
#### GRAPH
library(tidybayes)
library(emmeans)
marg_eff_attack_0 <- m_use_pr_int %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)

marg_eff_attack_0
#saveRDS(marg_eff_attack_0,  here::here("_posts", "mus", "mods", "marg_eff_attack_0"))
marg_eff_attack_0 <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0.rds"))

marg_eff_attack_0a <- m_prior %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
#saveRDS(marg_eff_attack_0a,  here::here("_posts", "mus", "mods", "marg_eff_attack_0a"))

# sensitivity
marg_eff_attack_0s <- m_use_pr_sens %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
#saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s"))

marg_eff_attack_0s
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))



marg_eff_attack_0s2 <- m_use_pr_sens2 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s2"))

marg_eff_attack_0s2
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

marg_eff_attack_0s3 <- m_use_pr_sens3 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)

saveRDS(marg_eff_attack_0s3,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s3"))



plot_baseline_unconstr <- ggplot(
  marg_eff_attack_0a,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nModel estimates"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_unconstr

# marg_eff_m_use_pr <- m_use_pr %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# saveRDS(marg_eff_m_use_pr,  here::here("_posts", "mus", "mods", "marg_eff_m_use_pr"))
# 
#  <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

## plot all
# marg_eff_attack_0
plot_baseline_constr <- ggplot(
  marg_eff_attack_0,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: constrain baseline to Time 10"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_constr

# 
# plot_baseline_sens <- ggplot(
#   marg_eff_attack_0s,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) + 
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes: Time 10,11,12.\nSensitivity analysis: strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens


# plot_baseline_sens2 <- ggplot(
#   marg_eff_attack_0s2,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) + 
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens2


plot_baseline_sens3 <- ggplot(
  marg_eff_attack_0s3,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_sens2

# ML Graph 

d_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) + labs(subtitle="maximu")

pl_ml <-plot(d_ml) +  
  scale_y_reverse() +
  coord_flip() +
  scale_y_continuous(limits=c(4,4.6))+
  labs(x = "Predicted Warmth Response",
       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
       title = '',
       fill = "Conterfactual Contrasts",
       subtitle = "Frequentist posterior locations of potential outcomes:\nSensitivity analysis: assume strong time effect") +
  theme_pubclean() + theme(legend.position = "bottom")
pl_ml

plot_baseline_constr
outplot <- (pl_ml + plot_baseline_unconstr ) /
  (plot_baseline_constr + plot_baseline_sens2 )  + 
  plot_annotation(tag_levels = 'a', title = "Estimation of  predicted marginal effects of the attack under different assumptions")
outplot
ggsave(
  outplot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "outplot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)





#marg_eff_attack_0a
# Obtain contrasts
library(tidyverse)
marg_t <- marg_eff_attack_0%>%
  filter((Wave=="Time 10" | Wave == "Time 11" ) )#Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
marg_t


# marg_eff_attack_0
plot_2019 <- ggplot(
  marg_t,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes: 2018 vs 2020"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.7))
plot_2019

ggsave(
  plot_2019,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height = 6,
  units = "in",
  filename = "plot_2019",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


## 2020
marg_t2 <- marg_eff_attack_0%>%
  filter((Wave=="Time 10" | Wave == "Time 12" ) )#Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
marg_t2


# marg_eff_attack_0
plot_2020 <- ggplot(
  marg_t2,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) + 
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes: 2018 vs. 2020"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.7))
plot_2020

ggsave(
  plot_2020,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height = 6,
  units = "in",
  filename = "plot_2020",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


#plot_all

# 
# # 
# grand_mean_ame_a <- m_bayesR %>%
#     emmeans(~ As*Wave,
#           epred = TRUE, re_formula = NA) %>%
#   contrast() %>%
#   gather_emmeans_draws()
# 
# grand_mean_ame_a
# #   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# 
# saveRDS(grand_mean_ame_a,  here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# #grand_mean_ame_a <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# 
# # remove erro contrasts / we don't observe 0 in year two 
# 

# does not

# plot_all__ame <- ggplot(grand_mean_ame_a,
#                                      aes(x = .value)) +
#   stat_halfeye(slab_alpha = 0.75, fill = "grey") +
#   scale_fill_okabe_ito(order = c(3, 4)) +
#   labs(x = "Average marginal effect of attack on warmth to Muslims",
#        y = "Density") +
#   #facet_wrap(vars(region)) +
#   theme_classic() + 
#   theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
# plot_all__ame 
# Try
library(patchwork)
bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
bayes_plots_a

# ggsave(
#   bayes_plots_a,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "bayes_plots_a.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )

````


### Impute all waves of MUS PRED

```{r}

table1::table1(~ Partner|Wave, data = df)%>%
  kbl()%>%
  kable_paper("hover", full_width = F)

all_d <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    Partner,
    Parent,
    EthnicCats,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    GenCohort,
    Urban,
    TSCORE,
    EthnicCats,
    Employed,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    Muslim,
    TSCORE,
    WSCORE,
    YearMeasured,
    Religious
  ) %>%
  dplyr::filter(TSCORE < 3545) %>%
  dplyr::filter(
    Wave == 2012 &
      YearMeasured == 1 |
      Wave == 2013 &
      YearMeasured == 1 |
      Wave == 2014 &
      YearMeasured == 1 |
      Wave == 2016 &
      YearMeasured == 1 |
      Wave == 2017 &
      YearMeasured == 1 |
      Wave == 2018 &
      YearMeasured == 1 
  ) %>% 
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  droplevels() %>%
  arrange(Wave,Id) %>%
  # group_by(Id) %>%
  # dplyr::group_by(Id) %>% filter(n() > 6) %>%
  # dplyr::add_tally() %>%
  # filter(n == 7) %>%
  # arrange(Wave,Id) %>%
  dplyr::mutate(Warm.Muslims_lag = lag(Warm.Muslims))%>%
  droplevels() %>%
  ungroup() %>%
  # dplyr::mutate(Attack = ifelse(TSCORE >= 3545, 1, 0)) %>%
  # dplyr::group_by(Id) %>% filter(n() > 5) %>% 
  # ungroup()%>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(yrs =  (dys/365))%>% 
  arrange(Id,Wave)%>%
  #   dplyr::filter(Wave == 2012 | Wave== 2013 | Wave == 2014 | Wave == 2015| Wave == 2016 | Wave ==2017 | Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  #   droplevels() %>%
  #   dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  #   group_by(Id) %>%
  #   dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  #   filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  #     ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2012", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2012", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2012", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2012", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2012", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2012", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2012", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2012", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2012", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2012", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 
length(unique(all_d$Id))


#levels(all_d$Wave) <- c("Time4", "Time5", "Time6","Time7", "Time8","Time9", "Time10")


t13<-table1::table1(~ Warm.Muslims|Wave, data = all_d, overall=FALSE)
kable(t13, format ="markdown", booktabs = TRUE)


# Amelia will center and scale continuous variables during the MI process. 

library(Amelia)

# data needs to be a data frame if passed to Amelia
all_da <-all_d%>%
  dplyr::select(Id,Wave,wave,TSCORE,Warm.Muslims_lag,Warm.Muslims,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b,yrs,TSCORE)
all_da <-as.data.frame(all_da)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 

# assume Y^0|A=2018 = Y^0 2019
all_da<- as.data.frame(all_da)
imputed_m <- amelia(
  set.seed = 1234,
  all_da,
  #dataset to impute
  cs = c("Id"),
  ts = c("yrs"),
  m = 10,
  # number of imputations
  lags="Warm.Muslims",
  leads="Warm.Muslims",
  noms = c("EthnicCats_b"),
  idvars = c("Wave", "TSCORE","wave")
)


### IPW

library(ipw)
# 
# 
# w_ipw <- ipwtm(
#   exposure = yrs,
#   family = "gaussian",
#   # Time invariant 
#   numerator =  ~ Male + GenCohort + Edu + EthnicCats,
#   # Time invariant and non-invariant confounders
#   denominator = ~ Male + GenCohort + Edu + EthnicCats + Religious + Pol.Orient + Urban+NZdep,
#   id = Id,
#   timevar = Wave,
#   corstr = "exchangeable",
#   type = "all",
#   data = as.data.frame(km2)
# )
library(splines)
m<-10
model_m<-NULL
for(i in 1:m) {
  model_m[[i]] <- lmer(Warm.Muslims ~ bs(yrs)  + (1|Id), data = imputed_m$imputations[[i]])
}



# USE
tab<-pool_parameters(model_m)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

pa <- ggeffects::ggemmeans(model_m[[3]], terms = c("yrs[all]")) 


mus_plot_model_all <-plot(pa)+labs(title="Growth in Muslim Acceptance") +  scale_y_continuous(limits=c(3,5))#coord_flip() 
mus_plot_model_all 

m00 <- lmer(Warm.Muslims ~   yrs + Warm.Muslims_lag +   (1|Id), weights=ipw,
            data = km2)



# dplyr::select(Id,Wave,wave,Attack,Warm.Muslims,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b,yrs)

# #check no weights greater than 10
# max(w_ipw$ipw.weights) # max is 1.189481
# 
# ## add weights to data frame. 
# km$Pol.Orient
# library(tidyverse)
# km2 <- km2 %>% 
#   mutate(ipw = w_ipw$ipw.weights)








# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019


imputed_18_3<- amelia(
  d18, #dataset to impute
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As","Id","wave")) #, # do 


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

d19<-as.data.frame(d19)
imputed_19_3<- amelia(
  set.seed=14321,
  d19, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) # correct

imputed_20_3 <- amelia(
  set.seed=14321,
  d19, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) # correct


# Here filter only the 2019 Y^1s

m<-10
imputed_19s_3<-NULL

for(i in 1:m) {
  imputed_19s_3$imputations$imp[[i]] <- imputed_19_3$imputations[[i]] %>%
    dplyr::filter(Wave == 2019 & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19s_3$imputations$imp[[1]]

m<-10
imputed_20s_3<-NULL

for(i in 1:m) {
  imputed_20s_3$imputations$imp[[i]] <- imputed_20_3$imputations[[i]] %>%
    dplyr::filter(Wave == 2020 & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19s_3$imputations$imp[[1]]
imputed_20s_3$imputations$imp[[1]]


m<-10
imps3 <-NULL
for(i in 1:m) {
  imps3$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0_3$imputations[[i]], 
                                                 imputed_18_3$imputations[[i]],
                                                 imputed_19s_3$imputations$imp[[i]],
                                                 imputed_20s_3$imputations$imp[[i]])
  imps3$imputations$imp[[i]] <- imps3$imputations$imp[[i]] %>% arrange(Wave,Id)
}

# 
# m<-20
# imps<-NULL
# for(i in 1:m) {
#   imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]], 
#                                      imputed_1$imputations[[i]])
#   imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
#   
# }

table1::table1(~ Ys |Wave*As, data = km_all)

m<-10
impsL3 <-NULL
dat3 <- NULL

table1::table1(~ Warm.Muslims |Wave, data = df)

for(i in 1:m) {
  impsL3$imputations[[i]]<- as.data.frame(imps3$imputations$imp[[i]])
}

# dat3 <- impsL3$imputations
# saveRDS(dat3, here::here("_posts", "mus", "mods", "dat3"))
# saveRDS(impsL3, here::here("_posts", "mus", "mods", "impsL3"))
# saveRDS(imps3, here::here("_posts", "mus", "mods", "imps3"))
# impsL3 <- readRDS(here::here("_posts", "mus", "mods", "impsL3"))

tscsPlot(imputed_19_3, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
         obscol = "black")

m<-10
models<-NULL
for(i in 1:m) {
  models[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=impsL3$imputations[[i]])
}

pool_parameters(models)
pred<- ggeffects::ggemmeans(models[[2]], terms = c("Wave","As"))
mus_plot <-plot(pred)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

mus_plot

df1<- df%>%
  filter(YearMeasured==1)
df$KESSLER6
table1::table1(~ Religion.Church|Wave, data = df1, transpose=T)
```






## EStimate the variances as well as the mean

We can fit a distributional model



```{r}
sigma ~ group

m_bayes3waveSigma <- brms::brm( 
  bf(Ys ~ As *  Wave + (1|Id), 
     sigma ~ As * Wave,  
     set_rescor(rescor = FALSE)),
  data = dat3, 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_bayes3waveSigma"))

tab<-(m_bayes3waveSigma)

blh_tab <- lazerhawk::brms_SummaryTable(m_bayes3waveSigma, panderize=F)
blh_tab
blh_tab %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
  print()

m_bayes3waveSigma

tab <-parameters::model_parameters(m_bayes3waveSigma, test = c("pd"))
tab2<- sjstats::tidy_stan(m_bayes3waveSigma)

tab2<- modelsummary::modelsummary(m_bayes3waveSigma, metrics="SIGMA")


m_bayes3waveSigma %>%
  spread_draws(r_condition[condition,]) %>%
  compare_levels(r_condition, by = condition) %>%
  ungroup() %>%
  mutate(condition = reorder(condition, r_condition)) %>%
  ggplot(aes(y = condition, x = r_condition)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") 
```




## negative control

```{r}
km_ov <- df %>%
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
    # Warm.Elderly,
    # Warm.MentalIllness,
    # Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Overweight, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  select(Id, Wave, Ys, As, Warm.Overweight, Attack, Age, EthnicCats, Edu, Employed, Urban, Male, GenCohort, Religious, Pol.Orient, Parent, Partner, Urban, NZdep,wave) %>%
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  arrange(Id,Wave) 

# check N
length(unique(km_ov$Id))
# correct

t13<-table1::table1(~ Warm.Overweight|Wave*as.factor(Attack), data = km_ov, overall=FALSE)
t13

kable(t13, format ="latex", booktabs = TRUE)

table1::table1(~Warm.Overweight|Wave*as.factor(Attack), data = km_all3, overall=FALSE)

obtl<-table1::table1(~ Warm.Overweight|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)


x <- table1::table1(~ Warm.Overweight + Age + Male + GenCohort + Edu + Employed + EthnicCats  + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave*Attack, data = km_all3, overall=FALSE)

t1kable(x, format ="latex")



# create new data set
library(tidyverse)
kfo  <- km_ov %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave==2018 & Attack == 1|Wave == 2019|Wave ==2020, 0, 
                      ifelse(Wave==2018 & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave==2018 & Attack == 1|Wave ==2019|Wave ==2020, NA, 
                     ifelse(Wave==2018 & Attack == 0, NA, Warm.Overweight)))%>%
  ungroup() %>%
  arrange(Wave,Id) 



# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kfo, overall=F)



## Bind - double dataset to creat missing values
koo <- km_ov%>%
  bind_rows(kfo)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) %>%
  select(-c(Warm.Overweight,Attack))

head(koo)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = koo, overall=F)
t2
# impute missing data
# avoid collineraity
ko_selected <-koo%>%
  select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)

ko_amelia<-as.data.frame(ko_selected) 


### WITHOUT JOINT ASSUMPTION 
# filter only 0s and only 1s for the As
d0o <- ko_amelia %>%
  filter(As==0 )

d18o <- ko_amelia %>%
  filter(As==1 & Wave ==2018)

# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19o <- ko_amelia 

d20o <- ko_amelia



#ka3$Wave<-as.numeric(ka3$Wave)

# Amelia will center and scale continuous variables during the MI process. 

library(Amelia)

# data needs to be a data frame if passed to Amelia
d0o <-as.data.frame(d0o)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 

# assume Y^0|A=2018 = Y^0 2019

imputed_0_3o<- amelia(
  set.seed=1234,
  d0o, #dataset to impute
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) 



# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
d18o<-as.data.frame(d18o)

imputed_18_3o<- amelia(
  d18o, #dataset to impute
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As","Id","wave")) #, # do 


# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

d19o<-as.data.frame(d19o)
imputed_19_3o<- amelia(
  set.seed=14321,
  d19o, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) # correct

imputed_20_3o <- amelia(
  set.seed=14321,
  d19o, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  idvars=c("Wave","As")) # correct


# Here filter only the 2019 Y^1s

m<-10
imputed_19s_3o<-NULL

for(i in 1:m) {
  imputed_19s_3o$imputations$imp[[i]] <- imputed_19_3o$imputations[[i]] %>%
    dplyr::filter(Wave == 2019 & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19s_3o$imputations$imp[[1]]

m<-10
imputed_20s_3o<-NULL

for(i in 1:m) {
  imputed_20s_3o$imputations$imp[[i]] <- imputed_20_3o$imputations[[i]] %>%
    dplyr::filter(Wave == 2020 & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19s_3o$imputations$imp[[1]]
imputed_20s_3o$imputations$imp[[1]]


m<-10
imps3o <-NULL
for(i in 1:m) {
  imps3o$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0_3o$imputations[[i]], 
                                                  imputed_18_3o$imputations[[i]],
                                                  imputed_19s_3o$imputations$imp[[i]],
                                                  imputed_20s_3o$imputations$imp[[i]])
  imps3o$imputations$imp[[i]] <- imps3o$imputations$imp[[i]] %>% arrange(Wave,Id)
}

# 
# m<-20
# imps<-NULL
# for(i in 1:m) {
#   imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]], 
#                                      imputed_1$imputations[[i]])
#   imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
#   
# }


```


### TEST Ordinal


```{r}
km_all3 <- df %>%
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
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019 
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  # dplyr::filter(Attack == 0) %>%
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>% 
  dplyr::mutate(Ys = Warm.Muslims, 
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>% 
  dplyr::mutate(wave = as.numeric(Wave)-1) %>% 
  group_by(Id)%>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>% 
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup()%>%
  rename(Wave = )
arrange(Id,Wave) 
levels(km_all3$Wave) <- c("Time 10", "Time 11","Time 12")
# check N
length(unique(km_all3$Id))
# correct
t13<-table1::table1(~ Warm.Muslims|Wave * as.factor(Attack), data = km_all3, overall=FALSE)
t13
kable(t13, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all3, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)

km_2018 <- km_all3 %>% 
  dplyr::filter(Wave =="Time 10")%>%
  droplevels()

x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = km_all3, overall = FALSE)
x

t1kable(x, format ="latex")


km_all3$As
# create new data set
library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", 0, 
                      ifelse(Wave=="Time 10" & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", NA, 
                     ifelse(Wave=="Time 10" & Attack == 0, NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id) 



# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf3, overall=F)



## Bind - double dataset to creat missing values
ka3 <- km_all3%>%
  bind_rows(kf3)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka3$Ys)
# Check 

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
t2

t1kable(t2, format ="latex")
# impute missing data
# avoid collineraity

ka2_selected <-ka3%>%
  dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)

ka_amelia<-as.data.frame(ka2_selected) 


### WITHOUT JOINT ASSUMPTION 
# filter only 0s and only 1s for the As
d0 <- ka_amelia %>%
  filter(As==0 )

ka_amelia$Time
d18_only1 <- ka_amelia %>%
  dplyr::filter(As==1 & Wave =="Time 10")

nrow(d18_only1)


# need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
d19_only1 <- ka_amelia %>%
  filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))

d20_only1 <- ka_amelia %>%
  filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))


library(Amelia)

# data needs to be a data frame if passed to Amelia
d0 <-as.data.frame(d0)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution. 
# assume Y^0|A=2018 = Y^0 2019

imputed_0o<- amelia(
  set.seed=1234,
  d0, #dataset to impute
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  ors = c("Ys"),
  idvars=c("Wave","As", "Id","wave")) 

saveRDS(imputed_0o, here::here("_posts","mus","mods", "imputed_0o"))

d18_only1<-data.frame(d18_only1)

## For 2018 v 2020 contrast
imputed_18only1o<- amelia(
  set.seed=1234,
  d18_only1, #dataset to impute
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  ors = c("Ys"),
  idvars=c("Wave","As", "Id","wave")) 


saveRDS(imputed_18only1o, here::here("_posts","mus","mods", "imputed_18only1o"))


# we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
d19_only1<-as.data.frame(d19_only1)

str(d19_only1)
# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

imputed_19only1o<- amelia(
  d19_only1, #dataset to impute
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  noms = c(
    "EthnicCats_b"
  ),
  ors = c("Ys"),
  idvars=c("As", "Wave"),
  lags="Ys",
  leads="Ys",
  logs= "Ys") 



imputed_19only1o<-transform(imputed_19only1o, Ys = exp(Ys))

saveRDS(imputed_19only1o, here::here("_posts","mus","mods", "imputed_19only1o"))
```

# This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.  
# Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later. 

```{r}
d20_only1o<-as.data.frame(d20_only1)
imputed_20only1o <- amelia(
  set.seed=14321,
  d20_only1, #dataset to impute
  m = 10, # number of imputations
  cs= c("Id"),
  ts= c("wave"),
  noms = c(
    "EthnicCats_b"
  ),
  bound = rbind(c(1, 0, Inf))),
idvars=c("Wave","As"),
lags="Ys",
leads="Ys")  # correct

saveRDS(imputed_20only1o, here::here("_posts","mus","mods", "imputed_20only1o"))


## 18 one
m<-10
imputed_18one<-NULL

for(i in 1:m) {
  imputed_18one$imputations$imp[[i]] <- imputed_18only1o$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 10" & As == 1) %>%
    arrange(Wave, Id)
}

## 18 Zeros
m<-10
imputed_18zero<-NULL

for(i in 1:m) {
  imputed_18zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 10" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2019 Y^1s
m<-10
imputed_19one<-NULL

# use 20 wave: King says leads give better estimates (Amelia documentation)
for(i in 1:m) {
  imputed_19one$imputations$imp[[i]] <- imputed_19only1o$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 1) %>%
    arrange(Wave, Id)
}

# check

imputed_19one$imputations$imp[[1]]

## 19 Zeros
m<-10
imputed_19zero<-NULL

for(i in 1:m) {
  imputed_19zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 11" & As == 0) %>%
    arrange(Wave, Id)
}



# Here filter only the 2020 Y^1s
m<-10
imputed_20one<-NULL

for(i in 1:m) {
  imputed_20one$imputations$imp[[i]] <- imputed_20only1$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 1) %>%
    arrange(Wave, Id)
}

m<-10
imputed_20zero<-NULL

for(i in 1:m) {
  imputed_20zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
    dplyr::filter(Wave == "Time 12" & As == 0) %>%
    arrange(Wave, Id)
}



# check

imputed_20one$imputations$imp[[1]]


# combine the data and arrange by wave

m<-10
imps_18 <-NULL
for(i in 1:m) {
  imps_18$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18zero$imputations$imp[[i]], 
                                                   imputed_18one$imputations$imp[[i]])
}

m<-10
imps_19 <-NULL
for(i in 1:m) {
  imps_19$imputations$imp[[i]] <- dplyr::bind_rows(imputed_19zero$imputations$imp[[i]], 
                                                   imputed_19one$imputations$imp[[i]])
}

m<-10
imps_20 <-NULL
for(i in 1:m) {
  imps_20$imputations$imp[[i]] <- dplyr::bind_rows(imputed_20zero$imputations$imp[[i]], 
                                                   imputed_20one$imputations$imp[[i]])
}


## ALL 1s
m<-10
imps_all <-NULL
for(i in 1:m) {
  imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
    imputed_18zero$imputations$imp[[i]],
    imputed_18one$imputations$imp[[i]],
    imputed_19zero$imputations$imp[[i]],
    imputed_19one$imputations$imp[[i]],
    imputed_20zero$imputations$imp[[i]],
    imputed_20one$imputations$imp[[i]]
  ) %>%
    arrange(Wave, Id)
}

# Save data
#saveRDS(imps_all, here::here("_posts","mus","mods", "imps_all"))


#mod 2018
m<-10
model_18<-NULL
for(i in 1:m) {
  model_18[[i]] <- lm(Ys ~ As, data = imps_18$imputations$imp[[i]])
}

pool_parameters(model_18)
pred_18<- ggeffects::ggemmeans(model_18[[6]], terms = c("As"))
mus_plot_18 <-plot(pred_18)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

mus_plot_18


# mod 19
#mod 2018
m<-10
model_19<-NULL
for(i in 1:m) {
  model_19[[i]] <- lm(Ys ~ As, data = imps_19$imputations$imp[[i]])
}

pool_parameters(model_19)
pred_19<- ggeffects::ggemmeans(model_19[[6]], terms = c("As"))
mus_plot_19 <-plot(pred_19)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

mus_plot_19

# mod 20
m<-10
model_20<-NULL
for(i in 1:m) {
  model_20[[i]] <- lm(Ys ~ As, data = imps_20$imputations$imp[[i]])
}


pool_parameters(model_18)
pool_parameters(model_19)
pool_parameters(model_20)


model_20<- ggeffects::ggemmeans(model_20[[6]], terms = c("As"))
mus_plot_20 <-plot(model_20)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

mus_plot_18 + mus_plot_19 + mus_plot_20


# all one

m<-10
model_all<-NULL
for(i in 1:m) {
  model_all[[i]] <- lmer(Ys ~  Wave * As + (1|Id), data = imps_all$imputations$imp[[i]])
}


# m<-10
# model_allS<-NULL
# for(i in 1:m) {
#   model_allS[[i]] <- lmer(scale(Ys) ~  Wave * As + (1|Id), data = imps_all$imputations$imp[[i]])
#   }
# 
# pool_parameters(model_all, digits = 3) 
# pool_parameters(model_allS, digits = 3) 
# 
# # library Evalues for sensitivity analysis
# library(EValue)
# 
# # Evalue
# evalues.OLS(est=.17, se = .01, sd = 1, delta = 1)
# 
# # > evalues.OLS(est=.17, se = .01, sd = 1, delta = 1)
# #             point    lower    upper
# # RR       1.167308 1.146713 1.188272
# # E-values 1.609235 1.556882       NA
# 
# #2019 AS 1
# # > evalues.OLS(est=.14, se = .01, sd = 1, delta = 1)
# #             point    lower    upper
# # RR       1.135871 1.115832 1.156271
# # E-values 1.528723 1.475343       NA


## Another approach a little better for writing code, identical results
library(merTools)
modList <- lmerModList(Ys ~  Wave * As + (1|Id), data =  imps_all$imputations$imp)
fixef(modList) # model with dropped missing

modelFixedEff(modList)[,c(1:3)]%>%
  kbl("latex",booktabs = TRUE,digits=3)


tab<-pool_parameters(modList)[,1:6]
tab
plot(tab, show_labels = TRUE)

pl_model_all<- ggeffects::ggemmeans(model_all[[6]], terms = c("Wave","As"))
mus_plot_model_all <-plot(pl_model_all)+ scale_y_continuous(limits=c(4.10,4.5))+labs(title="Effect of attack on acceptance of Muslims")

visualisation_matrix(model_all)

estimate_means(
  model_all[[1]],
  contrast = "As",
  at = c("As","Wave"),
  # fixed = NULL,
  # transform = "none",
  ci = 0.95,
  adjust = "holm",
  length = 2
)

estimate_contrasts( model_all[[1]],
                    contrast = "As",
                    at = c("Wave","As") )
%>%
  kbl("latex",booktabs = TRUE,digits=2)





mus_plot_18 + mus_plot_19 + mus_plot_20 

# #for brms model
dat_l<-imps_all$imputations$imp
# str(dat4)
# str(dat3)
saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # saveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
#  dat_l <- readRDS(here::here("_posts", "mus", "mods", "dat_l"))
# 
# tscsPlot(imputed_20only1, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
#   obscol = "black")
# 

## 2018-2020 contrast
# m<-10
# imps_base2020 <-NULL
# 
# for(i in 1:m) {
#   imps_base2020$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18b$imputations[[i]], 
#                                       imputed_18_3$imputations[[i]],
#                                       imputed_20s_3$imputations$imp[[i]]%>%
#                                        droplevels())
#   imps_base2020$imputations$imp[[i]] <- imps_base2020$imputations$imp[[i]] %>% arrange(Wave,Id)
# }
# 
# m<-10
# imps_d <-NULL
# dat_d <- NULL
# 
# for(i in 1:m) {
#   imps_d$imputations[[i]]<- as.data.frame(imps_base2020$imputations$imp[[i]])
# }
# 
# dat_d<-imps_d$imputations
# m<-10
# models_d<-NULL
# for(i in 1:m) {
#   models_d[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=imps_d$imputations[[i]])
# }
# pool_parameters(models_d)
# pred__base20202<- ggeffects::ggemmeans(models_d[[6]], terms = c("Wave","As"))
# plot(pred__base20202)
# mus_plot_base20202 <-plot(pred__base20202)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

mus_plot
```

## Bayesian model 3 x waves

```{r}
## Do not save residuals
#   save_pars=save_pars(group=FALSE))
dat_l<- readRDS(here::here("_posts","mus","mods","dat_l"))
rm(m_b)
m_b <- brms::brm( 
  Ys ~ As *  Wave + (1|Id), 
  data 
  = dat_l, 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_b"))

lazerhawk::brms_SummaryTable(m_b)
prior_summary(m_b)



# Print Table
ouput <- tidy(m_b)
str(ouput)
ouput
#output<- as.data.frame(ouput)

test <-tidybayes::summaries_draws.grouped_df(m_b)

library(lazerhawk)
blh_tab <- lazerhawk::brms_SummaryTable(m_b, panderize=F)
blh_tab
blh_tab %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
  print()


pred3<- ggeffects::ggemmeans(m_b, terms = c("Wave","As"))
mus_plot3 <-plot(pred3)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")

p <-mcmc_intervals(m_b)
ps<- brms::posterior_summary(m_b) 

```

### lognormal prior


```{r}

m_lgn <- brms::brm( 
  bf(Ys ~ As  *  Wave + (1|Id)),#, 
  # sigma ~ As + Wave + (1|Id)),
  data = dat_l,
  prior = c(prior(lognormal(0, 0.25), class = b, coef = "WaveWave11"),
            prior(lognormal(0, 0.25), class = b, coef = "WaveWave12")), 
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "m_lgn"))
```

Marginal means

### Graphs 



