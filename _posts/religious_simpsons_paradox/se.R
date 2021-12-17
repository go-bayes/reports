
# standardise 
summary(df$Cohort)
dat2 <- df %>%
  dplyr::filter(YearMeasured == 1 )%>% # filtering only people who have responded in a year
  dplyr::select(Wave, Age, Wave, Religious1 ,Id, Relid, years, Cohort.WideBand,GenCohort, Cohort)%>%
  dplyr::mutate(yearW  = as.numeric(Wave)-1) %>%
  # filter(!is.na(Religious1))%>%
  group_by(Id) %>% filter(n() > 1)%>% # have responded to at least 2 x waves
  filter(n() !=0)%>%
  ungroup(Id)%>%
  group_by(Wave) %>%
  filter(!is.na(Religious1)) %>%
  filter(!is.na(GenCohort)) %>%
  ungroup()%>%
  mutate(age2009 = 2019 - Cohort) %>%
  mutate(Cohort_WideBand = as.numeric(Cohort.WideBand)) %>%
  arrange(Id,yearW)

relwidelong<-  df %>%
  dplyr::mutate(Religious = Religious1-1)%>%
  dplyr::select(Id,Religious,Wave)


GenCohortwidelong<-  df %>%
  dplyr::select(Id,GenCohort,Wave)
##THIS HANGS
#out <- with(relwidelong, long2matrices(id = Id, X = Age, Y = Religious))

relwide<- spread(relwidelong,Wave,Religious)
relwide
genwide<- spread(GenCohortwidelong,Wave,GenCohort)

Genwide = as.data.frame(genwide$`2019`)
head(Genwide)

colnames(Genwide)<-"GenCohort"

str(Genwide$GenCohort)




m3b  <- msm(
  Religious1 ~ yearW,
  Id,
  data = dat2,
  covariates = ~ Cohort_WideBand,
  est.initprobs = TRUE,
  exacttimes = TRUE,# Not running without this assumption.
  # gen.inits = TRUE,
  qmatrix = q3,
  ematrix = rbind(c(.1, .1, .1),
                  c(.1, .1, .1),
                  c(.1, .1, .1))
)

# saveRDS(m3b, here::here("models","simpsons-paradox-religion-three-state-cohortwide"))


# options(scipen = 100)
m3b
covariates=list(sex=0))

pmatrix.msm(m3b)
pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 1))
pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 5))

m1
str(m3b)

m3b$hmodel$initprobs  


# 
# round( pmatrix.msm(m3b, covariates = list(Age = 20)), 2)
# rouncd( pmatrix.msm(m3b, covariates = list(Age = 40)), 2)
# 
# pmatrix.msm(m3, covariates = "mean" )
# 
# 
# 
saveRDS(m3b, here::here("models","simpsons-paradox-religion-three-state-cohortwide"))


# Graph
hoz_70<- as.matrix( msm::pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 8)))
hoz_70 <- round(hoz_70, digits = 2)
hoz_70

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_70) <- stateNames; colnames(hoz_70) <- stateNames
hoz_70
## need to transpose or else arrows go in the wrong direction
hoz_70 <- t(hoz_70) 


plotmat(hoz_70,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Age = 70")

```


```{r}
# Graph
table(dat2$Cohort_WideBand)
hoz_20<- as.matrix( msm::pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 3)))
hoz_20 <- round(hoz_20, digits = 2)
hoz_20

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_20) <- stateNames; colnames(hoz_20) <- stateNames
hoz_20
## need to transpose or else arrows go in the wrong direction
hoz_20 <- t(hoz_20) 


plotmat(hoz_20,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Cohort = 3")

```


```{r}
# Graph
table(dat2$Cohort_WideBand)
hoz_90<- as.matrix( msm::pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 9)))
hoz_90 <- round(hoz_90, digits = 2)
hoz_90

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_90) <- stateNames; colnames(hoz_90) <- stateNames
hoz_90
## need to transpose or else arrows go in the wrong direction
hoz_90 <- t(hoz_90) 


plotmat(hoz_90,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Cohort = 9")

```



```{r}
# Graph
table(dat2$Cohort_WideBand)
hoz_50<- as.matrix( msm::pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 5)))
hoz_50 <- round(hoz_50, digits = 2)
hoz_50

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_50) <- stateNames; colnames(hoz_50) <- stateNames
hoz_50
## need to transpose or else arrows go in the wrong direction
hoz_50 <- t(hoz_50) 


plotmat(hoz_50,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Cohort = 5")

```


```{r}
Graph
table(dat2$Cohort_WideBand)
hoz_m<- as.matrix( msm::pmatrix.msm(m3b, covariates = "mean"))
hoz_m <- round(hoz_m, digits = 2)
hoz_m

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_m) <- stateNames; colnames(hoz_m) <- stateNames
hoz_m
## need to transpose or else arrows go in the wrong direction
hoz_m <- t(hoz_m) 


plotmat(hoz_m,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Cohort = average")

```


```{r}
# Graph
table(dat2$Cohort_WideBand)
hoz_70<- as.matrix( msm::pmatrix.msm(m3b, covariates = list(Cohort_WideBand = 7)))
hoz_70 <- round(hoz_70, digits = 2)
hoz_70

stateNames <- c("Secular ","Religious", "Liminal")
row.names(hoz_70) <- stateNames; colnames(hoz_70) <- stateNames
hoz_70
## need to transpose or else arrows go in the wrong direction
hoz_70 <- t(hoz_70) 


plotmat(hoz_70,
        # pos = c(1,2), 
        # lwd = 1, 
        # box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.09, 
        # box.type = "circle", 
        box.prop = 0.5,
        arr.pos =  0.4, 
        shadow.size = 0.001,
        box.col = c("lightgreen","lightblue","orange"),
        # arr.length=.1,
        # arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .12,
        main = "Hidden Markov Model reveals religious switching
       New Zealand: years 2009-2020, Cohort = 7")

```

We can estimate a Hidden Markov Model using the seqHMM package.  

```{r}
# 
# library(seqHMM)
# #check to pull right data
# library(LMest)
# relwidelong<-  df %>%
#   dplyr::mutate(Religious = Religious1-1)%>%
#   dplyr::select(Id,Religious,Wave)
# 
# 
# GenCohortwidelong<-  df %>%
#   dplyr::select(Id,GenCohort,Wave)
# ##THIS HANGS
# #out <- with(relwidelong, long2matrices(id = Id, X = Age, Y = Religious))
# 
# relwide<- spread(relwidelong,Wave,Religious)
# relwide
# genwide<- spread(GenCohortwidelong,Wave,GenCohort)
# 
# Genwide = as.data.frame(genwide$`2019`)
# head(Genwide)
# 
# colnames(Genwide)<-"GenCohort"
# 
# str(Genwide$GenCohort)
# 
# #cohort_wideband = as.numeric(df$Cohort.WideBand)
# cohort_wideband = as.numeric(df$Cohort.WideBand)
# 
# 
# relwide2 = cbind(relwide, Genwide)
# head(relwide2)
# 
# #relwide<-relwide[complete.cases(relwide), ]
# 
# 
# 
# relw <- relwide2[, 2:12]
# relw
# #####
# relseq2 <- seqdef(relw,label = c("Rel-","Rel+"))
# relseq2
# nrow(relseq2)
# 
# 
# ## 2 state model in  this package
# sc_initmod_random <- build_hmm(observations = relseq2, n_states = 3)
# sc_initmod_random
# 
# 
# nrow(GenCohort)
# listdat<-list(relwide2)
# 
# listdat$covariates$cohort<-factor(Genwide$GenCohort, labels = c("silent","boomers","genx","geny","genz"))
# 
# listdat$covariates$cohort
# # 
# 
# 
# 
# sc_initmod <- build_hmm(
#   observations = relseq2,
#   initial_probs = sc_initmod_random$initial_probs,
#   transition_probs = sc_initmod_random$transition_probs,
#   emission_probs = sc_initmod_random$emission_probs,
#   formula = ~ cohort, 
#   data = listdat$covariates
# )
# 
# sc_initmod
# 
# sc_fit <- fit_model(sc_initmod)
# 
# #saveRDS(sc_fit, here::here("models","hmm_threestate_cohort"))
# 
# sc_fit<- readRDS(here::here("models","hmm_threestate_cohort"))
# fm<- sc_fit$model
# 
# 
# summary(fm, conditional_se = FALSE)
# str(fm)
# fm
# 
# library(igraph)
# plot(sc_fit$model,
#      layout = layout_nicely, 
#      # vertex.label = "names"
#      title ="Hidtden States Religious Identification",
#     legend.prop =.3)
# dev.off()
# 
# transprob<-as.matrix(
#   fm$transition_probs
#   )
# transprob

```



```{r}
library(seqHMM)
####  ## Mixed clusters ### THIS WORKS AS FOLLOWS...  
dat2 <- df %>%
  dplyr::filter(YearMeasured == 1 )%>% # filtering only people who have responded in a year
  dplyr::select(Wave, Age, Wave, Religious1 ,Id, Relid, years, Cohort.WideBand,GenCohort, Cohort)%>%
  dplyr::mutate(yearW  = as.numeric(Wave)-1) %>%
  # filter(!is.na(Religious1))%>%
  group_by(Id) %>% filter(n() > 1)%>% # have responded to at least 2 x waves
  filter(n() !=0)%>%
  ungroup(Id)%>%
  group_by(Wave) %>%
  filter(!is.na(Religious1)) %>%
  filter(!is.na(Cohort.WideBand)) %>%
  ungroup()%>%
  tidyr::fill(Cohort.WideBand) %>%
  mutate(age2009 = 2019 - Cohort) %>%
  mutate(Cohort_WideBand = as.factor(Cohort.WideBand)) %>%
  arrange(Id,yearW)

relwidelong<-  dat2 %>%
  dplyr::mutate(Religious = Religious1-1)%>%
  dplyr::select(Id,Religious,Wave)




GenCohortwidelong<-  dat2 %>%
  dplyr::select(Id,GenCohort,Wave) 



#out <- with(relwidelong, long2matrices(id = Id, X = Age, Y = Religious))
GenCohortwidelong
relwide<- spread(relwidelong,Wave,Religious)
relwide
nrow(relwide)

genwide<- spread(GenCohortwidelong,Wave,GenCohort) %>%
  gather(key, val, -Id) %>%
  group_by(Id) %>% 
  fill(val) %>% 
  spread(key, val)


length(unique(genwide$Id))

Genwide<- as.data.frame(as.factor(genwide$`2019`))
Genwide

colnames(Genwide)<-"GenCohort"

str(Genwide$GenCohort)

listdat <-list(relwide)

str(listdat )

listdat$covariates$GenCohort <- factor(Genwide$GenCohort, labels = c("silent","boomers","genx","geny","genz"))

length(listdat$covariates$GenCohort)

relw <- relwide[, 2:12]
relw
#####
relseq <- seqdef(relw,label = c("Rel-","Rel+"))
relseq

# check rows same
nrow(relseq)== nrow(Genwide)



# Plotting state distribution plots of observations
ssplot(mc_obs, title = "State distribution plots")


#
hmm_initmod <- build_hmm(observations = relseq, n_states = 3)
hmm_initmod

## 3 states 2 clusters
initial_probs<-hmm_initmod$initial_probs

initial_probs
transition_probs<-hmm_initmod$transition_probs
transition_probs
em_probs<-hmm_initmod$emission_probs
em_probs

mhmm_init <- list(initial_probs,initial_probs)
mhmm_init

mhmm_trans <- list(transition_probs,transition_probs)
mhmm_trans

mhmm_em <- list(em_probs,em_probs)
mc_obs<- list(relseq)
mc_obs

mhmm_trans
mhmm_init
mhmm_em



init_mhmm<-build_mhmm(observations = mc_obs, 
                      initial_probs = mhmm_init,
                      transition_probs = mhmm_trans, 
                      emission_probs = mhmm_em, 
                      formula = ~ GenCohort, 
                      data = listdat$covariates,
                      n=3,
                      n_clusters=2)

str(init_mhmm)





# Run 3 times

#mhmm_fit <- fit_model(init_mhmm, threads = 4)

saveRDS(mhmm_fit,"mhmm_fit")

#saveRDS(mhmm_fit, here::here( "mhmm_fit_relid_cohort"))

# 
# # control_em = list(restart = list(times = 10)))
# 
# mhmm_fit.v3 <- fit_model(init_mhmm, threads = 4)
# saveRDS(mhmm_fit.v3,"mhmm_fit")

options(scipen=999)

mhmm <- mhmm_fit$model
# mhmm <- mhmm_fit.run2$model
# mhmm <- mhmm_fit.v3$model

Summary
mhmm 

require("igraph")
require(TraMineR)

mhmm

plogis(0.0123 -2.3053)

plot(mhmm,interactive = F, ask = FALSE,
     # layout = layout_nicely,
     legend.prop =.4,
     ncol.legend = 6,
     label.max.length = 15,
     vertex.size = 40)



dev.off()
plogis( 1.8009 +(-0.0201)*60)




dev.off()
str(mhmm)
plot(mhmm,
     interact = F,
     #  layout = layout_nicely,
     # vertex.label = c("ReligiousZeroVolunteer","ReligiousVolunteeer","ZeroReligiousZeroVolunteer","ZeroReligiousVolunteer"),
     main ="Hidden States Religious Change",
     legend.prop =.2,
     ncol.legend = 3)

sep_hmm <- separate_mhmm(mhmm)
plot(sep_hmm[[1]], 
     loops=T, 
     layout =layout_nicely, 
     legend.prop =.1,
     ncol.legend = 3)
dev.off()
plot(sep_hmm[[2]], 
     loops=T, 
     layout =layout_nicely, 
     #layout =layout_nicely, 
     legend.prop =.2,
     ncol.legend = 3,sortv = "mds.hidden")


str(sc_hmm)
sc_hmm <- mc_to_sc(mhmm)

dev.off()  
png(file=here::here("figs", "test_hmm.png"), width = 500, height = 500, units = "px", pointsize = 12)

ssplot(sep_hmm[[2]], type = "I", sortv = "from.end", sort.channel = 0, 
       legend.prop = 0.5)
dev.off()  


ssplot(
  mhmm, 
  #  type = "I", 
  # plots = "both",
  # Sorting subjects according to multidimensional
  # scaling scores of the most probable hidden state paths
  #sortv = "mds.hidden", 
  # Naming the channels
  #  ylab = c("Religious", "Secular", "Mixed"), 
  # Title for the plot
  title = "Observed sequences and the 
most probable paths of hidden states",
  # Labels for hidden states (most common states)
  #  hidden.states.labels = c("1: Childless single, with parents", 
  #                          "2: Childless single, left home",
  #                          "3: Married without children"),
  # Colours for hidden states
  #  hidden.states.colors = c("olivedrab", "bisque", "indianred"),
  # Labels for x axis
  # xtlab = 15:30,   
  xlab = "Age",
  # Proportion for legends
  legend.prop = 0.45)
```


```{r}
######### Next with just two states and two clust 

#
hmm_initmod <- build_hmm(observations = relseq, n_states = 2)
hmm_initmod

## 2 states 2 clusters
initial_probs<-hmm_initmod$initial_probs

initial_probs
transition_probs<-hmm_initmod$transition_probs
transition_probs
em_probs<-hmm_initmod$emission_probs
em_probs

mhmm_init <- list(initial_probs,initial_probs)
mhmm_init

mhmm_trans <- list(transition_probs,transition_probs)
mhmm_trans

mhmm_em <- list(em_probs,em_probs)
mc_obs<- list(relseq)



init_mhmm_2 <-build_mhmm(observations = mc_obs, 
                         initial_probs = mhmm_init,
                         transition_probs = mhmm_trans, 
                         emission_probs = mhmm_em, 
                         formula = ~ GenCohort, 
                         data = listdat$covariates,
                         n = 2,
                         n_clusters = 2)

init_mhmm_2

# Run 3 times

mhmm_fit_2 <- fit_model(init_mhmm_2)


saveRDS(mhmm_fit_2, here::here( "mhmm_fit_relid_cohort-2"))

# 
# # control_em = list(restart = list(times = 10)))
# 
# mhmm_fit.v3 <- fit_model(init_mhmm, threads = 4)
# saveRDS(mhmm_fit.v3,"mhmm_fit")

options(scipen=999)

mhmm2 <- mhmm_fit_2$model

mhmm2


plot(mhmm2,interactive = F, ask = FALSE,
     # layout = layout_nicely,
     legend.prop =.4,
     ncol.legend = 6,
     label.max.length = 15,
     vertex.size = 40)






dev.off()

plot(mhmm2,
     interact = F,
     #  layout = layout_nicely,
     # vertex.label = c("ReligiousZeroVolunteer","ReligiousVolunteeer","ZeroReligiousZeroVolunteer","ZeroReligiousVolunteer"),
     main ="Hidden States Religious Change",
     legend.prop =.2,
     ncol.legend = 3)

sep_hmm <- separate_mhmm(mhmm2)
plot(sep_hmm[[1]], 
     loops=T, 
     layout =layout_nicely, 
     legend.prop =.1,
     ncol.legend = 3)
dev.off()
plot(sep_hmm[[2]], 
     loops=T, 
     layout =layout_nicely, 
     #layout =layout_nicely, 
     legend.prop =.2,
     ncol.legend = 3,sortv = "mds.hidden")


# 
# 
# png(file=here::here("figs", "test_hmm.png"), width = 500, height = 500, units = "px", pointsize = 12)
# 
# ssplot(sep_hmm[[2]], type = "I", sortv = "from.end", sort.channel = 0, 
#     legend.prop = 0.5)
# dev.off()  


ssplot(
  mhmm, 
  #  type = "I", 
  # plots = "both",
  # Sorting subjects according to multidimensional
  # scaling scores of the most probable hidden state paths
  #sortv = "mds.hidden", 
  # Naming the channels
  #  ylab = c("Religious", "Secular", "Mixed"), 
  # Title for the plot
  title = "Observed sequences and the 
most probable paths of hidden states",
  # Labels for hidden states (most common states)
  #  hidden.states.labels = c("1: Childless single, with parents", 
  #                          "2: Childless single, left home",
  #                          "3: Married without children"),
  # Colours for hidden states
  #  hidden.states.colors = c("olivedrab", "bisque", "indianred"),
  # Labels for x axis
  # xtlab = 15:30,   
  xlab = "Age",
  # Proportion for legends
  legend.prop = 0.45)


## One chanel


hmm_initmod <- build_hmm(observations = relseq, n_states = 2)
hmm_initmod

## 2 states 2 clusters
initial_probs<-hmm_initmod$initial_probs

initial_probs
transition_probs<-hmm_initmod$transition_probs
transition_probs
em_probs<-hmm_initmod$emission_probs
em_probs


## 2 state model in  this package
sc_initmod_random <- build_hmm(observations = relseq, n_states = 2)
sc_initmod_random


listdat$covariates$cohort<-factor(Genwide$GenCohort, labels = c("silent","boomers","genx","geny","genz"))

length(listdat$covariates$cohort)
nrow(relseq)



sc_initmod <- build_hmm(
  observations = relseq,
  initial_probs = initial_probs,
  transition_probs = transition_probs,
  emission_probs = em_probs,
  formula = ~ cohort, # won't work
  data = listdat$covariates
)


#sc_fit <- fit_model(sc_initmod)

#saveRDS(sc_fit, here::here("models","hmm_twostate_cohort"))

sc_fit<- readRDS(here::here("models","hmm_twostate_cohort"))
fm<- sc_fit$model


summary(fm, conditional_se = FALSE)
str(fm)
fm

library(igraph)
plot(sc_fit$model,
     layout = layout_nicely,
     # vertex.label = "names"
     title ="Hidtden States Religious Identification",
     legend.prop =.3)
dev.off()

transprob<-as.matrix(
  fm$transition_probs
)
transprob


```


```{r}
##### MSM

#
q0
q0 <-rbind(c(.1,.1,.1),c(.1,.1,.1),c(.1,.1,.1)) #  Allow permitted transitions

## THIS WORKED! :)c 
gh.mod.mnm.2  <- msm(Religious1 ~ years,
                     Id,
                     data=ldf.5,
                     covariates = ~Age,
                     est.initprobs = TRUE,
                     qmatrix =q0,
                     ematrix = rbind(c(.1,.1,.1),
                                     c(.1,.1,.1),
                                     c(.1,.1,.1)))


qmatrix.msm(gh.mod.mnm.2)

pmatrix.msm(gh.mod.mnm.2) # This worked!
#saveRDS(gh.mod.mnm.2,"gh.mod.mnm.2")
gh.mod.mnm.2<-readRDS("gh.mod.mnm.2")
## Big and sharp shift at 50 
pmatrix.msm(gh.mod.mnm.2, covariates = list(Age =50))
pmatrix.msm(gh.mod.mnm.2,covariates = "mean")


q0 <-rbind(c(1,1,1),c(1,1,1),c(1,1,1)) #  Allow permitted transitions

#### nonsense
# gh.mod.mnm.3  <- msm(Religious1 ~ years,
#                       Id,
#                       data=ldf.5,
#                       covariates = ~Age,
#                       qmatrix =q0,
#                       ematrix = rbind(c(.1,.1,.1),
#                                       c(.1,.1,.1),
#                                       c(.1,.1,.1)),
#                      est.initprobs=TRUE)
#  
# 
# pmatrix.msm(gh.mod.mnm.3)
# ppass.msm(gh.mod.mnm.3)
# odds.msm(gh.mod.mnm.3)
# plot(gh.mod.mnm.3)
# saveRDS(gh.mod.mnm.3,"gh.mod.mnm.3")

