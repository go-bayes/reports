
library("here") # file management
#library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation 
#library("ggpubr") # graphs
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
library("sjstats")
library("magick")
library("simstudy")

<<<<<<< Updated upstream
easystats::install_suggested()
=======


>>>>>>> Stashed changes

# rstan options
library("dplyr")
library("brms") # bayesian estimation
library("cmdstanr") # backend brms
library("rstan") # backend brms
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
# library(bayesplot)
# color_scheme_set("brightblue")

# Make test data and improve list for  MI.
imps_bind <- readRDS(here::here("_posts", "mus", "mods", "imps_bind"))

# make list
imp1 <- as.data.frame(imps_bind$imputations$imp[[1]])
imp2 <- as.data.frame(imps_bind$imputations$imp[[2]])
imp3 <- as.data.frame(imps_bind$imputations$imp[[3]])
imp4 <- as.data.frame(imps_bind$imputations$imp[[4]])
imp5 <- as.data.frame(imps_bind$imputations$imp[[5]])
imp6 <- as.data.frame(imps_bind$imputations$imp[[6]])
imp7 <- as.data.frame(imps_bind$imputations$imp[[7]])
imp8 <- as.data.frame(imps_bind$imputations$imp[[8]])
imp9 <- as.data.frame(imps_bind$imputations$imp[[9]])
imp10 <- as.data.frame(imps_bind$imputations$imp[[10]])


ameliadata <- list(imp1,
                   imp2,
                   imp3,
                   imp4,
                   imp5,
                   imp6,
                   imp7,
                   imp8,
                   imp9,
                   imp10)

# test llist
str(ameliadata)

# save list 
saveRDS(ameliadata, here::here("_posts", "mus", "mods", "ameliadata"))


# make test data w/ 22 Ids

i1 <- imp1 %>% arrange(Id, Wave) %>% slice(1:900)
i2 <- imp2 %>% arrange(Id, Wave) %>% slice(1:900)
i3 <- imp3 %>% arrange(Id, Wave) %>% slice(1:900)
i4 <- imp4 %>% arrange(Id, Wave) %>% slice(1:900)
i5 <- imp5 %>% arrange(Id, Wave) %>% slice(1:900)
i6 <- imp6 %>% arrange(Id, Wave) %>% slice(1:900)
i7 <- imp7 %>% arrange(Id, Wave) %>% slice(1:900)
i8 <- imp8 %>% arrange(Id, Wave) %>% slice(1:900)
i9 <- imp9 %>% arrange(Id, Wave) %>% slice(1:900)
i10 <- imp10 %>% arrange(Id, Wave) %>% slice(1:900)

testdata <- list(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)

# observe
str(testdata)

saveRDS(testdata, here::here("_posts", "mus", "mods", "testdata"))



# BAYES USE CMDSTAN str prior #  -----------------------------------------------------------------
ameliadata<- readRDS(here::here("_posts", "mus", "mods", "ameliadata"))
testdata<- readRDS(here::here("_posts", "mus", "mods", "testdata"))

get_prior( bf(Ys ~ As  *  Wave + (0 + As|| Id),
              sigma ~ 0 + As, set_rescor(rescor = FALSE)), data = i10)
# test model 

test0 <- brms::brm(Ys ~ 1, data = i1)


system.time( testfit  <- brms::brm_multiple( 
  bf(Ys ~ As  *  Wave + (0 + As|| Id),
     sigma ~ 0 + As, set_rescor(rescor = FALSE)),
  family = gaussian, 
  data = testdata,
  c(
    prior(normal(.2,.25), class = b, coef = "As1"),
    prior(normal(0,.25), class = b, coef = "Wave"),
    prior(normal(0,.25),  class= b, coef = "As1:Wave"),
    prior(normal(log(1),1),  class= b, coef = "As0", dpar = "sigma"),
    prior(normal(log(1),1),  class= b, coef = "As1", dpar = "sigma"),
    prior(student_t(3,4.15,1), class = Intercept),
    prior(student_t(3,0,2.5), class = sd,  coef = "As0", group = "Id"),
    prior(student_t(3,0,2.5), class = sd,  coef = "As1", group = "Id")
 ),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 5,
  #future = TRUE#,
  backend = "cmdstanr"
# file = here::here("_posts", "mus", "mods", "remove_fit.rds")
) )
#stancode(testfit)

prior_summary(testfit)

summary(testfit)

plot(testfit)

testfit_plot <- plot(conditional_effects(testfit,  "Wave:As",  ndraws = 400, spaghetti = T), points = F)

bayes_test <- testfit_plot$`Wave:As` + # scale_y_continuous(limits=c(1.0,8)) +
  labs(subtitle="Regularised Prior",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.5) + theme_fivethirtyeight()
bayes_test




### BIGMODEL 



system.time( m1_model  <- brms::brm_multiple( 
  bf(Ys ~ As  *  Wave + (0 + As|| Id),
     sigma ~ 0 + As, set_rescor(rescor = FALSE)),
  family = gaussian, 
  data = ameliadata,
  c(
    prior(normal(.2,.25), class = b, coef = "As1"),
    prior(normal(0,.25), class = b, coef = "Wave"),
    prior(normal(0,.25),  class= b, coef = "As1:Wave"),
    prior(normal(log(1),1),  class= b, coef = "As0", dpar = "sigma"),
    prior(normal(log(1),1),  class= b, coef = "As1", dpar = "sigma"),
    prior(student_t(3,4.15,1), class = Intercept),
    prior(student_t(3,0,2.5), class = sd,  coef = "As0", group = "Id"),
    prior(student_t(3,0,2.5), class = sd,  coef = "As1", group = "Id")
  ),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 2,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "m1_model.rds")
) )
#stancode(testfit)

prior_summary(testfit)

summary(testfit)

plot(testfit)

testfit_plot <- plot(conditional_effects(testfit,  "Wave:As",  ndraws = 400, spaghetti = T), points = F)

bayes_test <- testfit_plot$`Wave:As` + # scale_y_continuous(limits=c(1.0,8)) +
  labs(subtitle="Regularised Prior",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.5) + theme_fivethirtyeight()
bayes_test




# no priors

system.time( testfit2  <- brms::brm_multiple( 
  bf(Ys ~ As  *  Wave + (0 + As||Id),
     sigma ~ 0 + As, set_rescor(rescor = FALSE)),
  family = gaussian, 
  data = testdata,
  # c(
  #   prior(normal(.15,1), class = b, coef = "As1"),
  #   prior(normal(.05,1), class = b, coef = "Wave"),
  #   prior(normal(0,1),  class= b, coef = "As1:Wave"),
  #   prior(normal(log(1),.5),  class= b, coef = "As0", dpar = "sigma"),
  #   prior(normal(log(1),.5),  class= b, coef = "As1", dpar = "sigma"),
  #   prior(student_t(3,4,1), class = Intercept),
  #   prior(student_t(3,0,1), class = "sd"),
  #   prior(student_t(3,0,1), class = "sd", coef = "As0", group = "Id"),
  #   prior(student_t(3,0,1), class = "sd", coef = "As1", group = "Id")),
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr"#,
  #  file = here::here("_posts", "mus", "mods", "test_fit.rds")
) )
prior_summary(testfit2)
stancode(testfit2)

summary(testfit)
summary(testfit2)


testfit_plot2 <- plot(conditional_effects(testfit2,  "Wave:As",  ndraws = 500, spaghetti = T), points = F)

bayes_test2 <- testfit_plot2$`Wave:As`  +  scale_y_continuous(limits=c(1.0,8)) +
  labs(subtitle="Weakly regularised prior",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.5) + theme_fivethirtyeight()

testpriors_graph<-(bayes_test + bayes_test2) + theme_fivethirtyeight() + plot_annotation(tag_levels = "i")

testpriors_graph

ggsave(
  testpriors_graph,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "testpriors_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)
# run model with strong priors --------------------------------------------

system.time( strong_fit  <- brms::brm_multiple( 
  bf(Ys ~ As  *  Wave + (0 + As||Id),
     sigma ~ 0 + As , set_rescor(rescor = FALSE)),
  family = gaussian, 
  data = ameliadata,
  c(
    prior(normal(.15,.5), class = b, coef = "As1"),
    prior(normal(.05,.5), class = b, coef = "Wave"),
    prior(normal(0,.5),  class= b, coef = "As1:Wave"),
    prior(normal(log(1),.5),  class= b, coef = "As0", dpar = "sigma"),
    prior(normal(log(1),.5),  class= b, coef = "As1", dpar = "sigma"),
    prior(student_t(3,4,1), class = Intercept),
    prior(student_t(3,0,1), class = "sd"),
    prior(student_t(3,0,1), class = "sd", coef = "As0", group = "Id"),
    prior(student_t(3,0,1), class = "sd", coef = "As1", group = "Id")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "strong_fit")
) )

lazerhawk::brms_SummaryTable(strong_fit, panderize=F)


check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
mod$print()
stan_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(stan_file)
mod$print()
# set up parrallel models -------------------------------------------------



