# futures parallel model
# notes -- m_cluster worked, took 

## This contains the model that we used !!! 


library("brms")
library("here")
library("tidyverse")
library("future")
#library(tidyverse)
library("dplyr")
library("brms") # bayesian estimation
#library("cmdstanr") # backend brms
library("rstan")
#rstan_options(threads_per_chain = 1)
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course

library("ggplot2") # graphs
library("ggthemes") #themes
library("ggokabeito")   # color palette
library("bayesplot")
library("wesanderson")
library("RColorBrewer")
library("ggsci")

here()
# import data -------------------------------------------------------------

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



# make test data w/ 22 Ids

i1 <- imp1 %>% arrange(Id, Wave) %>% slice(1:128)
i2 <- imp2 %>% arrange(Id, Wave) %>% slice(1:128)
i3 <- imp3 %>% arrange(Id, Wave) %>% slice(1:128)
i4 <- imp4 %>% arrange(Id, Wave) %>% slice(1:128)
i5 <- imp5 %>% arrange(Id, Wave) %>% slice(1:128)
i6 <- imp6 %>% arrange(Id, Wave) %>% slice(1:128)
i7 <- imp7 %>% arrange(Id, Wave) %>% slice(1:128)
i8 <- imp8 %>% arrange(Id, Wave) %>% slice(1:128)
i9 <- imp9 %>% arrange(Id, Wave) %>% slice(1:128)
i10 <- imp10 %>% arrange(Id, Wave) %>% slice(1:128)

str(imp1)

ameliadata <- list(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10)

# BAYES USE CMDSTAN str prior #  -----------------------------------------------------------------
ameliadata<- readRDS(here::here("_posts", "mus", "mods", "ameliadata"))
#testdata<- readRDS(here::here("_posts", "mus", "mods", "testdata"))

prior = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  prior(normal(0.05, 0.25), class = b, coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)

bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
            sigma ~ 0 + As, set_rescor(rescor = FALSE))






# test --------------------------------------------------------------------
# 
# 
# m_test  <- brms::brm_multiple(
#     bform,
#     family = gaussian,
#     data = testdata,
#     prior = prior,
#     seed = 1234,
#     init = 0,
#     warmup = 1000,
#     iter =  2000,
#     chains = 2,
#     future = TRUE
#     #, file = here::here("_posts", "mus", "mods", "m_core.rds")
#   )
# 
# 
# summary(m_test)
# prior_summary(m_test)



# model -------------------------------------------------------------------

# system.time(
# m_test  <- brms::brm_multiple(
#   bform,
#   family = gaussian,
#   data = testdata,
#   prior = prior,
#   #seed = 1234,
#   init = 0,
#   warmup = 1000,
#   iter =  2000,
#   chains = 2,
#   future = TRUE
#   #, file = here::here("_posts", "mus", "mods", "m_core.rds")
# )
# )
# 
# 
# summary(m_test)
# prior_summary(m_test)



# model_use ---------------------------------------------------------------
# start in fresh R session 


system.time(
m_cluster  <- brms::brm_multiple(
  bform,
  family = gaussian,
  data = ameliadata,
  prior = prior,
  #seed = 1234,
  init = 0,
  warmup = 1000,
  iter =  2000,
  chains = 2,
  future = TRUE
  , file = here::here("_posts", "mus", "mods", "m_cluster.rds")
)
)

# saveRDS(
#   m_cluster,  file = here::here("_posts", "mus", "mods", "m_cluster.rds")
# )

#
# get priors 
prior_summary(m_cluster)

stancode(m_cluster)

# simulate priors ---------------------------------------------------------
str(imp5)

prior = c(
  set_prior("normal(0, 1)", class = "b")
)

bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))



get_prior(bform,imp5)

sim_prior_strong  <- brm(
  bform,
  family = "gaussian",
  data = imp5,
  prior = c(set_prior('normal(0, 1)', class='b'),
            set_prior('exponential(1)', class = "b", dpar = "sigma", coef = "As1"),
            set_prior('exponential(1)', class = "b",  dpar = "sigma", coef = "As0")),
  sample_prior = "only",
  # only one dataset
  #seed = 1234,
  init = 0,
  warmup = 500,
  iter =  1000,
  chains = 1,
 # backend = "cmdstanr"
  ,file = here::here("_posts", "mus", "mods", "sim_prior_strong.rds")
)


sim_prior_strong_plot <- plot(conditional_effects(sim_prior_strong,  "Wave:As",  ndraws = 200, spaghetti = T))
library(ggsci)
sim_prior_strong_plot2 <- sim_prior_strong_plot$`Wave:As`  + scale_y_continuous(limits=c(-10.0,15)) +
  labs(subtitle="Sampling from prior only: weakly regularised",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


sim_prior_strong_plot2

ggsave(
  sim_prior_strong_plot2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_strong_plot2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




prior_strong_even = c(
  set_prior('normal(0.2, 0.25)', class = 'b', coef = "As1"),
  set_prior('normal(0.05, 0.25)', class = "b", coef = "Wave"),
  set_prior("normal(0, 0.25)",  class = "b", coef = "As1:Wave"),
  set_prior("normal(0,1)", class = "b",coef = "As0", dpar = "sigma"),
  set_prior("normal(0,1)",  class = "b",  coef = "As1", dpar = "sigma"),
  set_prior("student_t(3, 4.1, 1)", class = "Intercept"),
  set_prior("student_t(3, 0, 2.5)", class = "sd", coef = "As0", group = "Id"),
  set_prior("student_t(3, 0, 2.5)", class = "sd", coef = "As1",group = "Id"))


system.time(
  sim_prior_stronger_even  <- brms::brm(
    bform,
    prior= prior_strong_even,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
   , file = here::here("_posts", "mus", "mods", "sim_prior_stronger_even.rds")
  )
)

sim_prior_stronger_evenP <- plot(conditional_effects(sim_prior_stronger_even,  "Wave:As",  ndraws = 200, spaghetti = T))

sim_prior_stronger_evenPLOT <- sim_prior_stronger_evenP$`Wave:As`   + scale_y_continuous(limits=c(-10.0,15)) +
  labs(subtitle="Sampling from prior only: moderately regularised",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


sim_prior_stronger_evenPLOT
ggsave(
  sim_prior_stronger_evenPLOT,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
library(patchwork)
compare_priors <- sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT+ 
  plot_annotation(tag_levels  = "i", title= "Comparision of wealkly regularised and strongly regularised priors")

compare_priors

ggsave(
  compare_priors,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior("student_t(3, 4, 1)", class = "Intercept", lb=1, ub=7),
  set_prior("exponential(1)", class = "sd"))


system.time(
  sim_prior_stronger_even2  <- brms::brm(
    bform,
    prior= prior_strong_even2,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
    , file = here::here("_posts", "mus", "mods", "sim_prior_stronger_even2.rds")
  )
)

sim_prior_stronger_evenP2 <- plot(conditional_effects(sim_prior_stronger_even2,  "Wave:As",  ndraws = 200, spaghetti = T))

sim_prior_stronger_evenPLOT2 <- sim_prior_stronger_evenP2$`Wave:As`   + scale_y_continuous(limits=c(-10.0,15)) +
  labs(subtitle="Sampling from prior only: strongly regularised",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


sim_prior_stronger_evenPLOT2
ggsave(
  sim_prior_stronger_evenPLOT2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

library(patchwork)
compare_priors <- sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT + sim_prior_stronger_evenPLOT2 + 
  plot_annotation(tag_levels  = "i", title= "Comparision of regularised priors:", 
                  subtitle = "sampled from the posterior only")

compare_priors

ggsave(
  compare_priors,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# model with strongly regularised priors ----------------------------------
prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior("student_t(3, 4, 1)", class = "Intercept", lb=1, ub=7),
  set_prior("exponential(1)", class = "sd"))



### MODEL THAT WE USED !!!! 

system.time(
  m_cluster_st  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadata,
    prior = prior_strong_even2,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    , file = here::here("_posts", "mus", "mods", "m_cluster_st.rds")
  )
)

# saveRDS(
#   m_cluster,  file = here::here("_posts", "mus", "mods", "m_cluster.rds")
# )

#
# get priors 
prior_summary(m_cluster_st)

stancode(m_cluster_st)



# graph  ------------------------------------------------------------------


pl_m_cluster_st<- plot(conditional_effects(m_cluster_st,  "Wave:As",  ndraws = 200, spaghetti = T))

library(ggsci)
plot_m_cluster_st <- pl_m_cluster_st$`Wave:As`  + scale_y_continuous(limits=c(4.1,4.5)) +
  labs(subtitle="Predicted marginal means for prejudice by attack condition",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


plot_m_cluster_st



tab_cluster_st <- lazerhawk::brms_SummaryTable(m_cluster_st, panderize=F)
tab_cluster_st

#summary_m_cluster_st <-summary(m_cluster_st)
#summary_m_cluster_st


# saveRDS(
#   summary_m_cluster_st,  file = here::here("_posts", "mus", "mods", "summary_m_cluster_st")
# )




tab_cluster_st
tab_cluster_st %>%
  kable(booktabs = T, "latex", caption =  "Marginal effect of attack on warmth to Muslims", digits = 2) %>%
  print()


ggsave(
  plot_m_cluster_st,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "plot_m_cluster_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)





# make graphs for the stronger prior  --------------------------------

# bayesplot scheme
color_scheme_set("brightblue")

# graph for areas


## graph of expectation

m1_test_model_plot_st <- plot(conditional_effects(m_cluster_st,  "Wave:As",  ndraws = 200, spaghetti = T))

# save graph
#saveRDS(m1_test_model_plot_st, here::here("_posts", "mus", "mods", "m1_test_model_plot_st"))

m1_test_model_plot_st <- readRDS(here::here("_posts", "mus", "mods", "m1_test_model_plot_st"))


# mage graph
cluster_plot_st <- m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


cluster_plot_st <- cluster_plot_st + labs(title = "Predicted marginal means for prejudice by attack condition")

### USE !!! 
cluster_plot_st


# sim compare graph
cluster_plot_st_sim <- cluster_plot_st +  scale_y_continuous(limits=c(4.05,4.55)) +
  labs(subtitle="",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()

cluster_plot_st_sim


ggsave(
  cluster_plot_st,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "cluster_plot_title_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


## compare graph to more weakly regulated priors
cluster_plot_st <- m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Strongly regularising priors",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()

library(patchwork)

weak_strong_priors_prediction <- cluster_plot_st + cluster_plot_title + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "i", title = "Comparison of weakly and strongly regularising priors shows little effect in choice of prior")
weak_strong_priors_prediction

ggsave(
  weak_strong_priors_prediction,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "weak_strong_priors_prediction.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# strong prior density overlay and combo plots ----------------------------
# density overlay strong prior 
dens_overlay_strong_prior <- mcmc_dens_overlay(m_cluster_st, pars  = c("b_As1", "b_Wave", "b_As1:Wave"))

# save graph
saveRDS(dens_overlay_strong_prior, here::here("_posts", "mus", "mods", "dens_overlay_strong_prior"))

dens_overlay_strong_prior <- readRDS(here::here("_posts", "mus", "mods", "dens_overlay_strong_prior"))


### USE !!! 
dens_overlay_strong_prior





# areas plot
color_scheme_set("brightblue")

areas_plot_st <- mcmc_plot(m_cluster_st, 
                        variable = c("b_As1", "b_Wave", "b_As1:Wave"), 
                        #regex_pars = "beta",
                        type = 'areas',
                        prob = 0.95) # 80% intervals
                        # prob_outer = 0.95, # 99%
  #                       point_est = "mean") 
  +
  # labs(
  #   title = "Posterior distributions and 80% intervals")
# plot
areas_plot_st
saveRDS(areas_plot_st, here::here("_posts", "mus", "mods", "areas_plot"))
areas_plot_st <- readRDS( here::here("_posts", "mus", "mods", "areas_plot"))


### USE !!! 
areas_plot_st


ggsave(
  areas_plot_st,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "areas_plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



#hist <- mcmc_hist(m_cluster, variable = c("b_As1", "b_Wave", "b_As1:Wave"))


ggsave(
  dens_overlay_strong_prior,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "dens_overlay_strong_prior.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



dens_overlay_strong_prior









# strong prior combo plots ------------------------------------------------


plot_post_1<- plot(m_cluster_st, 
                   variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
                   plot = F,
                   ask = T)

plot_post_2<- plot(m_cluster_st, 
                   variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
                   plot = F,
                   ask = T)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]



pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]
pp2

plot(m_cluster_st)


plot_post_8_st<- plot(m_cluster_st, 
                   variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave",
                                "b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
                   plot = F,
                   newpage = F,
                   N=8)
plot_post_8_st


library(patchwork)
library(ggplot2)
plot_post_8_st[[2]]
trace_plotsS <- plot_post_8_st[[1]] 
trace_plotsS


p_trace_plotsS <- trace_plotsS$bayesplots[[1]] + trace_plotsS$bayesplots[[2]]

u_trace_plotsS<- p_trace_plotsS + labs(title = "MCMC posterior locations + chains")

u_trace_plotsS

saveRDS(p_trace_plotsS, here::here("_posts", "mus", "mods", "p_trace_plotsS"))

p_trace_plotsS <- readRDS (here::here("_posts", "mus", "mods", "p_trace_plotsS"))

p_trace_plotsS

u_trace_plotsS

ggsave(
  u_trace_plotsS,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "u_trace_plotsS.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


hyp0 <- hypothesis(m_cluster_st, "As1 + As1:Wave > As1 - Wave") 
p0 <-plot(hyp0, plot=F) 
out_h0<-p0[[1]] + labs(subtitle = "Post trajectory > 0") + 
  #scale_fill_colorblind() + 
  theme_classic()
out_h0

hyp1 <- hypothesis(m_cluster_st, "Wave + As1:Wave  =  Wave") 
p1 <-plot(hyp1, plot=F) 
out_h1<-p1[[1]] + labs(subtitle = "Post < Pre trajectory") + 
  #scale_fill_colorblind() +
  theme_classic()
out_h1



hyp2<- hypothesis(m_cluster_st, "sigma_As0 = sigma_As1", class = "b")
p2 <-plot(hyp2, plot=F) 

# out_h2<- p2[[1]]+ labs(title = "Sigma of attack condition is lower than sigma of no-attack") + 
#   #scale_fill_colorblind() + 
#   theme_classic()
# out_h2

hyp3 <- hypothesis(m_cluster_st, "As1 >0") 

p3 <-plot(hyp3, plot=F) 
out_h3<-p3[[1]] + labs(subtitle = "Attack causes warmth") + 
  #scale_fill_colorblind() + 
  theme_classic()
out_h3


# combine graph
bayes_hypothesis <- out_h3 +  out_h0 + out_h1 + plot_annotation(
  #title = "Bayesian hypothesis tests",
  tag_levels = "i")
bayes_hypothesis<- bayes_hypothesis +   plot_layout(guides = 'collect')
bayes_hypothesis



ggsave(
  bayes_hypothesis_st,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height =9,
  units = "in",
  filename = "bayes_hypothesis_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



#### FIGURE 2 USE
dev.off()

#  (out_h3/ out_h0 / out_h1  )
fig2 <- (  dens_overlay_strong_prior ) /  (areas_plot_st + cluster_plot_st  ) + plot_annotation(tag_levels = "i")
fig2

ggsave(
  fig2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 09,
  units = "in",
  filename = "fig2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)









# show plot
results_main <- cluster_plot_st +(out_h3/ out_h0 / out_h1  ) + plot_annotation(tag_levels = "i") + 
  plot_layout(ncol = 2, widths = c(2, 1))
results_main



ggsave(
  results_main,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_main.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


# # show plot
# results_st <- cluster_plot_st +(out_h3/ out_h0 / out_h1  ) + plot_annotation(tag_levels = "i") + 
#   plot_layout(ncol = 2, widths = c(2, 1))
# results_st
# 
# # show plot
# results_st <- results_st +(out_h3/ out_h0 / out_h1  ) + plot_annotation(tag_levels = "i") + 
#   plot_layout(ncol = 2, widths = c(2, 1))
# results_st
# 
# 


dens_overlay_strong_prior




results_use <-areas_plot_st + dens_overlay_strong_prior  + 
  plot_annotation(tag_levels = "i") 
#
results_use

ggsave(
  results_use,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_use.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)







# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)

# old results -----------------------------------------------------------------

# model from home machine run
def_homemachine <-readRDS(here::here("_posts", "mus", "mods", "def_homemachine.rds"))

# combine with cluster machine run
# will not work 
models_combined <- combine_models(def_homemachine, m_cluster )

summaryhome <- summary(def_homemachine)
summaryhome


# density overlay home
dens_overlay_home <- mcmc_dens_overlay(def_homemachine, pars  = c("b_As1", "b_Wave", "b_As1:Wave"))
#dens_overlay_home 
saveRDS(dens_overlay_home, here::here("_posts", "mus", "mods", "dens_overlay_home"))


tab_cluster <- lazerhawk::brms_SummaryTable(m_cluster, panderize=F)


tab_cluster
tab_cluster %>%
  kable(booktabs = T, "latex", caption =  "Marginal effect of attack on warmth to Muslims", digits = 2) %>%
  print()


cluster_plot <- m1_test_model_plot$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


cluster_plot_title <- cluster_plot + labs(title = "Predicted marginal means for prejudice by attack condition")
cluster_plot_title




ggsave(
  cluster_plot_title,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "cluster_plot_title.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)







# mcluster results --------------------------------------------------------



lazerhawk::brms_SummaryTable(m_cluster)

#summary_m_cluster <-summary(m_cluster)

# saveRDS(
#   summary_m_cluster,  file = here::here("_posts", "mus", "mods", "summary_m_cluster")
# )





summary_m_cluster <- readRDS(here::here("_posts", "mus", "mods", "summary_m_cluster"))

summary_m_cluster

m1_test_model_plot <- plot(conditional_effects(m_cluster,  "Wave:As",  ndraws = 200, spaghetti = T))
#saveRDS(m1_test_model_plot, here::here("_posts", "mus", "mods", "m1_test_model_plot"))
m1_test_model_plot <- readRDS(here::here("_posts", "mus", "mods", "m1_test_model_plot"))

# bayesplot scheme
color_scheme_set("brightblue")

cluster_plot <- m1_test_model_plot$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Weakly regularising priors",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
 # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()


cluster_plot_title <- cluster_plot #+ labs(title = "Predicted marginal means for prejudice by attack condition")
cluster_plot_title


ggsave(
  cluster_plot_title,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "cluster_plot_title.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




# inspect
color_scheme_set("brightblue")
#color_scheme_set("brewer-Spectral")
#color_scheme_set("mix-blue-red")
#plot(m_cluster)

plot_post_1<- plot(m_cluster, 
                   variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
                   plot = F,
                   ask = T)

plot_post_2<- plot(m_cluster, 
                   variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
                   plot = F,
                   ask = T)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]

<- plot_post_1[[1]]$bayesplots

pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]
pp2


plot_post_8<- plot(m_cluster, 
                   variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave",
                                "b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
                   plot = F,
                   newpage = F,
                   N=8)
plot_post_8


library(patchwork)
library(ggplot2)
trace_plots <- plot_post_8[[1]] 
p_trace_plots <- trace_plots$bayesplots[[1]] + trace_plots$bayesplots[[2]]
u_trace_plots<- p_trace_plots + labs(title = "MCMC posterior locations + chains")
u_trace_plots

saveRDS(p_trace_plots, here::here("_posts", "mus", "mods", "trace_plots"))

ggsave(
  u_trace_plots,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "u_trace_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

## hypotheses

hyp0 <- hypothesis(m_cluster, "As1 + As1:Wave > As1 - Wave") 
p0 <-plot(hyp0, plot=F) 
out_h0<-p0[[1]] + labs(subtitle = "Post trajectory > 0") + 
  #scale_fill_colorblind() + 
  theme_classic()
out_h0

hyp1 <- hypothesis(m_cluster, "Wave + As1:Wave  =  Wave") 
p1 <-plot(hyp1, plot=F) 
out_h1<-p1[[1]] + labs(subtitle = "Post < Pre trajectory") + 
  #scale_fill_colorblind() +
  theme_classic()
out_h1



hyp2<- hypothesis(m_cluster, "sigma_As0 = sigma_As1", class = "b")
p2 <-plot(hyp2, plot=F) 

# out_h2<- p2[[1]]+ labs(title = "Sigma of attack condition is lower than sigma of no-attack") + 
#   #scale_fill_colorblind() + 
#   theme_classic()
# out_h2

hyp3 <- hypothesis(m_cluster, "As1 >0") 

p3 <-plot(hyp3, plot=F) 
out_h3<-p3[[1]] + labs(subtitle = "Attack causes warmth") + 
  #scale_fill_colorblind() + 
  theme_classic()
out_h3


# combine graph
bayes_hypothesis <- out_h3 +  out_h0 + out_h1 + plot_annotation(
  #title = "Bayesian hypothesis tests",
  tag_levels = "i")
bayes_hypothesis<- bayes_hypothesis +   plot_layout(guides = 'collect')
bayes_hypothesis



ggsave(
  bayes_hypothesis,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height =9,
  units = "in",
  filename = "bayes_hypothesis.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)




# show plot
results <- cluster_plot_title +(out_h3/ out_h0 / out_h1  ) + plot_annotation(tag_levels = "i") + 
  plot_layout(ncol = 2, widths = c(2, 1))
results

# show plot
results <- cluster_plot_title +(out_h3/ out_h0 / out_h1  ) + plot_annotation(tag_levels = "i") + 
  plot_layout(ncol = 2, widths = c(2, 1))
results



results_use <-areas_plot_st + dens_overlay_plot_st  + 
  plot_annotation(tag_levels = "i") 
#
results_use

ggsave(
  results0,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results0.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



#beta_trace <- plot(m_cluster, variable = "^b", regex = TRUE)


# individual
#round(m_cluster$rhats, 1)

# areas plot
color_scheme_set("brightblue")

areas_plot <- mcmc_plot(m_cluster, 
                       variable = c("b_As1", "b_Wave", "b_As1:Wave"), 
                       #regex_pars = "beta",
                       type = 'areas',
          prob = 0.8, # 80% intervals
         # prob_outer = 0.95, # 99%
          point_est = "mean") # +
  # labs(
  #   title = "Posterior distributions and 80% intervals")
areas_plot
saveRDS(areas_plot, here::here("_posts", "mus", "mods", "areas_plot"))


ggsave(
  areas_plot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "areas_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



#hist <- mcmc_hist(m_cluster, variable = c("b_As1", "b_Wave", "b_As1:Wave"))
# 
# #dens_overlay <- mcmc_dens_overlay(m_cluster, pars  = c("b_As1", "b_Wave", "b_As1:Wave"))
# dens_overlay 
# #saveRDS(dens_overlay, here::here("_posts", "mus", "mods", "dens_overlay"))
# 
# dens_overlay <- readRDS( here::here("_posts", "mus", "mods", "dens_overlay"))
# dens_overlay_plot <- dens_overlay + labs(title = "Density overlay of posterior locations by chain")
# dens_overlay_plot


ggsave(
  dens_overlay_plot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "dens_overlay_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


ggsave(
  dens_overlay_plot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "dens_overlay_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# graph use ---------------------------------------------------------------

results0 <-areas_plot + dens_overlay_plot  + 
  plot_annotation(tag_levels = "i") 
#
results0

ggsave(
  results0,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results0.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



results2 <- cluster_plot_title + ( (areas_plot/ dens_overlay_plot ) ) + 
  plot_annotation(tag_levels = "i")  +
plot_layout(ncol = 2, widths = c(2, 1))
results2



results3 <- cluster_plot_title + ( dens_overlay_plot ) + 
  plot_annotation(tag_levels = "i") +
 plot_layout(ncol = 2, widths = c(2, 1))
results3




ggsave(
  results3,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


dens_overlay_plot + bayes_hypothesis + plot_annotation(tags = "i")



ggsave(
  results2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "results2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

ggsave(
  results,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



# save plot


# Too big
# 
# 
# color_scheme_set("brightblue")
# trace_plot <- mcmc_trace(m_cluster, n_warmup = 1000,
#                 facet_args = list(ncol = 2, labeller = label_parsed))
# 

#saveRDS( trace_plot, here::here("_posts", "mus", "mods", "trace_plot"))

trace_plot

ggsave(
  trace_plot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "trace_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


library(patchwork)
areas_plot / dens_overlay_plot + plot_annotation(tag_levels = "i")



#dens_overlaySigma <- mcmc_dens_overlay(m_cluster, variable = c("b_sigma_As0, b_sigma_As1"))
#saveRDS(dens_overlaySigma, here::here("_posts", "mus", "mods", "dens_overlaySigma"))
#dens_overlaySigma

# V2
color_scheme_set("mix-blue-red")
color_scheme_set("brewer-Spectral")

# Too big
# dens_overlayAll2 <- mcmc_dens_overlay(m_cluster)
#saveRDS(dens_overlayAll2, here::here("_posts", "mus", "mods", "dens_overlayAll2"))



# # Trace
# color_scheme_set("brewer-Spectral")
# sigma <- mcmc_trace(x, pars = "sigma")
# 

# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)

# coefficient plot
library(visibly)
library(scico)
coefplot <- visibly::plot_coefficients(m_cluster)
coefplot

# Areas plot
color_scheme_set("brightblue")
areas_plot <- mcmc_areas(
  posterior, 
  pars = c("Wave", "As1", "Wave:As1"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)  +
  labs(
    title = "Posterior distributions",
    subtitle = "with medians and 80% intervals")
  



# sensitivity anlaysis ----------------------------------------------------
# imagine strong time effect


priorS = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  set_prior("constant(0.12)", class = "b", coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)

b_sens_cluster <- brms::brm(
  bform,
  data = ameliadata,
  prior = priorS,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 1,
  future = TRUE,
  # backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_sens_cluster")
)

prior_summary(b_sens)

summary_b_sens_cluster<- summary(b_sens_cluster)
summary(b_sens_cluster)



tab_bsense_cluster<- lazerhawk::brms_SummaryTable(b_sens_cluster, panderize=F)

tab_bsense_cluster %>%
  kable(booktabs = T, "latex", caption =  "Sensitivity for effect of attack on warmth to Muslims: baseline trajectory fixed 2 $\times$ stronger than estimate", digits = 2) %>%
  print()


b_sens_cluster <- plot(conditional_effects(b_sens_cluster,  "Wave:As",  ndraws = 200, spaghetti = T))
#saveRDS(b_sens_cluster, here::here("_posts", "mus", "mods", "b_sens_cluster"))
b_sens_cluster <- readRDS(here::here("_posts", "mus", "mods", "b_sens_cluster"))

#


sens_plot <- b_sens_cluster$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="",
       y= "Muslim Warmth", 
       x = "Years: 2018-2020/21; N = 47948") + 
  scale_color_npg(alpha =.5) + 
  # scale_colour_fivethirtyeight(alpha = .5) + 
  #scale_colour_okabe_ito(alpha =.5) + 
  theme_classic()




# plot sensitivity --------------------------------------------------------


sens_plot_title <-  cluster_plot_st + sens_plot + labs(title = "Sensitivity analysis: predicted marginal means for prejudice by attack condition",
                                    subtitle = "Assuming 2 x baseline acceptance growth rate does not diminish post-attack growth rate")
sens_plot_title

dev.off()

ggsave(
  sens_plot_title,
  path = here::here(here::here("_posts", "mus", "mods")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sens_plot_title.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


    
    
    
    
# 
# # test 2 ------------------------------------------------------------------
# 
# 
# # restard fresh r session
# # do not specify cores
# 
# system.time(
#   m_test  <- brms::brm_multiple(
#     bf(Ys ~ As  *  Wave + (0 + As || Id),
#        sigma ~ 0 + As, set_rescor(rescor = FALSE)),
#     family = gaussian,
#     data = testdata,
#     c(
#       prior(normal(0.2, 0.25), class = b, coef = "As1"),
#       prior(normal(0.05, 0.25), class = b, coef = "Wave"),
#       prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As0",
#         dpar = "sigma"
#       ),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As1",
#         dpar = "sigma"
#       ),
#       prior(student_t(3, 4.1, 1), class = Intercept),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As0",
#         group = "Id"
#       ),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As1",
#         group = "Id"
#       )
#     ),
#     seed = 1234,
#     init = 0,
#     warmup = 1000,
#     iter =  2000,
#     chains = 2
#     #, file = here::here("_posts", "mus", "mods", "m_core.rds")
#   )
# )
# 
# 
# 
# # test 3 ------------------------------------------------------------------
# 
# 
# 
# system.time(
#   m_test3  <- brms::brm_multiple(
#     bf(Ys ~ As  *  Wave + (0 + As || Id),
#        sigma ~ 0 + As, set_rescor(rescor = FALSE)),
#     family = gaussian,
#     data = testdata,
#     c(
#       prior(normal(0.2, 0.25), class = b, coef = "As1"),
#       prior(normal(0.05, 0.25), class = b, coef = "Wave"),
#       prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As0",
#         dpar = "sigma"
#       ),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As1",
#         dpar = "sigma"
#       ),
#       prior(student_t(3, 4.1, 1), class = Intercept),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As0",
#         group = "Id"
#       ),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As1",
#         group = "Id"
#       )
#     ),
#     seed = 1234,
#     init = 0,
#     warmup = 1000,
#     iter =   1500,
#     chains = 2,
#     backend = "cmdstanr"#,
#     #, file = here::here("_posts", "mus", "mods", "m_core.rds")
#   )
# )
# 
# summary(m_test3)
# 
# 
# system.time(
#   m_core  <- brms::brm_multiple(
#     bf(Ys ~ As  *  Wave + (0 + As || Id),
#        sigma ~ 0 + As, set_rescor(rescor = FALSE)),
#     family = gaussian,
#     data = ameliadata,
#     c(
#       prior(normal(0.2, 0.25), class = b, coef = "As1"),
#       prior(normal(0.05, 0.25), class = b, coef = "Wave"),
#       prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As0",
#         dpar = "sigma"
#       ),
#       prior(
#         normal(log(1), 1),
#         class = b,
#         coef = "As1",
#         dpar = "sigma"
#       ),
#       prior(student_t(3, 4.1, 1), class = Intercept),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As0",
#         group = "Id"
#       ),
#       prior(
#         student_t(3, 0, 2.5),
#         class = sd,
#         coef = "As1",
#         group = "Id"
#       )
#     ),
#     seed = 1234,
#     init = 0,
#     warmup = 1000,
#     iter =  2000,
#     chains = 2,
#     future = TRUE
#     #, backend = "cmdstanr"
#     #, file = here::here("_posts", "mus", "mods", "m_core.rds")
#   )
# )
# 
# 
# # Needed to make machine work
# saveRDS(m_core.rds,  file = here::here("_posts", "mus", "mods", "m_core.rds"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #IGNORE BELOW
# 
# 
# # prepare data ------------------------------------------------------------
# 
# i1 <- as.data.frame(imps_bind$imputations$imp[[1]])
# i2 <- as.data.frame(imps_bind$imputations$imp[[2]])
# i3 <- as.data.frame(imps_bind$imputations$imp[[3]])
# i4 <- as.data.frame(imps_bind$imputations$imp[[4]])
# i5 <- as.data.frame(imps_bind$imputations$imp[[5]])
# i6 <- as.data.frame(imps_bind$imputations$imp[[6]])
# i7 <- as.data.frame(imps_bind$imputations$imp[[7]])
# i8 <- as.data.frame(imps_bind$imputations$imp[[8]])
# i9 <- as.data.frame(imps_bind$imputations$imp[[9]])
# i10 <- as.data.frame(imps_bind$imputations$imp[[10]])
# 
# # test data
# library(dplyr)
# ti1 <- imp1 %>% arrange(Id, Wave) %>% slice(1:128)
# ti2 <- imp2 %>% arrange(Id, Wave) %>% slice(1:128)
# ti3 <- imp3 %>% arrange(Id, Wave) %>% slice(1:128)
# ti4 <- imp4 %>% arrange(Id, Wave) %>% slice(1:128)
# ti5 <- imp5 %>% arrange(Id, Wave) %>% slice(1:128)
# ti6 <- imp6 %>% arrange(Id, Wave) %>% slice(1:128)
# ti7 <- imp7 %>% arrange(Id, Wave) %>% slice(1:128)
# ti8 <- imp8 %>% arrange(Id, Wave) %>% slice(1:128)
# ti9 <- imp9 %>% arrange(Id, Wave) %>% slice(1:128)
# ti10 <- imp10 %>% arrange(Id, Wave) %>% slice(1:128)
# 
# testdata <- list(ti1,ti2,ti3,ti4,ti5,ti6,ti7,ti8,ti9,ti10)
# saveRDS(testdata, here::here("_posts", "mus", "mods", "testdata"))
# 
# 
# # testmodel brms ordinary -------------------------------------------------
# prior <- c(
#   prior(normal(.2,.25), class = b, coef = "As1"),
#   prior(normal(0.05,.25), class = b, coef = "Wave"),
#   prior(normal(0,.25),  class= b, coef = "As1:Wave"),
#   prior(normal(log(1),.5),  class= b, coef = "As0", dpar = "sigma"),
#   prior(normal(log(1),.5),  class= b, coef = "As1", dpar = "sigma"),
#   prior(student_t(3,4.15,.5), class = Intercept),
#   prior(student_t(3,0,1), class = "sd", coef = "As0", group = "Id"),
#   prior(student_t(3,0,1), class = "sd", coef = "As1", group = "Id"))
# 
# 
# bmod <-  bf(Ys ~ As  *  Wave + (0 + As||Id),
#             sigma ~ 0 + As , set_rescor(rescor = FALSE))
# 
# 
# 
# 
# # compare with other options ----------------------------------------------
# 
# 
# seed = 1234
# warmup = 500
# iter = 1500
# chains = 2
# 
# system.time(m2 <-brm_multiple(
#   bmod,
#   family = gaussian, 
#   data = testdata,
#   init = 0,
#   prior = prior,
#   seed = seed,
#   warmup = warmup,
#   iter =  iter,
#   chains = chains,
#   future = TRUE
#   #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# )
# ) 
# 
# 
# 
# 
# system.time(
#   m3 <-brm_multiple(
#   bmod,
#   family = gaussian, 
#   data = testdata,
#   init = 0,
#   prior = prior,
#   seed = seed,
#   warmup = warmup,
#   iter =  iter,
#   chains = chains,
#   cores = parallel::detectCores(),
#   backend = "cmdstanr"
#  # , file =  here::here("_posts", "mus", "mods", "test_delete"))
# )
# )
# summary(m3)
# 
# # run model ---------------------------------------------------------------
# 
# ameliadata <- readRDS(here::here("_posts", "mus", "mods", "ameliadata"))
# 
# seed = 1234
# warmup = 1000
# iter = 2000
# chains = 2
# 
# 
# 
# system.time(
#   m_ml <-brm_multiple(
#     bmod,
#     family = gaussian, 
#     data = ameliadata,
#     init = 0,
#     prior = prior,
#     seed = seed,
#     warmup = warmup,
#     iter =  iter,
#     chains = chains,
#     cores = parallel::detectCores(),
#     backend = "cmdstanr"
#     , file =  here::here("_posts", "mus", "mods", "m_ml"))
# )
# 
# summary(m_ml)
# 
# 
# 
# # futures not run ---------------------------------------------------------
# 
# 
# # target model -------------------------------------------------------------------
# 
# 
# system.time( strong_fit  <- brms::brm_multiple( 
#   bf(Ys ~ As  *  Wave + (0 + As||Id),
#      sigma ~ 0 + As , set_rescor(rescor = FALSE)),
#   family = gaussian, 
#   data = ameliadata,
#   c(
#     prior(normal(.2,.25), class = b, coef = "As1"),
#     prior(normal(0.05,.25), class = b, coef = "Wave"),
#     prior(normal(0,.25),  class= b, coef = "As1:Wave"),
#     prior(normal(log(1),.5),  class= b, coef = "As0", dpar = "sigma"),
#     prior(normal(log(1),.5),  class= b, coef = "As1", dpar = "sigma"),
#     prior(student_t(3,4.15,.5), class = Intercept),
#     prior(student_t(3,0,1), class = "sd", coef = "As0", group = "Id"),
#     prior(student_t(3,0,1), class = "sd", coef = "As1", group = "Id")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   #chains = 4,
#   future = TRUE#,
#   # backend = "cmdstanr",
#   # file = here::here("_posts", "mus", "mods", "strong_fit")
# ) )
# 
# 
# 
# 
# # futures set up ----------------------------------------------------------
# 
# # see 
# # https://rpubs.com/mvuorre/brms-parallel
# combine_models(..., mlist = NULL, check_data = TRUE)
# 
# # 
# # plan(
# #   list(
# #     tweak(multisession, workers = 2),
# #     tweak(multisession, workers = 2)
# #   )
# # )
# 
# 
# # plan(multisession,  workers = availableCores())
# # 
# # 
# # 
# # plan(multisession,  workers = 4)
# # 
# # seed = 1234
# # warmup = 200
# # iter = 500
# # chains = 2
# # 
# # run_my_models <- function() {
# #   f1 <-brm(
# #   bmod,
# #   family = gaussian, 
# #   data = ti1,
# #   init = 0,
# #   prior = prior,
# #   seed = seed,
# #   warmup = warmup,
# #   iter =  iter,
# #   chains = chains,
# #   future = TRUE
# #   #, file =  here::here("_posts", "mus", "mods", paste0(name, "f1"))
# #   )
# #   f2 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti2,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #     )
# #   f3 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti3,
# #     init =0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f1"))
# #   )
# #   f4 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti4,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #   )
# #   f5 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti5,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f1"))
# #   )
# #   f6 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti6,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #   )
# #   f7 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti7,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f1"))
# #   )
# #   f8 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti8,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #   )
# #   f9 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti9,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #   )
# #   f10 <-brm(
# #     bmod,
# #     family = gaussian, 
# #     data = ti8,
# #     init = 0,
# #     prior = prior,
# #     seed = seed,
# #     warmup = warmup,
# #     iter =  iter,
# #     chains = chains,
# #     future = TRUE
# #     #, file =  here::here("_posts", "mus", "mods", paste0(name, "f2"))
# #   )
# #   combine_models(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, check_data = FALSE)
# # }
# # 
# # 
# # plan(multisession,  workers = 20)
# # output %<-% run_my_models()
# # output
# # 
# 
# 


# Details
Compiling the C++ model
Fitting imputed model 1

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.096428 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 964.28 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8681.15 seconds (Warm-up)
Chain 1:                3850.11 seconds (Sampling)
Chain 1:                12531.3 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.053072 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 530.72 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8369.52 seconds (Warm-up)
Chain 2:                3745.14 seconds (Sampling)
Chain 2:                12114.7 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 2

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.052935 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 529.35 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8386.25 seconds (Warm-up)
Chain 1:                3726.66 seconds (Sampling)
Chain 1:                12112.9 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.053003 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 530.03 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8916.59 seconds (Warm-up)
Chain 2:                3861.01 seconds (Sampling)
Chain 2:                12777.6 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 3

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.053754 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 537.54 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8717.6 seconds (Warm-up)
Chain 1:                3827.4 seconds (Sampling)
Chain 1:                12545 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.052482 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 524.82 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8169.86 seconds (Warm-up)
Chain 2:                3784.58 seconds (Sampling)
Chain 2:                11954.4 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 4

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.053605 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 536.05 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8669.18 seconds (Warm-up)
Chain 1:                3908.63 seconds (Sampling)
Chain 1:                12577.8 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.055033 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 550.33 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8785.55 seconds (Warm-up)
Chain 2:                3901.01 seconds (Sampling)
Chain 2:                12686.6 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 5

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.052337 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 523.37 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8774.18 seconds (Warm-up)
Chain 1:                3919.15 seconds (Sampling)
Chain 1:                12693.3 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.052881 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 528.81 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8769.52 seconds (Warm-up)
Chain 2:                3862 seconds (Sampling)
Chain 2:                12631.5 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 6

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.053552 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 535.52 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8738.35 seconds (Warm-up)
Chain 1:                3831.1 seconds (Sampling)
Chain 1:                12569.5 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.052848 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 528.48 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8411.69 seconds (Warm-up)
Chain 2:                3704.81 seconds (Sampling)
Chain 2:                12116.5 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 7

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.054662 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 546.62 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8188.6 seconds (Warm-up)
Chain 1:                3700.62 seconds (Sampling)
Chain 1:                11889.2 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.053658 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 536.58 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8218.77 seconds (Warm-up)
Chain 2:                3698.26 seconds (Sampling)
Chain 2:                11917 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 8

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.054359 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 543.59 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8178.44 seconds (Warm-up)
Chain 1:                3666.07 seconds (Sampling)
Chain 1:                11844.5 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.053911 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 539.11 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8328.07 seconds (Warm-up)
Chain 2:                3652.8 seconds (Sampling)
Chain 2:                11980.9 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 9

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.052857 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 528.57 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8076.03 seconds (Warm-up)
Chain 1:                3677.59 seconds (Sampling)
Chain 1:                11753.6 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.052789 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 527.89 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8550.79 seconds (Warm-up)
Chain 2:                3834.66 seconds (Sampling)
Chain 2:                12385.5 seconds (Total)
Chain 2: 
  Start sampling
Fitting imputed model 10

SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
Chain 1: 
  Chain 1: Gradient evaluation took 0.052343 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 523.43 seconds.
Chain 1: Adjust your expectations accordingly!
  Chain 1: 
  Chain 1: 
  Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
  Chain 1:  Elapsed Time: 8203.55 seconds (Warm-up)
Chain 1:                3747.7 seconds (Sampling)
Chain 1:                11951.2 seconds (Total)
Chain 1: 
  
  SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
Chain 2: 
  Chain 2: Gradient evaluation took 0.05248 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 524.8 seconds.
Chain 2: Adjust your expectations accordingly!
  Chain 2: 
  Chain 2: 
  Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 2: 
  Chain 2:  Elapsed Time: 8366.66 seconds (Warm-up)
Chain 2:                3785.48 seconds (Sampling)
Chain 2:                12152.1 seconds (Total)
Chain 2: 
  Start sampling
user     system    elapsed 
241379.401   9114.124 250230.766 
