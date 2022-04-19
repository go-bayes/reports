
#functions for pooled parameters 

# read data

msm_10_r_imp1 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp1" ))
msm_10_r_imp2 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp2" ))
msm_10_r_imp3 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp3" ))
msm_10_r_imp4 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp4" ))
msm_10_r_imp5 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp5" ))

m1<- msm_10_r_imp1
m2<- msm_10_r_imp2
m3<- msm_10_r_imp3
m4<- msm_10_r_imp4
m5<- msm_10_r_imp5

m1
m2
m3
m4
m5

### Function for graph 
library(msm)

msm_graph  <- function(x, l_str, u_str ){
  p20 <-
    pmatrix.msm(x, covariates = list(Age = 20), ci = "normal")
  p20.Convert <- p20$estimates[1, 2]
  p20.Convert.L <- p20$L[1, 2]
  # get upper bound
  p20.Convert.U <- p20$U[1, 2]
  con.20 <- c(20, p20.Convert, p20.Convert.L, p20.Convert.U)
  ## 30 year olds
  p30 <-
    pmatrix.msm(x, covariates = list(Age = 30), ci = "normal")
  p30.Convert <- p30$estimates[1, 2]
  # set lower bound
  p30.Convert.L <- p30$L[1, 2]
  # get upper bound
  p30.Convert.U <- p30$U[1, 2]
  #combine
  con.30 <- c(30, p30.Convert, p30.Convert.L, p30.Convert.U)
  ## 40 year old
  p40 <-
    pmatrix.msm(x, covariates = list(Age = 40), ci = "normal")
  # Get Transition to Relid Estimate
  p40.Convert <- p40$estimates[1, 2]
  # set lower bound
  p40.Convert.L <- p40$L[1, 2]
  # get upper bound
  p40.Convert.U <- p40$U[1, 2]
  con.40 <- c(40, p40.Convert, p40.Convert.L, p40.Convert.U)
  ## 50 year old
  p50 <-
    pmatrix.msm(x, covariates = list(Age = 50), ci = "normal")
  # Get Transition to Relid Estimate
  p50.Convert <- p50$estimates[1, 2]
  # set lower bound
  p50.Convert.L <- p50$L[1, 2]
  # get upper bound
  p50.Convert.U <- p50$U[1, 2]
  con.50 <- c(50, p50.Convert, p50.Convert.L, p50.Convert.U)
  ## 60 year olds
  p60 <-
    pmatrix.msm(x, covariates = list(Age = 60), ci = "normal")
  # Get Transition to Relid Estimate
  p60.Convert <- p60$estimates[1, 2]
  # set lower bound
  p60.Convert.L <- p60$L[1, 2]
  # get upper bound
  p60.Convert.U <- p60$U[1, 2]
  con.60 <- c(60, p60.Convert, p60.Convert.L, p60.Convert.U)
  ## 70 year olds
  p70 <-
    pmatrix.msm(x, covariates = list(Age = 70), ci = "normal")
  # Get Transition to Relid Estimate
  p70.Convert <- p70$estimates[1, 2]
  # set lower bound
  p70.Convert.L <- p70$L[1, 2]
  # get upper bound
  p70.Convert.U <- p70$U[1, 2]
  con.70 <- c(70, p70.Convert, p70.Convert.L, p70.Convert.U)
  ### ## 80 year olds
  p80 <-
    pmatrix.msm(x, covariates = list(Age = 80), ci = "normal")
  # Get Transition to Relid Estimate
  p80.Convert <- p80$estimates[1, 2]
  # set lower bound
  p80.Convert.L <- p80$L[1, 2]
  # get upper bound
  p80.Convert.U <- p80$U[1, 2]
  con.80 <- c(80, p80.Convert, p80.Convert.L, p80.Convert.U)
  # ### ## 90 year olds
  p90 <-
    pmatrix.msm(x, covariates = list(Age = 90), ci = "normal")
  # # Get Transition to Relid Estimate
  p90.Convert <- p90$estimates[1, 2]
  # # set lower bound
  p90.Convert.L <- p90$L[1, 2]
  p90.Convert.U <- p90$U[1, 2]
  con.90 <- c(90, p90.Convert, p90.Convert.L, p90.Convert.U)
  # ### create data frame for conversion plot
  conplot <-
    data.frame(rbind(con.20, con.30, con.40, con.50, con.60, con.70, con.80, con.90))
  colnames(conplot) <-
    c("age", "conv_probability", "conv_lower", "conv_upper")
  library(ggplot2)
  #library(wesanderson)
  #wes_palette("Zissou1", type = c("discrete"))
  #library("ggsci")
  
  ##### CREAT PLOT FOR DECONVERSION
  ## 20 year olds
  p20 <-
    pmatrix.msm(x, covariates = list(Age = 20), ci = "normal")
  # Get Transition to Relid Estimate
  p20.decon <- p20$estimates[2, 1]
  p20.decon.L <- p20$L[2, 1]
  p20.decon.U <- p20$U[2, 1]
  decon.20 <- c(20, p20.decon, p20.decon.L, p20.decon.U)
  p30 <-
    pmatrix.msm(x, covariates = list(Age = 30), ci = "normal")
  p30.decon <- p30$estimates[2, 1]
  p30.decon.L <- p30$L[2, 1]
  p30.decon.U <- p30$U[2, 1]
  decon.30 <- c(30, p30.decon, p30.decon.L, p30.decon.U)
  p40 <-
    pmatrix.msm(x, covariates = list(Age = 40), ci = "normal")
  p40.decon <- p40$estimates[2, 1]
  p40.decon.L <- p40$L[2, 1]
  p40.decon.U <- p40$U[2, 1]
  decon.40 <- c(40, p40.decon, p40.decon.L, p40.decon.U)
  p50 <-
    pmatrix.msm(x, covariates = list(Age = 50), ci = "normal")
  p50.decon <- p50$estimates[2, 1]
  p50.decon.L <- p50$L[2, 1]
  p50.decon.U <- p50$U[2, 1]
  decon.50 <- c(50, p50.decon, p50.decon.L, p50.decon.U)
  p60 <-
    pmatrix.msm(x, covariates = list(Age = 60), ci = "normal")
  p60.decon <- p60$estimates[2, 1]
  p60.decon.L <- p60$L[2, 1]
  p60.decon.U <- p60$U[2, 1]
  decon.60 <- c(60, p60.decon, p60.decon.L, p60.decon.U)
  p70 <-
    pmatrix.msm(x, covariates = list(Age = 70), ci = "normal")
  p70.decon <- p70$estimates[2, 1]
  p70.decon.L <- p70$L[2, 1]
  p70.decon.U <- p70$U[2, 1]
  decon.70 <- c(70, p70.decon, p70.decon.L, p70.decon.U)
  p80 <-
    pmatrix.msm(x, covariates = list(Age = 80), ci = "normal")
  p80.decon <- p80$estimates[2, 1]
  p80.decon.L <- p80$L[2, 1]
  p80.decon.U <- p80$U[2, 1]
  decon.80 <- c(80, p80.decon, p80.decon.L, p80.decon.U)
  p90 <-
    pmatrix.msm(x, covariates = list(Age = 90), ci = "normal")
  p90.decon <- p90$estimates[2, 1]
  p90.decon.L <- p90$L[2, 1]
  p90.decon.U <- p90$U[2, 1]
  decon.90 <- c(90, p90.decon, p90.decon.L, p90.decon.U)
  
  ### create data frame for conversion plot
  deconplot <-
    data.frame(rbind(
      decon.20,
      decon.30,
      decon.40,
      decon.50,
      decon.60,
      decon.70,
      decon.80,
      decon.90
    ))
  colnames(deconplot) <-
    c("age", "decon_probability", "decon_lower", "decon_upper")
  #grid.arrange(plot.con,plot.decon, ncol=2)
  ### SUPERIMPOSE
  deconplot$Group <- as.factor(rep(l_str, nrow(deconplot)))
  conplot$Group <- as.factor(rep(u_str, nrow(conplot)))
  deconplot2 <- deconplot %>%
    dplyr::mutate(
      conv_probability = decon_probability,
      conv_lower = decon_lower,
      conv_upper = decon_upper
    ) %>%
    dplyr::select(-c(decon_probability, decon_upper, decon_lower))
  
  comboplot <- rbind(conplot, deconplot2)

}

# religious works 
r1 <- msm_graph(msm_10_r_imp1, "Disaffiliation", "Affiliation")
r2 <- msm_graph(msm_10_r_imp2, "Disaffiliation", "Affiliation")
r3 <- msm_graph(msm_10_r_imp3, "Disaffiliation", "Affiliation")
r4 <- msm_graph(msm_10_r_imp4, "Disaffiliation", "Affiliation")
r5 <- msm_graph(msm_10_r_imp5, "Disaffiliation", "Affiliation")


# God 
g1 <- msm_graph(msm_10_g_imp1, "Disbelief", "Belief")
g2 <- msm_graph(msm_10_g_imp2, "Disbelief", "Belief")
g3 <- msm_graph(msm_10_g_imp3, "Disbelief", "Belief")
g4 <- msm_graph(msm_10_g_imp4, "Disbelief", "Belief")
g5 <- msm_graph(msm_10_g_imp5, "Disbelief", "Belief")



# Spirit
s1 <- msm_graph(msm_10_s_imp1, "Disbelief", "Belief")
s2 <- msm_graph(msm_10_s_imp2, "Disbelief", "Belief")
s3 <- msm_graph(msm_10_s_imp3, "Disbelief", "Belief")
s4 <- msm_graph(msm_10_s_imp4, "Disbelief", "Belief")
s5 <- msm_graph(msm_10_s_imp5, "Disbelief", "Belief")


# manifest

# religious works 
mr1 <- msm_graph(msm_10_r_MAN_imp1, "Disaffiliation", "Affiliation")
mr2 <- msm_graph(msm_10_r_MAN_imp2, "Disaffiliation", "Affiliation")
mr3 <- msm_graph(msm_10_r_MAN_imp3, "Disaffiliation", "Affiliation")
mr4 <- msm_graph(msm_10_r_MAN_imp4, "Disaffiliation", "Affiliation")
mr5 <- msm_graph(msm_10_r_MAN_imp5, "Disaffiliation", "Affiliation")


# God 
mg1 <- msm_graph(msm_10_g_MAN_imp1, "Disbelief", "Belief")
mg2 <- msm_graph(msm_10_g_MAN_imp2, "Disbelief", "Belief")
mg3 <- msm_graph(msm_10_g_MAN_imp3, "Disbelief", "Belief")
mg4 <- msm_graph(msm_10_g_MAN_imp4, "Disbelief", "Belief")
mg5 <- msm_graph(msm_10_g_MAN_imp5, "Disbelief", "Belief")



# Spirit
ms1 <- msm_graph(msm_10_s_MAN_imp1, "Disbelief", "Belief")
ms2 <- msm_graph(msm_10_s_MAN_imp2, "Disbelief", "Belief")
ms3 <- msm_graph(msm_10_s_MAN_imp3, "Disbelief", "Belief")
ms4 <- msm_graph(msm_10_s_MAN_imp4, "Disbelief", "Belief")
ms5 <- msm_graph(msm_10_s_MAN_imp5, "Disbelief", "Belief")



# 
# msm_pool_mean <- function(df1, df2) {
#   out <- cbind(df1$conv_probability, df2$conv_probability)
#   col_m <- data.frame(sapply(out, mean))
#   colnames(col_m) <-  "conv_probability_m"
#   col_m
# }
# 
# 
# msm_pool_VB <- function(df1, df2) {
#   out <- cbind(df1$conv_probability, df2$conv_probability)
#   col_m <- data.frame(rowMeans(out))
#   colnames(col_m) <-  "conv_probability_m"
#   col_m
#   v_b = as.data.frame( 1/(5-1) * ((df1$conv_probability - col_m) ^ 2) + ((df2$conv_probability - col_m) ^ 2))
#   v_b
# }
# 
# 
# msm_pool_VW <- function(df1,df2){
#   se1 <- as.data.frame((df1$conv_upper - df1$conv_lower)/3.92)
#   colnames(se1) <- "se1"
#   se2 <- as.data.frame((df2$conv_upper - df2$conv_lower)/3.92)
#   colnames(se2) <- "se2"
#   out = cbind( se1 , se2 ) 
#   v_w <- as.data.frame( (out$se1^2 + out$se2^2)/2) 
#   colnames(v_w) <- "v_w"
#   v_w
# }
# 
# test2
# nrow(test2)
# msm_pool_VW(test,test2)
# msm_pool_VB(test,test2)

# This isn't quite right, doesn't calculate tscore correctly

# msm_se_pooled<- function(df1,df2, df3, df4, df5){
#   out <- cbind(df1$conv_probability, df2$conv_probability, df3$conv_probability, df4$conv_probability,df5$conv_probability)
#   age <- df1$age
#   Group <- df1$Group
#   col_m <- data.frame(rowMeans(out))
#   colnames(col_m) <-  "conv_probability_m"
#   col_m
#   var1 = ((df1$conv_probability - col_m) ^ 2)
#   var2 = ((df2$conv_probability - col_m) ^ 2)
#   var3 = ((df3$conv_probability - col_m) ^ 2)
#   var4 = ((df4$conv_probability - col_m) ^ 2)
#   var5 = ((df5$conv_probability - col_m) ^ 2)
#   #outest = cbind(var1, var2,var3,var4, var5)
#   #outest
#   v_b = as.data.frame( 1/(5-1) * ( var1 + var2 + var3 + var4 + var5))
#   v_b
#   se1 <- as.data.frame((df1$conv_upper - df1$conv_lower)/3.92)
#   colnames(se1) <- "se1"
#   se2 <- as.data.frame((df2$conv_upper - df2$conv_lower)/3.92)
#   colnames(se2) <- "se2"
#   se3 <- as.data.frame((df3$conv_upper - df3$conv_lower)/3.92)
#   colnames(se3) <- "se3"
#   se4 <- as.data.frame((df4$conv_upper - df4$conv_lower)/3.92)
#   colnames(se4) <- "se4"
#   se5 <- as.data.frame((df5$conv_upper - df5$conv_lower)/3.92)
#   colnames(se5) <- "se5"
#   out2 = cbind( se1 , se2, se3 , se4, se5 )
#   v_w <- as.data.frame( (out2$se1^2 + out2$se2^2 + out2$se3^2 + out2$se4^2 + out2$se5^2) /(5-1))
#   colnames(v_w) <- "v_w"
#   v_w
#   v_total = as.data.frame(v_w + v_b + v_b/5)
#   colnames(v_total) <- "v_total"
#   se_pooled = sqrt(v_total)
#   colnames(se_pooled) <- "se_pooled"
#   se_pooled
#   pooled_ci=   se_pooled * 1.96 # (col_m  + (1.96 * (se_pooled/66.64)))
#   pooled_upper_ci =  col_m + pooled_ci
#   pooled_lower_ci =   col_m - pooled_ci
#   pool_coef = as.data.frame(cbind(col_m, pooled_lower_ci, pooled_upper_ci, se_pooled, pooled_ci))
#   colnames(pool_coef) <- c("pooled_mean","pooled_lower_ci", "pooled_upper_ci", "se_pooled", "pooled_ci")
#   pool_coef
#   pooled_out <- as.data.frame(cbind(age, Group, pool_coef))
#   pooled_out
# }


## https://bookdown.org/mwheymans/bookmi/measures-of-missing-data-information.html#eq:lambda
## RIV = Relative increase in variance  = LAMDA 
# lambda = ( (v_b + (v_b/m) )/ v_total)
# riv = lambda = ( (v_b + (v_b/m) )/ v_w)
# df_old = (m -1 ) * (1 + (1/riv^2))
# df_observed  = ( (((n-k) + 1)/ ((n-k) + 3) ) * ((n-k) * (1- lambda)) )
# df_adjusted = ( (df_old * df_adjusted ) / (df_old + df_adjusted)  )
# alpha = 0.05
# t_score = qt( p=alpha/2, df= df_adjusted,lower.tail=F )
# margin_error = t_score * se_pooled
# 

#This correctly calculates ci's


## function for saving 

save_here <- function(df, name) {
  x = df
  saveRDS( x,  here::here("_posts", "religious_simpsons_paradox",  "mods", paste0(name, '')))
}


# function for reading 

read_here <- function(name) {
  df = readRDS(here::here("_posts", "religious_simpsons_paradox", "mods", paste0(name, '')))
  df
}


msm_se_pooled_ci <- function(df1,df2, df3, df4, df5){
  m = 5
  n = 4441
  k = 1
  alpha = .05
  out <- cbind(df1$conv_probability, df2$conv_probability, df3$conv_probability, df4$conv_probability,df5$conv_probability)
  age <- df1$age
  Group <- df1$Group
  col_m <- data.frame(rowMeans(out))
  colnames(col_m) <-  "conv_probability_m"
  var1 = ((df1$conv_probability - col_m) ^ 2)
  var2 = ((df2$conv_probability - col_m) ^ 2)
  var3 = ((df3$conv_probability - col_m) ^ 2)
  var4 = ((df4$conv_probability - col_m) ^ 2)
  var5 = ((df5$conv_probability - col_m) ^ 2)
  v_b = as.data.frame( 1/(m-1) * ( var1 + var2 + var3 + var4 + var5)) # var between
  se1 <- as.data.frame((df1$conv_upper - df1$conv_lower)/3.92)
  colnames(se1) <- "se1"
  se2 <- as.data.frame((df2$conv_upper - df2$conv_lower)/3.92)
  colnames(se2) <- "se2"
  se3 <- as.data.frame((df3$conv_upper - df3$conv_lower)/3.92)
  colnames(se3) <- "se3"
  se4 <- as.data.frame((df4$conv_upper - df4$conv_lower)/3.92)
  colnames(se4) <- "se4"
  se5 <- as.data.frame((df5$conv_upper - df5$conv_lower)/3.92)
  colnames(se5) <- "se5"
  out2 = cbind( se1 , se2, se3 , se4, se5 )
  v_w <- as.data.frame( (out2$se1^2 + out2$se2^2 + out2$se3^2 + out2$se4^2 + out2$se5^2) /(m))# Var withing
  colnames(v_w) <- "v_w"
  v_total = as.data.frame(v_w + v_b + v_b/m) # var total
  colnames(v_total) <- "v_total"
  se_pooled = sqrt(v_total)
  colnames(se_pooled) <- "se_pooled"
  #pooled_ci=   se_pooled * 1.96 # (col_m  + (1.96 * (se_pooled/66.64))) # not used 
  lambda = ( (v_b + (v_b/m) ) / v_total)
  riv = ( (v_b + (v_b/m) )/ v_w )
  df_old = ( (m - 1 ) * (1 + (1/(riv^2)) ) )
  df_observed  = ( ( ( (n-k) + 1 )/ ( (n-k) + 3) ) * ((n-k) * (1- lambda) ) )
  df_adjusted = ( (df_old * df_observed ) / (df_old + df_observed)  )
  colnames(df_adjusted) <- "df_adjusted"
  df_adjusted
  alpha = 0.05
  t_score = as.data.frame( qt( p = alpha/2, df = df_adjusted$df_adjusted, lower.tail = F ) )
  colnames(t_score) <- "t_score"
  margin_error = as.data.frame ( t_score * se_pooled )
  colnames(margin_error) <- "margin_error"
  pool_coef = as.data.frame(cbind(col_m, se_pooled, margin_error))
  colnames(pool_coef) <- c("pooled_mean", "se_pooled", "margin_error")
  pooled_out <- as.data.frame(cbind(age, Group, pool_coef))
  pooled_out
}





r_pool_plot <- msm_se_pooled_ci(r1,r2,r3,r4,r5)

r_pool_plot_man <- msm_se_pooled_ci(mr1,mr2,mr3,mr4,mr5)

g_pool_plot <- msm_se_pooled_ci(g1,g2,g3,g4,g5)

g_pool_plot_man <- msm_se_pooled_ci(mg1,mg2,mg3,mg4,mg5)

s_pool_plot <- msm_se_pooled_ci(s1,s2,s3,s4,s5)

s_pool_plot_man <- msm_se_pooled_ci(ms1,ms2,ms3,ms4,ms5)



markov_hidden <- function(df, outcome ) {
# matrix for msms model on these data sets
  q_mat <- rbind(c(.9, .1),c(.1, .9))
  #intial values 
  crude_inits <-
    msm::crudeinits.msm(outcome  ~ yearW, Id, # Id = identifier
                        data = df,
                        qmatrix = q_mat)
  out  <- msm(
    outcome ~ yearW,
    Id,
    data = a_imp_5_r,
    covariates =  ~ Age,
    qmatrix = crude_inits,
    ematrix = rbind(c(.1, .1), c(.1, .1)), 
    est.initprobs = TRUE,
    exacttimes = TRUE,
    gen.inits = TRUE
  )
}



combine_imp_r <- function(amelia_obj, m) {
  out  <- list()
  model <- list()
  graph <- list()
  name1 <- "Disbelief"
  name2 <- "Belief"

  q_mat <- rbind(c(.9, .1),c(.1, .9))
  for (i in 1:m) {
    out[[i]] <- amelia_obj$imputations[[i]]
    out[[i]] %>%
      dplyr::arrange(Id, yearW)
  }
  #intial values 
  crude_inits <-
    msm::crudeinits.msm(Religious1  ~ yearW, Id, # Id = identifier
                        data = out[[i]],
                        qmatrix = q_mat)
  for (i in 1:m){
  model[[i]]  <- msm(
    Religious1 ~ yearW,
    Id,
    data = out[[i]],
    covariates =  ~ Age,
    qmatrix = crude_inits,
    # ematrix = rbind(c(.1, .1), c(.1, .1)), 
    est.initprobs = TRUE,
    exacttimes = TRUE,
    gen.inits = TRUE
  )
  }
  return(model)
}




religion_man  <- combine_imp_r(dat_all_10_r, 20)

for i in 1:20 {msm_graph()
religion_man[[1]],
religion_man[[2]],
religion_man[[3]],
religion_man[[4]],
religion_man[[5]],
religion_man[[6]],
religion_man[[7]],
religion_man[[8]],
religion_man[[9]],
religion_man[[10]],
religion_man[[11]],
religion_man[[12]],
religion_man[[13]],
religion_man[[14]],
religion_man[[15]],
religion_man[[16]],



msm_se_pooled_ci(religion_man)

