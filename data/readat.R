library(here)
library(tidyverse)

df<-readRDS("/Users/joseph/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5")

glimpse(df)

saveRDS(df, here::here("data","df"))
