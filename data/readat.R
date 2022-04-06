library(here)
library(tidyverse)
library(fs)
# find home directory

# relative path from computer (only works for JB's machines)
pull_path <- fs::path_expand("~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5")

#
df<-readRDS(pull_path)

# observe
dplyr::glimpse(df)

# save
saveRDS(df, here::here("data","df"))
