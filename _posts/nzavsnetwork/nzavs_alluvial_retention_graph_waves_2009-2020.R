# R code for NZAVS Alluvial graphs
# Joseph Bulbulia
# Date: 10 Aug 2021
# contact: joseph.bulbulia@gmail.com


# load packages
library("tidyverse") # data wrangling
library("haven")
library("here") # setting directories
library("ggplot2") # graph
library("ggthemes") # themes
library("table1") # tables
library("ggalluvial") # alluvial graphs

# read data. note that Wave 2011.5 = T035
sampling_data <-
  haven::read_sav(here::here("data", "GraphData2.sav")) # Chris's sample frame data (Aug 2021)

# check
# table(sampling_data$SampleOriginYear)

# remove SPSS labels
sampling_data <- haven::zap_formats(sampling_data)
sampling_data <- haven::zap_label(sampling_data)
sampling_data <- haven::zap_widths(sampling_data)
sampling_data <- haven::zap_labels(sampling_data)

# rename Id variable
sampling_data <- sampling_data %>%
  rename(Id = Questionnaire.Num)

# check
#str(sampling_data)

# checks
# str(sampling_data)
# table(sampling_data$SampleOriginYear)

## transform data into long form, required for ggalluvial, and rename the waves
inner_join(
  sampling_data  %>%
    dplyr::select(c(
      Id,
      c(
        'T01',
        "T02",
        'T03',
        "T035",
        "T04",
        'T05',
        "T06",
        'T07',
        "T08",
        "T09",
        "T10",
        "T11",
        "T12"
      )
    )) %>%      #~~~~ Left side = date component ~~~~~~~~
    gather(Wave, YearMeasured, -Id) %>%  #~ long form = 1 row per prod per seq   ~
    mutate(
      Wave = recode_factor(
        Wave,
        T01 = "2009",
        T02 = "2010",
        T03 = "2011",
        T035 = "2011.5",
        T04 = "2012",
        T05 = "2013",
        T06 = "2014",
        T07 = "2015",
        T08 = "2016",
        T09 = "2017",
        T10 = "2018",
        T11 = "2019",
        T12 = "2020"
      )
    ) %>%
    arrange(Id),
  sampling_data %>%
    dplyr::select(c(Id, starts_with('SampleOriginYear'))) %>%
    arrange(Id),
) -> sdf

# check data
# head(sdf)

# define sample origin year
sdf <- sdf %>%
  mutate(SampleOriginYear = SampleOriginYear + 2008)

#  Make sample frame into a factor
sdf$SampleOriginYear <- as.factor(sdf$SampleOriginYear)

# Check
# table(sdf$SampleOriginYear)

## Created levels for "missing", "not yet measured" and "deceased"
## then filter "not yet measured"

sdf.0 <- sdf %>%
  mutate(Wave = as.numeric(as.character(Wave))) %>%
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(first = {
    YearMeasured == 1
  } %>% {
    . * !duplicated(.)
  }) %>%
  mutate(
    value_tmp = if_else(first == 1, Wave, NA_real_),
    firstwave  = mean(value_tmp, na.rm = TRUE) # this is a hack, but works
  ) %>%
  mutate(state  = ifelse(
    YearMeasured == -1,
    "deceased",
    ifelse(
      YearMeasured == 0 & Wave < firstwave,
      "notyetmeasured",
      ifelse(YearMeasured == 0 &
               Wave > firstwave, "missing",
             "measured")
    )
  )) %>%
  dplyr::mutate(Wave = as.factor(Wave)) %>% # return Wave to a factor
  dplyr::select(Wave, Id, state, YearMeasured, SampleOriginYear) %>%
  dplyr::filter(state != "notyetmeasured") %>%
  droplevels() %>%
  arrange(Id, Wave)

# Check
# table(sdf.0$SampleOriginYear)

# This is to create a "recovered" state. Not used but could be useful for modelling recovery:

sdf.01 <- sdf.0 %>%
  group_by(Id) %>%
  mutate(lag_state = dplyr::lag(state, n = 1, default = "init"))

sdf.1 <- sdf.01 %>%
  group_by(Id, Wave) %>%
  mutate(recovered = ifelse(lag_state == "missing" &
                              state == "measured", "recovered", state))


# We need to back fill the missing values for our alluvial graph.
# This is OK because a participants original sampling frame does not change.

ssdf0 <- sdf.1 %>%  # Fill missing values for Sample.Frame
  arrange(Id) %>%
  group_by(Id) %>%
  fill(SampleOriginYear)  %>%
  ungroup()

# now we create the "state_frame" variable
tf <- ssdf0 %>%
  mutate(SampleOriginYear = as.character(SampleOriginYear))

ssdf <- tf %>%
  mutate(state_frame = ifelse(
    state == "missing",
    "missing",
    ifelse(state == "deceased", "deceased",
           SampleOriginYear)
  ))
ssdf.0 <- ssdf %>%
  mutate(state_frame = factor(state_frame))

# check
# levels(ssdf.0$state_frame)

# next we order the levels of the factor to create a pretty graph
ssdf.01 <- ssdf.0 %>%
  mutate(state_frame = forcats::fct_relevel(
    state_frame,
    c(
      "2009",
      "2010",
      "2011",
      "2011.5",
      "2012",
      "2013",
      "2014",
      "2015",
      "2016",
      "2017",
      "2018",
      "2019",
      "2020",
      "missing",
      "deceased"
    )
  )) %>%
  rename(Recovered = recovered) # RENAME


# Tables (uncomment to run
# table1::table1( ~ state_frame | Wave, data = ssdf.01, overall = FALSE)
# table1::table1( ~ Recovered | Wave, data = ssdf.01, overall = FALSE)

# prepare data
datsSF <- ssdf.01 %>%
  group_by(Wave, state_frame, Id) %>%
  summarise(n = n())

# check data
# datsSF

# Graph

p <- ggplot(
  datsSF,
  aes(
    x = Wave,
    stratum = state_frame,
    alluvium = Id,
    y = n,
    fill = state_frame,
    label = state_frame
  )
) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  guides(color = guide_legend(override.aes = list(size = 20))) +
  scale_x_discrete(position = 'top', expand = c(0, 0)) +
  scale_y_reverse(
    expand = c(0.001, 0.001),
    minor_breaks = seq(0, 70000, by = 1000),
    breaks = seq(0, 70000, by = 5000)
  ) +
  scale_fill_viridis_d(option = "turbo") +
  ggtitle("NZAVS alluvial retention graph: waves 2009-2020")  +
  theme_bw() +   # other theme options below.
  # theme_wsj() +
  # theme_radar() +
  # theme_solarized() +
  # theme_abyss() +
  # theme_economist() +
  # theme_economist_white() +
  # theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = "top") +
  theme(legend.title = element_blank())  # remove legend title

# graph
p

# to save file in your directory at correct resolution, uncomment below
# file <- here("figs", paste("alluvial_highres_turbo_BW", ".png", sep = ""))
#
# ggsave(
#   file,
#   device = "png",
#   scale = 1,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300,
#   limitsize = TRUE
# )


## Facet graph
# Facet graph for retention by sample frame.

# prepare data
ssdf.01
datsSS <- ssdf.01 %>%
  group_by(Wave, state, SampleOriginYear, Id) %>%
  summarise(n = n())

# Graph
ggplot(datsSS,
       aes(
         x = Wave,
         stratum = state,
         alluvium = Id,
         y = n,
         fill = state,
         label = state
       )) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme(legend.position = "none") +
  scale_x_discrete(position = 'top') +
  scale_y_reverse(
    expand = c(0.001, 0.001),
    minor_breaks = seq(0, 70000, by = 1000),
    breaks = seq(0, 70000, by = 1000)
  ) +
  facet_grid(SampleOriginYear ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_fill_viridis_d(option = "viridis") +
  # scale_fill_viridis_d(option="turbo") + # another option
  ggtitle("NZAVS alluvial retention graph: waves 2009-2020",
          subtitle  = "Facets are sample frames")  + theme_bw() +
  theme(legend.title = element_blank())  # remove legend title


# uncomment to save file
# file <- here("figs", paste("alluvial_facets_turbo_highres_VIRIDIS_3", ".png", sep = ""))
#
# ggsave(
#   file,
#   device = "png",
# #  path = here::here("figs", paste("alluvial_highres", ".png", sep = "")),
#   scale = 1,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300,
#   limitsize = TRUE
# )

