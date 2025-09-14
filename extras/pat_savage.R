# PAT 

library("scholar") # citations
library("tidyverse") # wrangling
library("ggplot2") # plots
library("patchwork") # plots
library("brms") # regression model
library("splines")
library("here")

# pats google id, from google scholar
pat <- 'UCIt79UAAAAJ&hl'
# get profle
# l <- get_profile(pat)
ct <- get_citation_history(pat) %>%
  filter(year != 2021) # to avoid bias from incomplete year
ct
# rough model of pats's annual increase in citations


# get profle
# l <- get_profile(shaver)
ct <- get_citation_history(pat) %>%
  filter(year != 2022) # to avoid bias from incomplete year
ct
# rough model of John's annual increase in citations


ggplot(ct, aes(year, cites)) + geom_line() + geom_point() + theme_classic() + ylab("Pat's Citation Counts")


m2 <- glm(cites ~ year, data = ct, family = "poisson")
perc = m2$coefficients["year"]
print(sprintf('Annual increase for Pat Savage %f', perc))

# regression model for citations (non-linear)
m1 <- brm(cites ~ bs(year),
          data = ct, 
          family = "poisson")

# one method for plotting
plot(ggeffects::ggpredict(m1, terms = "year"), add.data = TRUE) + theme_classic()

summary(m1)
# preferred method
pl<-conditional_effects(
  m1,
  prob = 0.9,
  spaghetti = TRUE,
  nsamples = 100,
  points = TRUE
) 

plot(pl, plot = FALSE)[[1]] + theme_classic() + labs(title = "Annual increase for Pat Savage's citations indicates promise")


Predicted citations


#new data
year <- data.frame(year = seq(from = 2015, to = 2030, by = .1))
# predict citations from brms model
predicted <- data.frame(predict(
  m1,
  newdata = year,
  summary = TRUE,
  robust = TRUE,
  probs = c(0.1, 0.9)
))
newdata <- data.frame(year, predicted)
newdata

# graph 
library("ggplot2")
predplot2 <-
  ggplot2::ggplot(data = newdata, aes(x = year, y = Estimate)) + geom_line(colour = "cadetblue")  +
  ggplot2::geom_errorbar(data = newdata, aes(ymin = Q10,
                                             ymax = Q90),
                         width = .1,  position = "identity") +
  theme_classic()  +
  xlab("Years") +
  ylab("Predicted Citations") +
  labs(title = "Bayesian model predicting citations for Pat: 2021-2031 (90% confidence intervals)")
predplot2
# equivalent to:
# plot(ggeffects::ggpredict(m1, terms = "year[2021:2031]", ppd = TRUE))

# Citation plot
cites1<- ggplot(ct, aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
  geom_point(size=3, color='firebrick') + theme_classic() + labs(title = "Pat's citations by year")


# co-author network
coauthor_network <- get_coauthors('UCIt79UAAAAJ&hl', n_coauthors = 7)
co_authors <- plot_coauthors(coauthor_network)
# predicted h index 

pj <- predict_h_index(pat)
pj

# plot of predicted h index 
preditedh <-
  ggplot(pj, aes(years_ahead, h_index)) + geom_line() + geom_point() +
  theme(legend.position = c(.2, .8)) + theme_classic() + xlab("Years 2021-2031") + ylab("Predicted H-index") + labs(title = "Pat Savage's  H-index Forcast (method: Acuna et al 2012)")
library(patchwork)
(cites1 + preditedh  )  / ( co_authors) + plot_annotation(tag_levels = 'a') 
```


```{r}
publications <- scholar::get_publications(pat) 
publications$journal
ifdata <- scholar::get_impactfactor(publications$journal) 
ifdata %>% dplyr::arrange(desc(ImpactFactor) ) %>% tidyr::drop_na()
knitr::kable(iftable)
```


End. 



