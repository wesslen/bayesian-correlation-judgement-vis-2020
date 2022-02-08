library(plyr)
library(lme4)
library(tidyverse)
library(sjPlot)
library(rjson)
library(glmmTMB)
theme_set(theme_sjplot())

result <- fromJSON(file = "data/bootstrap_results.json")

# Convert JSON file to a data frame.
uncertainties <- ldply(result, data.frame)
uncertainties <- unique(uncertainties[c("vars","pearsonr","uncertainty_lower","uncertainty_upper")])

df <- read.csv(file="data/processed_data.csv")

df_exclude <- df %>% 
              filter(exclude == 0) 

pop_corrs <- unique(df_exclude[c("vars", "pop_corr")])

df_exclude$visGroup <- factor(df_exclude$visGroup, c("scatter","line","band","hop"))
df_exclude <- rename(df_exclude, visTreatment = visGroup)
levels(df_exclude$visTreatment ) <- c("Scatter","Line","Cone","HOPs")
df_exclude$with_uncertainty <- lapply(df_exclude$visTreatment,function(x){
  if (x=="Scatter" | x== "Line") {
    return ("no")
  } else {
    return ("yes")
  }
})

df_exclude <- left_join(df_exclude, uncertainties, by="vars")
df_exclude$true_uncertainty <- abs(df_exclude$uncertainty_upper-df_exclude$uncertainty_lower)
df_exclude$size_of_belief_change <- abs(df_exclude$diff_belief)
df_exclude$size_of_uncertainty_change <-abs(df_exclude$diff_uncertainty)
df_exclude$population_correlation_abs <- factor(abs(df_exclude$pop_corr))
df_exclude$sample_correlation_abs <- abs(df_exclude$pearsonr)
df_exclude$prior_belief_abs_error <- abs(df_exclude$prior_belief - df_exclude$pearsonr)
df_exclude$posterior_belief_abs_error <- abs(df_exclude$post_belief - df_exclude$pearsonr)
df_exclude$posterior_error <- df_exclude$post_belief - df_exclude$pearsonr


# some NA responses
df_exclude = df_exclude[!is.na(df_exclude$diff_belief),]


summary(df_exclude)


# Posterior error -----

# hist posterior error by condition and population correlation
ggplot(data=df_exclude) +
  geom_histogram(aes(x=posterior_error)) +
  facet_grid(visTreatment ~ pop_corr, scales="free_y")

# hist absolute posterior error by condition and pop corr
ggplot(data=df_exclude) +
  geom_histogram(aes(x=posterior_belief_abs_error)) +
  facet_grid(visTreatment ~ pop_corr, scales="free_y")

# hist absolute posterior error by condition and *absolute* pop corr
ggplot(data=df_exclude) +
  geom_histogram(aes(x=posterior_belief_abs_error)) +
  facet_grid(visTreatment ~ population_correlation_abs, scales="free_y")


df_exclude$posterior_belief_abs_error_bnd = df_exclude$posterior_belief_abs_error/2

ggplot(data=df_exclude) +
  geom_histogram(aes(x=posterior_belief_abs_error_bnd)) +
  facet_grid(visTreatment ~ population_correlation_abs, scales="free_y")


## mixed effects models
m = glmmTMB(posterior_belief_abs_error_bnd ~ 
              visTreatment + population_correlation_abs + 
              (1|usertoken) + (1|vars), 
            df_exclude, 
            family=list(family="beta", link="logit"))

# This gives list of coefficients and associated effects
summary(m)
confint(m)

plot_model(m, vline.color = "red",show.values = TRUE, value.offset = .3)

#(Line:β=−.17[−.29,−.05],z=−2.74,p=.006; Cone:β=−.29[−.42,−.16],z=−4.37,p<.001;HOP:β=−.23[−.35,−.11],z=−3.77,p<.001).  Relative to theρ=0variable sets, absolute error was higher forρ=±0.4variablesets  (β=.23[.13,.34],z=4.39,p<.001)  but  did  not  differ  fromρ=±0.9 variable sets (β=.04[−.06,.15],z=.83,p=.41)

# Belief change -----

# hist belief change by condition and population correlation
ggplot(data=df_exclude) +
  geom_histogram(aes(x=diff_belief)) +
  facet_grid(visTreatment ~ pop_corr, scales="free_y")

ggplot(data=df_exclude) +
  geom_histogram(aes(x=size_of_belief_change)) +
  facet_grid(visTreatment ~ population_correlation_abs, scales="free_y")

# size_of_belief_change_bnd = diffBeliefAbs
# 


ggplot(df_exclude, aes(x = size_of_belief_change)) +
  geom_histogram()


# models

# linear mixed effects

m = lmer(size_of_belief_change ~  visTreatment + population_correlation_abs +   (1|usertoken) + (1|vars),df_exclude)
m1 = lmer(diff_uncertainty ~ visTreatment + population_correlation_abs + (1|usertoken) + (1|vars),df_exclude)

a <- plot_model(m,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est" , show.intercept = TRUE ) +
  #scale_y_continuous(breaks=seq(-.75,0.75,.25)) +
  theme(axis.text.y = element_text(size = 8),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Absolute Belief Difference", title = "") +
  ylim(-0.75, 0.75)

b <- plot_model(m1, vline.color = "grey",show.values = TRUE, value.offset = .4, value.size = 3, show.intercept = TRUE ) +
  theme(axis.text.y=element_blank(),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Uncertainty Difference", title = "") +
  ylim(-.5,.5) 

# final plots
library(cowplot)

plot_grid(a,
          b,
          label_x = -0.2,
          ncol = 2,
          rel_widths = c(6.5, 4)) %>%
  ggsave(width = 6, height = 2.5, filename = "images/study2-me.pdf")