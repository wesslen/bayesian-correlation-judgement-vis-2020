library(plyr)
library(statsr)
library(lme4)
library(tidyverse)
library(sjPlot)
library(sjlabelled)
#library(rstan)
#library(rstanarm)
library(sjmisc)
library(rjson)
library(glmmTMB)
library(effects)
library(gtsummary)
theme_set(theme_sjplot())

df <- read.csv(file="data/data_exclude.csv")

df$visGroup <- factor(df$visGroup, c("line","band","hop"))
levels(df$visGroup ) <- c("Line","Cone","HOPs")
df$nDataShown <- factor(df$nDataShown)

df <- rename(df, 
             sampleUncertainty = uncertaintyShown,
             visTreatment = visGroup)

#df$visTreatment <- df$visTreatment

# tiny plots
g1 <- df %>%
  rename(Congruency = congruency) %>%
  ggplot(aes(x=preBeliefDistance,fill=Congruency)) + 
  geom_density(alpha=0.5) +
  annotate("text", x = 1.4, y = 2.8, label = "Incongruent", size = 2.5) +
  annotate("text", x = 0.75, y = 3.7, label = "Congruent", size = 2.5) +
  theme(legend.position = "none") + 
  labs(x = " ", y = " ", subtitle = "Pre-Belief Distance") 

g2 <- df %>%
  ggplot(aes(x=sampleUncertainty,fill=nDataShown)) + 
  geom_density(alpha=0.5) +
  annotate("text", x = 1.3, y = 2, label = "Data Shown\n(n = 10)", size = 2.5) +
  annotate("text", x = 0.75, y = 3.7, label = "Data Shown\n(n = 100)", size = 2.5) +
  theme(legend.position = "none") +
  labs(x = " ", y = " ", subtitle = "Sample Uncertainty")

cowplot::plot_grid(g1, g2,
                   label_x = -0.2,
                   ncol = 2) %>%
  ggsave(width = 4, height = 1.7, filename = "images/study3-ivs.pdf")


ggplot(df,aes(x=preBeliefDistance, fill=congruency)) + 
  geom_density(alpha=0.5) +
  facet_wrap(~vars)
  

# models

m = lmer(diffBeliefAbs ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  +  (1|usertoken) + (1|vars),df)
m1 = lmer(diffUncertainty ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  +(1|usertoken) + (1|vars),df)

# linear mixed effects

a <- plot_model(m,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est", show.intercept = TRUE ) +
  scale_y_continuous(breaks=seq(-.75,0.75,.25)) +
  theme(axis.text.y = element_text(size = 8),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Absolute Belief Difference", title = "") +
  ylim(-0.25, 0.9)

b <- plot_model(m1, vline.color = "grey",show.values = TRUE, value.offset = .4, value.size = 3, show.intercept = TRUE) +
  ylim(-.3,.3) +
  theme(axis.text.y=element_blank(),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Uncertainty Difference", title = "")

# final plots
library(cowplot)

plot_grid(a,
  b,
  label_x = -0.2,
  ncol = 2,
  rel_widths = c(4.6, 2.4)) %>%
  ggsave(width = 6, height = 3.5, filename = "images/study3-me.pdf")


pvalue_adjust <- function(x){
  style_pvalue(stats::p.adjust(x, "fdr"), digits = 2)
}

gtsummary::tbl_regression(
  m,
  pvalue_fun = pvalue_adjust
)


df$posterior_belief_abs_error_bnd = df$postBeliefDistance/2

## beta regression

#m2 = lmer(postBeliefDistance ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  +  (1|usertoken) + (1|vars),df)

#visTreatment + population_correlation_abs + 

m2 = glmmTMB(posterior_belief_abs_error_bnd ~ 
               visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  +  (1|usertoken) + (1|vars), 
            df,
            family=list(family="beta", link="logit"))

c <- plot_model(m2, vline.color = "grey",show.values = TRUE, value.offset = .4, value.size = 3) +
  ylim(-.2,.2) +
  labs(title = " ")

confint(m2)
summary(m2)
