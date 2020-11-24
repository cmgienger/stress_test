#Stress test - Key

library(tidyr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(ggfortify)

#import data
herbicide <-read.csv ("herbicide.csv")
data_herbicide <- herbicide %>%
  select(herbicide, weight) %>%
  filter(weight !="")


#ANOVA
model_herbicide <- lm(weight~herbicide, data = data_herbicide)
autoplot(model_herbicide)
#variances of residuals seem equal; no obvious trends in the leverage plot or scale-location
#plot. Residuals appear to have a normal distribution. 

anova(model_herbicide)
summary(model_herbicide)


#pairwise comparisons
emm_model_herbicide <- emmeans(model_herbicide, "herbicide")
pairs(emm_model_herbicide)
multcomp::cld(emm_model_herbicide)

#means of each group
sum_herbicide <- data_herbicide %>%
  group_by(herbicide) %>%
  summarise(mean.weight = mean(weight))
sum_herbicide


#visualize data
ggplot(data_herbicide, aes (x=herbicide, y = weight)) + 
  geom_boxplot() +
  xlab ("Herbicide") + 
  ylab ("Weight") + 
  theme_bw() + 
  annotate("text", x=c(1,2,3,4), y= c(2.25,2.25,2.25,2.25),
           label = c("1", "2", "2", "2"))


#We tested whether herbicide treatment had an effect on corn yield. We found a significant
#overall increase in corn yield as an effect of herbicide treatment (F3,12 = 42.108, P<0.0001).
#The herbicides (DNCR, Sesin, and TCA) did not differ significantly in their effects on corn yield 
#(means weights are 1.90, 1.74 and 1.70 units respectively), but all treatments did differ significantly
#from the control treatment which averaged a weight of 1.08 units (P<0.0001 for each comparison). 
