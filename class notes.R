library(readxl)
library(tidyverse)
library(broom)

ad_treatment <- read_excel("ad_treatment.xlsx")

sum_df <- ad_treatment %>% 
  mutate(
    sex = factor(sex, labels = c("Male", "Females")),
    drug_treatment =  factor(drug_treatment, levels = c("Placebo", "Low dose", "High Dose")),
    health_status = factor(health_status, levels = c("Healthy", "Alzheimer's"))
  )

#main effects
ad_aov <- aov(mmse ~ sex + drug_treatment + health_status, data = sum_df)
summary(ad_aov)

ad_aov <- aov(mmse ~ sex * drug_treatment * health_status, data = sum_df)
summary(ad_aov)

tidyTUKEY <- TukeyHSD(ad_aov, which = 'drug_treatment:health_status',conf.level=0.95)

plot(tidyTUKEY)
