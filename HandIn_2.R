# Ecx HandIn II 12.12.2022

library(estimatr)
library(dplyr)
library(car)

setwd("C:/Users/sophi.LAPTOP-O5LENC3P/Desktop/R")
wage_data <- read.csv("lwage.csv")

# a)
reg_a <- lm_robust(lwage ~ educ + exper, data = wage_data, alpha = 0.02)
summary(reg_a)

reg_a <- lm_robust(lwage ~ educ + exper, data = wage_data, alpha = 0.05)
summary(reg_a)
reg_a <- lm(lwage ~ educ + exper, data = wage_data, alpha = 0.05)
summary(reg_a)
# b)
reg_b <- lm_robust(lwage ~ educ + exper, data = wage_data, se_type = "HC3", alpha = 0.02)
summary(reg_b)

reg_b <- lm_robust(lwage ~ educ + exper, data = wage_data, se_type = "HC3", alpha = 0.05)
summary(reg_b)
# c)
wage_data <- wage_data %>% mutate(educ_exper10 = educ*(exper - 10))

reg_c1 <- lm_robust(lwage ~ educ + exper + educ_exper10, data = wage_data, se_type = "HC3")
summary(reg_c1)

# e)
wage_data <- wage_data %>% mutate(married_urban = married*urban,
                                  nonmarried_urban = (1-married)*urban,
                                  married_rural = married*(1-urban),
                                  nonmarried_rural = (1-married)*(1-urban))

reg_e <- lm_robust(lwage ~ educ + exper + married_urban + married_rural + nonmarried_rural,
                   data = wage_data, se_type = "HC3")
summary(reg_e)

# f)
reg_super_great_final_function <- lm_robust(lwage ~ educ + exper + married + urban + meduc + feduc, data = wage_data, se_type = "HC3")
summary(reg_super_great_final_function)

linearHypothesis(reg_super_great_final_function, c("meduc=0", "feduc = 0"), test=c("F"))

