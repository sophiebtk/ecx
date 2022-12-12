# Ecx HandIn II 12.12.2022

library(estimatr)
library(dplyr)

setwd("C:/Users/sophi.LAPTOP-O5LENC3P/Desktop/R")
wage_data <- read.csv("lwage.csv")

# a)
reg_a <- lm_robust(lwage ~ educ + exper, wage_data, alpha = 0.05)
summary(reg_a)

# b)
reg_b <- lm_robust(lwage ~ educ + exper, data = wage_data, se_type = "HC3")
summary(reg_b)

# c) ???
reg_c <- lm_robust(lwage ~ educ + exper + educ*exper, data = wage_data, se_type = "HC3")
summary(reg_c)


wage_data <- wage_data %>% mutate(exper10 = exper - 10)

reg_c1 <- lm_robust(lwage ~ educ + exper + educ*exper10, data = wage_data, se_type = "HC3")
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

