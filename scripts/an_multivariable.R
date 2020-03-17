# Multivariable variables using the funcion summary_factor
# from the finalfit and dplyr library

# Created by Wafula Erick Mugoma
# Date: 16-03-2020
# Load the required Libarry
library(finalfit) #Library to check the variables are properly coded
library(dplyr) #Library to explore data

lg_model <- glm(hiv ~ sex_0, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)