# Multivariable variables using the funcion summary_factor
# from the finalfit and dplyr library

# Created by Wafula Erick Mugoma
# Date: 16-03-2020
# Load the required Libarry
library(finalfit) #Library to check the variables are properly coded
library(dplyr) #Library to explore data

lg_model <- glm(hiv ~ sex_0, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)

lg_model2 <- glm(hiv ~ sex_0 + age_cat2, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model2)

lg_model3 <- glm(hiv ~ sex_0 * age_cat2, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model3)

#Compare the models using anova - Checking Goodness-of-Fit
# Likelihood Ratio Test to assess if our models are improving the fit
# bserved difference in model fit is statistically significant
anova(lg_model2, lg_model3, test = "Chisq")

lg_model <- glm(hiv ~ sex_0 * age_cat2 + region_name, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)

anova(lg_model3, lg_model, test = "Chisq")

#Display the 95% CIs using std errors
confint.default(lg_model2)
#Display the 95% CIs using log-likelihood
confint(lg_model2)

#Add exp to display on the OR scale
exp(confint(lg_model2))


# Disply the Coeffients using the OR scale insteand of log(OR) scale
exp(lg_model2$coefficients)

