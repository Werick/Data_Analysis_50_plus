# Multivariable variables using the funcion summary_factor
# from the finalfit and dplyr library

# Created by Wafula Erick Mugoma
# Date: 16-03-2020
# Load the required Libarry
library(finalfit) #Library to check the variables are properly coded
library(dplyr) #Library to explore data

#Change the reference category of factor/categorical var using relevel function
# change primary educ to be the ref catagory
table(an_mydata_df$educat_cat2)
an_mydata_df$educat_cat2 <- relevel(an_mydata_df$educat_cat2, ref = "Primary")
table(an_mydata_df$educat_cat2)

# change Kenya to be the ref catagory
an_mydata_df$region_name <- relevel(an_mydata_df$region_name, ref = "Kenya")

# change Married to be the ref catagory
an_mydata_df$marital_status <- relevel(an_mydata_df$marital_status, ref = "Married")

# change informal_L/risk to be the ref catagory
an_mydata_df$occup_cat <- relevel(an_mydata_df$occup_cat, ref = "informal_L/risk") 

# change wealth quintile 4 to be the ref catagory
an_mydata_df$wealth_0 <- relevel(an_mydata_df$wealth_0, ref = "4")




explanatory = c("age_cat2","region_name", "marital_status","educat_cat2", "occup_cat", "wealth_0",
                "mobile_0","alcohol_0","self_hivtest_0")


# Univariable Analysis stratfied by gender/sex
# It would be easy to use the for loop here but unfornately no results are being displayed
# this has to be done one by one. Very painful indeed
# Female 

# This model throw this error :
# Error in model.frame.default(formula = hiv ~ i, data = an_mydata_df, subset = sex_0 ==  : 
#                                variable lengths differ (found for 'i')
# This is resolved using the paste fuction as shown below.

print(paste("ORs for: ",explanatory[1],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[1]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[2],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[2]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[3],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[3]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[4],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[4]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[5],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[5]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[6],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[6]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[7],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[7]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[8],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[8]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[9],"for Female Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[9]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

# Male 
explanatory = c("region_name","age_cat2", "marital_status","educat_cat2", "occup_cat", "wealth_0",
                "mobile_0","alcohol_0","non_circum_0","self_hivtest_0")


print(paste("ORs for: ",explanatory[1],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[1]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[2],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[2]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[3],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[3]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[4],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[4]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[5],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[5]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[6],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[6]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[7],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[7]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[8],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[8]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[9],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[9]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)

print(paste("ORs for: ",explanatory[10],"for Male Strata"))
lg_model <- glm(formula = paste("hiv ~", explanatory[10]), family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)
round(exp(coef(lg_model)),2)
round(exp(confint.default(lg_model)),2)



# Build the model using forward stepwise by adding variables that showed very strong association
# with HIV infection
lg_model <- glm(hiv ~ age_cat2, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)

# Display the coefficients on OR scale
exp(lg_model$coefficients)
# Display the 95% CIs using std errors and convert to OR scale using exp function
exp(confint.default(lg_model))

lg_model2 <- glm(hiv ~ age_cat2 + sex_0, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model2)

lg_model3 <- glm(hiv ~ age_cat2 * sex_0, family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model3)

# Check if there is Interaction betwn age_category and sex
# Check to see if the interaction param is significant (*)
anova(lg_model3, test = "Chisq")
exp(coef(lg_model3))
# we observed interaction/effect modification btwn age_group and sex, will present strified results


# Compare the models using anova - Checking Goodness-of-Fit
# Likelihood Ratio Test (LRT) to assess if our models are improving the fit
# observed effect modification by sex
anova(lg_model2, lg_model3, test = "Chisq")

lg_model <- glm(hiv ~ age_cat2 + sex_0 + region_name, family = binomial(link='logit'), data = an_mydata_df)
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




################################################################################################
# Kindly note that when building a causal regression model the tow important things to consider are:
# confounding and Muliti-colinearity

# Strong Confounders were
# 1. marital_status
# 2. Education Level
# 3. Previously Tested for HIV
# 4. Region
# 5. OCcupation
# 6. Alcohol

# Adding in mobility and wealth index due to known literature about their association with HIV
lg_model <- glm(hiv ~ age_cat2 + sex_0 + marital_status + educat_cat2 +  self_hivtest_0 +
                  region_name + occup_cat + alcohol_0 + mobile_0 + wealth_0, 
                family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)

# Display the coefficients on OR scale
round(exp(lg_model$coefficients),2)

#Check for interaction btwn sex and age_group
lg_model <- glm(hiv ~ age_cat2 * sex_0 + marital_status + educat_cat2 +  self_hivtest_0 +
                  region_name + occup_cat + alcohol_0 + mobile_0 + wealth_0, 
                family = binomial(link='logit'), data = an_mydata_df)
summary(lg_model)

anova(lg_model, test = "Chisq")

# Display the coefficients on OR scale
round(exp(lg_model$coefficients),2)


#We present stratified results
# Female
lg_model <- glm(hiv ~ age_cat2  + marital_status + educat_cat2 +  self_hivtest_0 +
                  region_name + occup_cat + alcohol_0 + mobile_0 + wealth_0, 
                family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female")
summary(lg_model)

# Display the coefficients on OR scale
round(exp(lg_model$coefficients),2)

# Display the 95% CI
round(exp(confint.default(lg_model)),2)

# Male
lg_model <- glm(hiv ~ age_cat2 + marital_status + educat_cat2 +  self_hivtest_0 +
                  region_name + occup_cat + alcohol_0 + mobile_0 + wealth_0 + non_circum_0, 
                family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male")
summary(lg_model)

# Display the coefficients on OR scale
round(exp(lg_model$coefficients),2)

# Display the 95% CI
round(exp(confint.default(lg_model)),2)
