# This is a model to analyse data using mixed effects
# Check out this for some explanation of GLMM vs GLM https://stats.stackexchange.com/questions/32419/difference-between-generalized-linear-models-generalized-linear-mixed-models
library(lme4)

mod1 <- glmer(hiv ~ age_cat2 + marital_status + educat_cat2 +  
                  occup_cat + alcohol_0 + mobile_0 +  non_circum_0 +(1 | region_name),
              family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Male", 
              control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

# print the mod results without correlations among fixed effects
print(mod1, corr = FALSE)
#alternatively
summary(mod1)


# confidence intervals (CIs). We can get rough estimates using the SEs
se <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(mod1), LL = fixef(mod1) - 1.96 * se, UL = fixef(mod1) + 1.96 *se)

# wanted odds ratios instead of coefficients on the logit scale
exp(tab)

mod2 <- glmer(hiv ~ age_cat2 + marital_status + educat_cat2 +  
                occup_cat + alcohol_0 + mobile_0  +(1 | region_name),
              family = binomial(link='logit'), data = an_mydata_df, subset = sex_0 == "Female", 
              control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

#alternatively
summary(mod2)


# confidence intervals (CIs). We can get rough estimates using the SEs
se <- sqrt(diag(vcov(mod2)))
# table of estimates with 95% CI
tab2 <- cbind(Est = fixef(mod2), LL = fixef(mod2) - 1.96 * se, UL = fixef(mod2) + 1.96 *se)

# wanted odds ratios instead of coefficients on the logit scale
exp(tab2)