# This script is used to check that variables are correctly coded using the finalfit library
# Created by Wafula Erick Mugoma
# Date: 15-03-2020
# See https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
# for guidance

# Load the required Libarry
library(finalfit) #Library to check the variables are properly coded


# Load the data
sex <- mydata_df$sex_0
age <- mydata_df$age_0
agegrp <- mydata_df$age_cat2
mobility <- mydata_df$mobile_0
mydata_df$hiv_0 <- as.factor(mydata_df$hiv_0)
hh_w_index <- mydata_df$wealth_0
educ <- mydata_df$educat_cat2
occup <- mydata_df$occup_cat
marital <- mydata_df$marital_status
previous_hiv_test <- mydata_df$self_hivtest_0
mydata_df$alcohol_0 <- as.factor(mydata_df$alcohol_0)
circum <- mydata_df$circumcision_0
region <- mydata_df$region_name
mydata_df$missing_hiv <- as.factor(mydata_df$missing_hiv)
mydata_df$resident_0 <- as.factor(mydata_df$resident_0)


#Examine with ff_glimpse - This shows the various levels for factor variables and missing % for each var

# This has been done for convience to capture only variables of interest, otherwise ff_glimse will
# display the vars in your dataset if no param is passed to it
explanatory = c("age_0","resident_0", "sex_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","circumcision_0","region_name","wealth_0","self_hivtest_0", "missing_hiv")
dependent = "hiv_0"

mydata_df %>%
  ff_glimpse(dependent,explanatory)

# #Run  display all vars
# mydata_df %>%
#   ff_glimpse()

# 2. Identify missing pattern in each variable: missing_plot
mydata_df %>%
  missing_pattern(dependent,explanatory)

#

