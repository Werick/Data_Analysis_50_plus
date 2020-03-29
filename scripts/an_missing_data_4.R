# This script is used for analysis association between Missing HIV Outcome and other demo xtics
# Written By Wafula Erick
# Date: 15-03-2020 (dd/mm/yyyy) 

# Load the required libraries
library(gmodels)
library(dplyr)
library(finalfit)

#Cross tabs can be done as shown below, but using the loop is super cool coz you end up with
# a very short block of code 
# Check Association with gender
CrossTable(table(mydata_df$missing_hiv,mydata_df$sex_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition btwn gender and educ
           format = "SPSS")

# strong Association with gender/sex chisq = 46.01 p <0.001


#Strong association with Region chisq = 57.57 p<0.001
# Majority missing were more likely from Western Uganda


# USing the final finalfit library with missing compare function
# See https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

explanatory = c("resident_0", "sex_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","circumcision_0","region_name","wealth_0","self_hivtest_0", "test_location_0")
dependent = "hiv_0"

mydata_df$missing_hiv <- factor(mydata_df$missing_hiv,levels = c(0,1), labels = c("Not Missing","Missing"))


#To generate the compare table, u need to run first missing_pair then followed by missing_compare
#shown below
#This is somehow complex so using the CrossTable is more palatable/easy than using this method.
#Even though this quick if works for you.
mydata_df %>%
  missing_pairs(dependent, explanatory)

mydata_df %>% 
  missing_compare(dependent, explanatory) %>% 
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run


# Using for loop to generate the cross tables, but rem u have to create a vector with th cols of interest
for(i in explanatory) {
  print(paste("Cross table for: ",i))
  CrossTable(table(mydata_df[[i]],mydata_df$missing_hiv),
             prop.r = TRUE, #add prop by row
             prop.c=TRUE, #add prop by column
             prop.t=FALSE, #remove prop by row
             prop.chisq=FALSE, #remove chisq contribution
             chisq=TRUE, #add chisq to display assocition with education
             digits = 1, #Number of digits after the decimal point for cell proportions
             format = "SPSS" )
}