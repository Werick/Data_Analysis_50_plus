# This script is used for analysis association between Missing HIV Outcome and other demo xtics
# Written By Wafula Erick
# Date: 15-03-2020 (dd/mm/yyyy) 

# Load the required libraries
library(gmodels)
library(dplyr)

#Check Association with gender
CrossTable(table(mydata_df$missing_hiv,mydata_df$sex_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition btwn gender and educ
           format = "SPSS")

# strong Association with gender/sex chisq = 46.01 p <0.001

#Check Association with age group
CrossTable(table(mydata_df$missing_hiv,mydata_df$age_cat2),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition btwn gender and educ
           format = "SPSS")

# Strong Association with age group chisq = 9.94 p = 0.007

#Check Association with marital status
CrossTable(table(mydata_df$missing_hiv,mydata_df$marital_status),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with marital status
           format = "SPSS")

# Strong Association with Marital status chisq = 47.32 p < 0.001


#Check Association with Education
CrossTable(table(mydata_df$missing_hiv,mydata_df$educat_cat2),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           format = "SPSS")

# Strong Association with Education level chisq = 142.81 p < 0.001
# people with primary educ were more likely to miss on HIV testing

#Check Association with those who had previously tested
CrossTable(table(mydata_df$missing_hiv,mydata_df$self_hivtest_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           format = "SPSS")
# Strong Association with those who had previously tested chisq = 1016.16 p < 0.001
# people had not previously tested for HIV more likely to miss on HIV testing


#Check Association with type of occupation
CrossTable(table(mydata_df$missing_hiv,mydata_df$occup_cat),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 2, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )

# Strong Association with occupation chisq = 194.68 p < 0.001
# Informal Low risk occup more likely to miss hiv status


#Check Association with HH wealth Index
CrossTable(table(mydata_df$missing_hiv,mydata_df$wealth_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 2, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )
#Strong Association with HH wealth Index chisq = 18.71, p=0.001

#Check Association with Mobility
CrossTable(table(mydata_df$missing_hiv,mydata_df$mobile_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 2, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )
# No assocaition with Mobility

#Check Association with region
CrossTable(table(mydata_df$missing_hiv,mydata_df$region_name),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 2, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )

#Strong association with Region chisq = 57.57 p<0.001
# Majority missing were more likely from Western Uganda