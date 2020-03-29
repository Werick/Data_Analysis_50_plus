# Confounder Analaysis
# Condition 2 (Associated with the exposure)
# Stratified by Gender
explanatory = c("educat_cat2", "occup_cat", "mobile_0","marital_status",
                "alcohol_0","region_name","wealth_0","self_hivtest_0","test_location_0")


for (i in explanatory) {
  print(paste("Two table for sex and ",i))
  CrossTable(table(an_mydata_df[[i]], an_mydata_df$age_cat2),
             prop.r = TRUE, #add prop by row
             prop.c=TRUE, #add prop by column
             prop.t=FALSE, #remove prop by row
             prop.chisq=FALSE, #remove chisq contribution
             chisq=TRUE, #add chisq to display assocition with education
             digits = 1, #Number of digits after the decimal point for cell proportions
             format = "SPSS" )
  
}

# Observed strong association btwn age and all the vars
