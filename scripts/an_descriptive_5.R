# Descritive summary of variables using the funcion summary_factor
# from the finalfit and dplyr library

# Created by Wafula Erick Mugoma
# Date: 15-03-2020
# See https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
# for guidance

# Load the required Libarry
library(finalfit) #Library to check the variables are properly coded
library(dplyr) #Library to explore data

#Load data
# Load the data

an_mydata_df <- mydata_df %>%
  filter(!is.na(hiv_0), !is.na(sex_0)) #Drop all participants missing HIV status in the final analysis and gender

an_mydata_df$hiv_0 <- as.factor(an_mydata_df$hiv_0)

# Add labels to test_location
an_mydata_df$test_location_0 <- factor(an_mydata_df$test_location_0, levels = c(0,1,2),
                                       labels = c("Post-CHC","CHC","Clinic"))

#Examine with ff_glimpse - This shows the various levels for factor variables and missing % for each var

# This has been done for convience to capture only variables of interest, otherwise ff_glimse will
# display the vars in your dataset if no param is passed to it
explanatory = c("age_0","resident_0", "sex_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","non_circum_0","region_name","wealth_0","self_hivtest_0","test_location_0")
dependent = "hiv_0"

an_mydata_df %>%
  summary_factorlist(dependent, explanatory, 
                     na_include = TRUE, #explanatory var missing data
                     p=TRUE,
                     cont = "median", #Can be mean/median for continuous var
                     p_cat = "chisq", # Categorical var test - chisq/fisher
                     total_col = TRUE, #Include Total col across factor levels
                     column = FALSE, #Row proportions (rather than column)
                     add_col_totals = TRUE, #Add column totals - this is the first row to show your n
                     add_dependent_label = TRUE #Label with dependent name
                     ) -> t #store in df called t
#display t
t

#Stratified by Gender
explanatory = c("resident_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","non_circum_0","region_name","wealth_0","self_hivtest_0","test_location_0")


for (i in explanatory) {
  print(paste("Two table for sex and ",i))
  CrossTable(table(an_mydata_df[[i]], an_mydata_df$sex_0),
             prop.r = TRUE, #add prop by row
             prop.c=TRUE, #add prop by column
             prop.t=FALSE, #remove prop by row
             prop.chisq=FALSE, #remove chisq contribution
             chisq=TRUE, #add chisq to display assocition with education
             digits = 1, #Number of digits after the decimal point for cell proportions
             format = "SPSS" )
  
}



# Workout HIV prevalence for category of the predictor
explanatory = c("resident_0", "sex_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","non_circum_0","region_name","wealth_0","self_hivtest_0","test_location_0")

# The functions used in this section are from gmodels package
#Ci.binom fuction requires that you outcome var is numeric and coded in Binary (0,1)
convertHIVtoBinary <- function(x){
  return(ifelse(x['hiv_0']==0,0,1))
}
an_mydata_df$hiv<-apply(an_mydata_df, 1, convertHIVtoBinary)
table(an_mydata_df$hiv_0,an_mydata_df$hiv) #check if the convesrion is well done

#get the 95% CI and HIV prevalence for the whole population
round(ci.binom(an_mydata_df$hiv),4)*100


#Cross check using cross tab
CrossTable(table(an_mydata_df$sex_0,an_mydata_df$hiv),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 1, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )



for (i in explanatory) {
  print(paste("Get HIV Prevalence for predictor: ",i))
  
  #get the 95% CI and HIV prevalence by Gender
  predictor_x <- levels(an_mydata_df[[i]])
  
 
  for (x in predictor_x){
    print(paste("HIV Prevalance for : ",i,"_", x))
    hiv_prevalence <- an_mydata_df[,c(i,"hiv")] #This method works as select had failed to work for some factor variables
    hiv_prevalence <- hiv_prevalence %>%
      # select(an_mydata_df[[i]],hiv)  %>%
      filter(an_mydata_df[[i]] == x) 
    t <- round(ci.binom(hiv_prevalence$hiv),4)*100 # present the values as %
    print(t)
  }
  
  
}



