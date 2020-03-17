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

an_mydata_df <- mydata_df %>%
  filter(!is.na(hiv_0)) #Drop all participants missing HIV status in the final analysis


#Examine with ff_glimpse - This shows the various levels for factor variables and missing % for each var

# This has been done for convience to capture only variables of interest, otherwise ff_glimse will
# display the vars in your dataset if no param is passed to it
explanatory = c("age_0","resident_0", "sex_0", "educat_cat2", "occup_cat","age_cat2", "mobile_0","marital_status",
                "alcohol_0","circumcision_0","region_name","wealth_0","self_hivtest_0")
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

#Workout HIV prevalence
# The functions used in this section are from gmodels package
#Ci.binom fuction requires that you outcone var is numeric and coded in Binary (0,1)
convertHIVtoBinary <- function(x){
  return(ifelse(x['hiv_0']==0,0,1))
}
an_mydata_df$hiv<-apply(an_mydata_df, 1, convertHIVtoBinary)
table(an_mydata_df$hiv_0,an_mydata_df$hiv) #check if the convesrion is well done

#get the 95% CI and HIV prevalence for the whole population
ci.binom(an_mydata_df$hiv)


#Cross check using cross tab
CrossTable(table(an_mydata_df$sex_0,an_mydata_df$hiv),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 1, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )

#get the 95% CI and HIV prevalence by Gender
sex <- levels(an_mydata_df$sex_0)
for (i in sex){
  print(paste("HIV Prevalance by Sex :", i))
  prev_male <- an_mydata_df %>%
    select(sex_0,hiv)  %>%
    filter(sex_0 == i) 
  t <- ci.binom(prev_male$hiv)
  print(t)
}



# Print the two way table
CrossTable(table(an_mydata_df$region_name,an_mydata_df$hiv_0),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 1, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )

#get the 95% CI and HIV prevalence by Region
region <- levels(an_mydata_df$region_name)
for (i in region){
  print(paste("HIV Prevalance by Region :", i))
  prev_eug <- an_mydata_df %>%
    select(region_name,hiv)  %>%
    filter(region_name == i) 
  t <- ci.binom(prev_eug$hiv)
  print(t)
}


CrossTable(table(an_mydata_df$age_cat2,an_mydata_df$hiv),
           prop.r = TRUE, #add prop by row
           prop.c=TRUE, #add prop by column
           prop.t=FALSE, #remove prop by row
           prop.chisq=FALSE, #remove chisq contribution
           chisq=TRUE, #add chisq to display assocition with education
           digits = 1, #Number of digits after the decimal point for cell proportions
           format = "SPSS" )


#get the 95% CI and HIV prevalence by Age_group
agegrp <- levels(an_mydata_df$age_cat2)
for (i in agegrp){
  print(paste("HIV Prevalance by Age Group :", i))
  prev_eug <- an_mydata_df %>%
    select(age_cat2,hiv)  %>%
    filter(age_cat2 == i) 
  t <- ci.binom(prev_eug$hiv)
  print(t)
}



