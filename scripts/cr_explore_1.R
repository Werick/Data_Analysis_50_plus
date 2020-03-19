# This is an R Script to explore and create data additional fields for 50 plus data analysis.
# This data is from SEARCH Fup-YR3 Data
# Created by Wafula Erick Mugoma
# Date: 15-03-2020

###########################################################################################
# Load the required Libraries
library(readstata13) # Read data from stata version 15 and older
library(gmodels) #Two way tables and Chisq
library(plyr)
library(dplyr)

##########################################################################################

# input Stata file
# Get data from 50plus from 
path_to_file <- "C:/Users/Ericks/OneDrive/EMP500_Data_Analysis/dofiles/cr_Phase1_baseline_4_50plus.dta"

mydata_df <- read.dta13(path_to_file)

copy_mydata_df <- mydata_df

# Check the file Structure
str(mydata_df) #

# Check the number of variables imported
dim(mydata_df)

# Create var Missing_hiv for the Outcome. This will help in analysis of missing outcome data
############################################################################################
mydata_df <- mydata_df %>%
  mutate(missing_hiv = ifelse(is.na(hiv_0),1,0))

# Check it is well done
mydata_df %>%
  select(hiv_0,missing_hiv) %>%
  #filter(hiv_0==1) %>% #Uncomment to see the NA's
  head(10)

# Create var age_cat3, This is a break down of age in groups of 5yrs and will be used for building
# The age pyramid
################################################################################################
mydata_df <- mydata_df %>%
  mutate(age_cat3 = ifelse(age_0 %in% 50:54,1,
                           ifelse(age_0 %in% 54:59,2,
                                  ifelse(age_0 %in% 60:64,3,
                                         ifelse(age_0 %in% 65:69,4,
                                                ifelse(age_0 %in% 70:74,5,
                                                       ifelse(age_0 %in% 75:79,6,7)))))))


# Check it is well done
mydata_df %>%
  select(age_0,age_cat3) %>%
  #filter(hiv_0==1) %>% #Uncomment to see the NA's
  head(10)

# Convert to factor and add the labels
mydata_df$age_cat3=factor(mydata_df$age_cat3,levels = c(1,2,3,4,5,6,7), 
                          labels = c("50-54 Yrs","55-59 Yrs","60-64 Yrs","65-69 Yrs","70-74 Yrs","75-79 Yrs",">=80 Yrs"))



# Create var age_cat2, This is a break down of age in groups of 10yrs and will be used for analysis
################################################################################################
mydata_df <- mydata_df %>%
  mutate(age_cat2 = ifelse(age_0 %in% 50:59,1,
                           ifelse(age_0 %in% 60:69,2,3)))


# Check it is well done
mydata_df %>%
  select(age_0,age_cat2) %>%
  #filter(hiv_0==1) %>% #Uncomment to see the NA's
  head(10)

# Convert to factor and add the labels
mydata_df$age_cat2=factor(mydata_df$age_cat2,levels = c(1,2,3), 
                          labels = c("50-59 Yrs","60-69 Yrs",">=70 Yrs"))


# Create var marital_status, this will be used in the final analysis
# Variable: Marital status
# "1 : Single
#  2 : Married
#  3 : Widowed
#  4 : Divorced
#  5 : Separated
# NA: Dont know, Refused to answer, Not Applicable, Missing
# If inmigrant_3=1, then set to marital_3"
################################################################################################
mydata_df %>%
  select(searchid,sex_0,marital_0)%>%
  filter(marital_0=="Single") %>%
  head() %>%
  str()

mydata_df <- mydata_df %>%
  mutate(marital_status = ifelse(marital_0=="Single",1,
                                 ifelse(marital_0=="Married",2,
                                        ifelse(marital_0 %in% c("Widowed","Divorced","Separated"),3,NA))))

# Set those married in a polygamous marriage
# Note that mutate failed to work and therefore I ended up using this method/function
setPolygamy <- function(d) {
  m_status <- d['marital_status']
  p <- d['polygamy_0']
  
  if (!is.na(p)) {
    #print(paste("Polygamy value ",class(p))) #Uncomment to debug
    if(as.numeric(p) == 1) {
      m_status <- 4
      #print(paste("M_status value ",m_status)) #Uncomment to debug
    }
  }
  return(m_status)
}


mydata_df$marital_status <- apply(mydata_df,1,setPolygamy)

mydata_df %>%
  select(marital_0,polygamy_0,marital_status) %>%
  #filter(marital_status==4) %>% #Uncomment to see the NA's
  head(10)

# Convert to factor and add the labels
mydata_df$marital_status <- as.numeric(trimws(mydata_df$marital_status))
mydata_df$marital_status=factor(mydata_df$marital_status,levels = c(1,2,3,4), 
                          labels = c("Single","Married","W/D/W","Polygamy"))

table(mydata_df$marital_status,exclude = NULL)

# Create var Educ_cat for education levels
# Education levels were different depending with country
# Kenya =1 and Uganda =2
# variable: education
# "NA: Dont know, Refused to answer, Not Applicable, Missing
# If inmigrant_3=1, then set to education_3
#  Uganda
# 0 : No school (1)
# 1 : P1-P6 (2)
# 2 : P7 (3)
# 3 : S1-S3 (4)
# 4 : S4 (5)
# 5 : S5 (6)
# 6 : S6 (7)
# 7 : Tertiary/Vocational (8)
# 8 : University (9)
# 9 : Post-graduate (10)
# Kenya
# 0 : No school (1)
# 1 : primary 1-4 (2)
# 2 : primary 5-8 (3)
# 3 : completed primary 8 (4)
# 4 : secondary form 1-2 (5)
# 5 : secondary form 3-4 (6)
# 6 : completed secondary form 4 (7)
# 7 : Tertiary/Vocational (8)
# 8 : University (9)
# 9 : Post-graduate" (10)

#################################################################################################

mydata_df <- mydata_df %>%
  mutate(educat_cat2 = ifelse(education_0 == "0",1,
                              ifelse(country == "Kenya" & education_0 %in% c("1","2","3"),2,
                                     ifelse(country == "Uganda" & education_0 %in% c("1","2"),2,3))))


# Check it is well done
mydata_df %>%
  select(country,education_0,educat_cat2) %>%
  filter(country=="Kenya", education_0 %in% c("4","5","6")) %>% #Uncomment to see the NA's
  head(10)

table(mydata_df$education_0, mydata_df$educat_cat2,exclude = NULL)

# Convert to factor and set labels
mydata_df$educat_cat2=factor(mydata_df$educat_cat2,levels = c(1,2,3), 
                                labels = c("No School","Primay","Secondary or Higher"))


# Create vr occup_cat
# /*******************************************************************************
#   "If inmigrant_3=1, set to occupation_3
# 1 : Farmer
#  2: Fishing/Fishmonger
#  3 : Shopkeeper/Market vendor
#  4 : Bar owner/Bar worker
#  5 : Transport [truck, taxi, motorcycle, bike, boat] drivers
#  6 : Hotel/Restaurant worker
#  7 : Tourism
#  8 : Teacher
#  9 : Student
#  10 : Government worker
#  11 : Military
#  12 : Housewife
#  13 : Household worker
#  14 : Healthcare worker
#  15 : Construction worker
#  16 : Factory worker
#  17 : Mining
#  18 : Disabled
#  19 : No job
#  20 : Other
#  21 : Manual Labour
#  77 : Other
#  NA : Dont know/NA; Refused; Missing"
# 
# 0 ==> no job (disabled, housewife, unemployed). move this to other b/c it has very few participants
# 1 ==> formal (student, teacher, government worker, military worker, health worker, factory worker), 
# 2 ==> informal low risk (Farmer, shopkeeper, market vendor, hotel worker, household worker, construction worker, Manual Labor, mining), 
# 3 ==> informal high risk (fisherman, bar owner, bar worker, truck/taxi/motorcycle/bike/boat drivers, tourism),   
# 4 ==> other/no job
# 
# 
# *******************************************************************************/
#   gen occup_cat=occupation
# *No Job
# recode occup_cat 18/19=4  20=4 77=4 8/11=1 14=1 16=1 1=2 3=2 6=2 12=2 13=2 15=2 17=2 21=2 2=3 4/5=3 7=3

mydata_df <- mydata_df %>%
  mutate(occup_cat = ifelse(occupation_0 %in% c(18:20,77),4,
                            ifelse(occupation_0 %in% c(8:11,14,16),1,
                                   ifelse(occupation_0 %in% c(1,3,6,12:13,15,17,21),2,
                                          ifelse(occupation_0 %in% c(2,4:5,7),3,NA)))))

table(mydata_df$occupation_0,mydata_df$occup_cat,exclude = NULL)

# Convert to factor and set labels
mydata_df$occup_cat=factor(mydata_df$occup_cat,levels = c(1,2,3,4), 
                             labels = c("formal","informal_L/risk","informal_H/risk","other/no_job"))

# Convert mobility to factor and set labels
mydata_df$mobile_0 <- factor(mydata_df$mobile_0,levels = c(0,1), labels = c("No","Yes"))

