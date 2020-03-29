# This DAG created from Dagitty and exported the R code
# Created on 27/03/2020

testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("HIV infection","Education Level","HH Wealth Index","Region","Occupation","Age Group"),
                       c("Age Group","Education Level"),
                       c("Age Group","Mobility","Occupation"),
                       c("Age Group","HH Wealth Index","Occupation","Education Level"),
                       c("Age Group","Region"),
                       c("Education Level","Mobility","Occupation"),
                       c("Education Level","Alcohol"),
                       c("Education Level","Marital Status"),
                       c("Education Level","Region"),
                       c("Occupation","Alcohol","Age Group"),
                       c("Occupation","Marital Status","Age Group"),
                       c("Occupation","Region"),
                       c("Mobility","HH Wealth Index","Occupation"),
                       c("Mobility","Alcohol","Age Group"),
                       c("Mobility","Alcohol","Occupation"),
                       c("Mobility","Marital Status","Age Group"),
                       c("Mobility","Marital Status","Occupation"),
                       c("Mobility","Region"),
                       c("HH Wealth Index","Alcohol","Age Group"),
                       c("HH Wealth Index","Alcohol","Education Level","Occupation"),
                       c("HH Wealth Index","Marital Status","Age Group"),
                       c("HH Wealth Index","Marital Status","Education Level","Occupation"),
                       c("Alcohol","Marital Status","Age Group"),
                       c("Alcohol","Region"),
                       c("Marital Status","Region"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
  
}

# Dagitty Code. Copy and Paste in DAGitty to Generate the DAG in DAGitty
dag {
  bb="0,0,1,1"
  "Age Group" [exposure,pos="0.269,0.385"]
  "Education Level" [pos="0.135,0.188"]
  "HH Wealth Index" [pos="0.724,0.086"]
  "HIV infection" [outcome,pos="0.871,0.374"]
  "Marital Status" [pos="0.269,0.762"]
  Alcohol [pos="0.532,0.494"]
  Mobility [pos="0.668,0.216"]
  Occupation [pos="0.535,0.249"]
  Region [pos="0.536,0.037"]
  "Age Group" -> "HIV infection"
  "Age Group" -> "Marital Status"
  "Age Group" -> Alcohol
  "Age Group" -> Occupation
  "Education Level" -> "HH Wealth Index"
  "Education Level" -> Occupation
  "HH Wealth Index" -> "HIV infection"
  "Marital Status" -> "HIV infection"
  Alcohol -> "HIV infection"
  Mobility -> "HIV infection"
  Occupation -> "HH Wealth Index"
  Occupation -> "HIV infection"
  Occupation -> Mobility
  Region -> "HH Wealth Index"
  Region -> "HIV infection"
}
