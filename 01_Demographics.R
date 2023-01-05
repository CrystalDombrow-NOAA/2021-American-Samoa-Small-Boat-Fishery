#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# Demographics section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)


#Run functions used in the analyses below
source("Functions/DataSummariesFunction.R")
source("Functions/DistributionFunction.R")
source("Functions/DistributionTablesFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q34. What is your gender?

#DISTRIBUTION
q34.distr.list <- distribution.tables.function(q.number = "Q34",
                                               categories = 1:2)


#---------------------------------------------------------------------

# Q35. What is your age?

#DISTRIBUTION
q35.distr.list <- distribution.tables.function(q.number = "Q35",
                                               categories = 1:6)

    
#DATA SUMMARIES
q35.data.sum.list <- data.summaries.function(q.number = "Q35.mid")


#---------------------------------------------------------------------

# Q36. What village do you live in?

#DISTRIBUTION
q36.distr.list <- distribution.function(q.number = "Q36")


#Create table; can't be produced through distribution tables function
q36.table <- q36.distr.list["q.full.sample.per"] %>%
  as.data.frame() %>%
  unique() %>%
  arrange(q.full.sample.per.Q36)

#Save table
  write.csv(q36.table, "Tables/Q36_distribution.csv", row.names = F)


#----------------------------------------------------------------------

# Q38. How would you describe your race? (check all that apply) 

#DISTRIBUTION
q38.distr.list <- distribution.tables.function(q.number = "Q38.1A",
                                               categories = 1:6)


#---------------------------------------------------------------------

# Q39. What is the highest level of education you have completed? 

#DISTRIBUTION
q39.distr.list <- distribution.tables.function(q.number = "Q39",
                                               categories = 1:7)


#---------------------------------------------------------------------

# Q40. What was your total household income, before taxes, in 2020, 
  #including fishing income?

#DISTRIBUTION
q40.distr.list <- distribution.tables.function(q.number = "Q40",
                                               categories = 1:6)
  
  
#DATA SUMMARIES
q40.data.sum.list <- data.summaries.function(q.number = "Q40.mid")

