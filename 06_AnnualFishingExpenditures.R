#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# ANNUAL FISHING EXPENDITURES section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)

#Source function
source("Functions/AnnualExpendituresFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q33. In an effort to better understand your economic contribution to American 
      # Samoa’s economy, we would like to ask about your fishing-related 
      # expenditures in 2020.  In the table below please indicate how much, 
      # if any, was spent on the following items during 2020.

  #For boat owners only.

#"Other" is 0 or NA for every observation. Removed from function because it 
  #messed up the calculations when retained.


#FULL SAMPLE
q33.full.sample.list <- annual.expenditures.function(breakdown = "Full.sample")

#ISLAND GROUP
q33.island.list <- annual.expenditures.function(breakdown = "Island")

# #BOTTOMFISH
# q33.bottomfish.list <- annual.expenditures.function(breakdown = "Q4.bottomfish")
# 
# #FISHER TYPE
# q33.crew.list <- annual.expenditures.function(breakdown = "Q1.combo")
# 
# #SAMOAN/NON-SAMOAN
# q33.samoan.list <- annual.expenditures.function(breakdown = "Q38.combo")
# 
# #AGE GROUP
# q33.age.list <- annual.expenditures.function(breakdown = "Q35.combo")
# 
# #PRIMARY MOTIVATION
# q33.pri.mot.list <- annual.expenditures.function(breakdown = "Q14.combo")

