#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# Vessel Characteristics section
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
source("Functions/DistributionTablesFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
  # (FOR BOAT OWNERS ONLY)
#-------------------------------------

# Q22. Do you own the boat that you fish on?

#DISTRIBUTION
q22.distr.list <- distribution.tables.function(q.number = "Q22",
                                               categories = 1:2)

#CHARACTERISTICS ABOUT BOAT OWNERS
own.boat.characteristics <- as.sbs.data.cleaned %>% 
  filter(Q22.chr == "yes") %>% 
  select(Q22:Q30, Island, Q35.combo, Q14.combo, Q31C, Q32C, Q40, Q1.combo, 
         Q38.combo, Q39)


#---------------------------------------------------------------------

# Q23. In 2020, what percent of time did other people (other than family members) 
    # use the boat without you? 

#DATA SUMMARIES
q23.data.sum.list <- data.summaries.function(q.number = "Q23.mid.ifelse")

#DISTRIBUTION
q23.distr.list <- distribution.tables.function(q.number = "Q23.ifelse",
                                               categories = 1:6)


#---------------------------------------------------------------------

# Q24. What is the length of your boat?

#DATA SUMMARIES
q24.data.sum.list <- data.summaries.function(q.number = "Q24.ifelse")


#---------------------------------------------------------------------

# Q25. What is the total horsepower? 

#DATA SUMMARIES
q25.data.sum.list <- data.summaries.function(q.number = "Q25.ifelse")


#---------------------------------------------------------------------

# Q26. In what year was the boat built?

#DATA SUMMARIES
q26.data.sum.list <- data.summaries.function(q.number = "Q26.ifelse")


#---------------------------------------------------------------------

# Q27. In what year did you purchase the boat you fish on?

#DATA SUMMARIES
q27.data.sum.list <- data.summaries.function(q.number = "Q27.ifelse")


#---------------------------------------------------------------------

# Q28. How much did you pay to purchase the boat you fish on?
      # (If homebuilt – how much did it cost to build it?) 

#DATA SUMMARIES
q28.data.sum.list <- data.summaries.function(q.number = "Q28.ifelse")


#---------------------------------------------------------------------

# Q29. What is the approximate market value of your boat? 
    # (considering age & current condition & including motor(s) & trailer) 

#DATA SUMMARIES
q29.data.sum.list <- data.summaries.function(q.number = "Q29.ifelse")


#---------------------------------------------------------------------

# Q30. Please enter the most recent year in which you made any major improvement 
    # to your vessel.

#DATA SUMMARIES
q30.data.sum.list <- data.summaries.function(q.number = "Q30.ifelse")

