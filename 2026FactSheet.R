##############
# Title: Fact Sheet for the 2021 American Samoa small boat fishery survey
# Author: Crystal Dombrow
# Date: August 2026
##############

#-------------------------------------
# RUN DATA CLEANING SCRIPT
#-------------------------------------

#Clear workspace
rm(list = ls())

#Run data cleaning script
source("Data/AS_SBF_DataCleaning.R")


#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)


#Run functions used in the analyses below
source("Functions/DataSummariesFunction_FactSheet.R")
source("Functions/DistributionFunction_FactSheet.R")


#-------------------------------------
# RUN ANALYSES TO PRODUCE TABLES
#-------------------------------------

# Q10/11. aggregate pounds caught of bottomfish (deep and shallow combined) from survey responses (Q10 + Q11)

#DATA SUMMARIES
Q10.11.combined.data.sum.factsheet <- 
  data.summaries.function.factsheet(q.number = "Q10.Q11.combined")

#------

#AGGREGATE LBS BOTTOMFISH FOR MEALS PROVIDED
Q10.11.combined.data.sum.factsheet <- 
  data.summaries.function.factsheet(q.number = "Q10.11.total.mid.ifelse") 


#full sample sum
q10.11.fullsample.sums.meals <- as.sbs.data.cleaned %>%
  summarise(sum.total.bf = round(sum(Q10.11.total.mid.ifelse, na.rm = T)),
            meals.bf = ((sum.total.bf * 0.7) * 4))

#by island sum
q10.11.island.sums.meals <- as.sbs.data.cleaned %>%
  group_by(Island) %>%
  summarise(sum.total.bf = round(sum(Q10.11.total.mid.ifelse, na.rm = T)),
            meals.bf = ((sum.total.bf * 0.7) * 4))


#-------------------------------------

# Self classification breakdown (Q14)

#DISTRIBUTION
q14.distr.factsheet <- distribution.function.factsheet(q.number = "Q14A.1",
                                                       categories = 1:8)


#-------------------------------------

# Estimated percentage of fish consumed at home and given to family members (Q16)

#CONSUMED AT HOME
q16a.distr.factsheet <- distribution.function.factsheet(q.number = "Q16A", 
                                                        categories = 1:6)

q16a.data.sum.factsheet <- data.summaries.function.factsheet(q.number = "Q16A.mid")


#GIVEN AWAY
q16b.distr.factsheet <- distribution.function.factsheet(q.number = "Q16B", 
                                                        categories = 1:6)

q16b.data.sum.factsheet <- data.summaries.function.factsheet(q.number = "Q16B.mid")

#Percentage of fishers that gave away fish (Q16 where give away > 0%) 
q16b.any.distr.factsheet <- distribution.function.factsheet(q.number = "Q16B.any", 
                                                            categories = 1:2)


#-------------------------------------

# Percentage of fishers with fish sales (Q17)

#DISTRIBUTION
q17.distr.factsheet <- distribution.function.factsheet(q.number = "Q17", 
                                                       categories = 1:2)


#-------------------------------------

# Percentage of fishers with fish sales to friends/neighbors (Q18)

#FRIENDS/NEIGHBORS/COWORKERS
q18e.distr.factsheet <- distribution.function.factsheet(q.number = "Q18E", 
                                                        categories = 1:2)


#-------------------------------------

# Percentage responses to "Will more people fish bottomfish next year" (Q41)

#BOTTOMFISH COMBINED
q41.bottomfish.distr.factsheet <- distribution.function.factsheet(q.number = 
                                             "Q41.bottomfish", categories = 1:2)


#-------------------------------------

# Why response summary for Yes responses to Q41 (Q42)

q42.bottomfish.factsheet <- as.sbs.data.cleaned %>%
  select(Q41.bottomfish, Q42, Island) %>%
  filter(Q41.bottomfish == 1) %>%
  group_by(Island)

write.csv(q42.bottomfish.factsheet, paste("Tables/Q42.bottomfish_factsheet.csv"), row.names = F)


#-------------------------------------

# Percentage responses to: (Q44)

# Respected by community
q44a.distr.factsheet <- distribution.function.factsheet(q.number = "Q44A", 
                                                        categories = 1:5)

# who i am
q44b.distr.factsheet <- distribution.function.factsheet(q.number = "Q44B", 
                                                        categories = 1:5)

# part of culture
q44c.distr.factsheet <- distribution.function.factsheet(q.number = "Q44C", 
                                                        categories = 1:5)


#-------------------------------------

# Percentage responses to "Managers know about fishing community" (Q45, Q46)

#Q45E
q45e.distr.factsheet <- distribution.function.factsheet(q.number = "Q45E", 
                                                        categories = 1:5)


#Q46E
q46e.distr.factsheet <- distribution.function.factsheet(q.number = "Q46E", 
                                                        categories = 1:5)


