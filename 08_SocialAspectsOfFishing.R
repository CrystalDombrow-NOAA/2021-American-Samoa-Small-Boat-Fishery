#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# SOCIAL ASPECTS OF FISHING section
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
source("Functions/DistributionTablesFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q44. Please state how much you agree or disagree with the following statements:


#DISTRIBUTION

# Q44A. As someone who fishes I am respected by the community
q44a.distr.list <- distribution.tables.function(q.number = "Q44A",
                                                categories = 1:5)

# Q44B. Fishing is an important part of who I am
q44b.distr.list <- distribution.tables.function(q.number = "Q44B",
                                                categories = 1:5)

# Q44C. Fishing is an important part of my culture
q44c.distr.list <- distribution.tables.function(q.number = "Q44C",
                                                categories = 1:5)

