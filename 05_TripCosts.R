#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# TRIP COSTS section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)

#Run trip costs function
source("Functions/TripCostsFunction.R")


#---------------------------------------
# CALCULATIONS BY GEAR TYPE & BREAKDOWN
#---------------------------------------

# Q31a. On average per trip, how much money did you spend on your most common 
  # (question 31) gear type trip? 
# Q32a. On average per trip, how much money did you spend on your second most 
  # common (question 32) gear type trip? 


# File naming convention uses gear type trip by codebook number to distinguish 
  # calculations, which can be referenced for their value in the comments below.


#------------------
#TROLLING
#------------------

#Full sample
q31.32.troll.full.list <- 
  trip.costs.function(gear.type.trip = "4", breakdown = "Full.sample")

#Island group
q31.32.troll.island.list <- 
  trip.costs.function(gear.type.trip = "4", breakdown = "Island")

# #Bottomfish
# q31.32.troll.bottomfish.list <- 
#   trip.costs.function(gear.type.trip = "4", breakdown = "Q4.bottomfish")
# 
# #Fisher type
# q31.32.troll.crew.list <- 
#   trip.costs.function(gear.type.trip = "4", breakdown = "Q1.combo")
# 
# #Samoan/Non-Samoan
# q31.32.troll.samoan.list <- 
#   trip.costs.function(gear.type.trip = "4", breakdown = "Q38.combo")
# 
# #Age group
# q31.32.troll.age.list <- 
#   trip.costs.function(gear.type.trip = "4", breakdown = "Q35.combo")
# 
# #Primary motivation
# q31.32.troll.pri.mot.list <- 
#   trip.costs.function(gear.type.trip = "4", breakdown = "Q14.combo")



#------------------
#DEEP BOTTOMFISH
#------------------

#Full sample
q31.32.deep.bf.full.list <- 
  trip.costs.function(gear.type.trip = "2", breakdown = "Full.sample")

#Island group
q31.32.deep.bf.island.list <- 
  trip.costs.function(gear.type.trip = "2", breakdown = "Island")

# #Bottomfish
# q31.32.deep.bf.bottomfish.list <- 
#   trip.costs.function(gear.type.trip = "2", breakdown = "Q4.bottomfish")
# 
# #Fisher type
# q31.32.deep.bf.crew.list <- 
#   trip.costs.function(gear.type.trip = "2", breakdown = "Q1.combo")
# 
# #Samoan/Non-Samoan
# q31.32.deep.bf.samoan.list <- 
#   trip.costs.function(gear.type.trip = "2", breakdown = "Q38.combo")
# 
# #Age group
# q31.32.deep.bf.age.list <- 
#   trip.costs.function(gear.type.trip = "2", breakdown = "Q35.combo")
# 
# #Primary motivation
# q31.32.deep.bf.pri.mot.list <- 
#   trip.costs.function(gear.type.trip = "2", breakdown = "Q14.combo")



#------------------
#SHALLOW BOTTOMFISH
#------------------
  #Only 2 respondents reported shallow bottomfishing as their primary gear trip
  #type, but 13 responded that it is their secondary gear trip type.


#Full sample
q31.32.sh.bf.full.list <- 
  trip.costs.function(gear.type.trip = "3", breakdown = "Full.sample")

#Island group
q31.32.sh.bf.island.list <- 
  trip.costs.function(gear.type.trip = "3", breakdown = "Island")

# #Bottomfish
# q31.32.sh.bf.bottomfish.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q4.bottomfish")
# 
# #Fisher type
# q31.32.sh.bf.crew.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q1.combo")
# 
# #Samoan/Non-Samoan
# q31.32.sh.bf.samoan.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q38.combo")
# 
# #Age group
# q31.32.sh.bf.age.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q35.combo")
# 
# #Primary motivation
# q31.32.sh.bf.pri.mot.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q14.combo")



#------------------
#SPEARFISHING
#------------------
  #7 responses on Tutuila for spearfishing, 0 on Manu'a Islands. Did not include
  #in brochure analysis because too few of categories had >=3 responses


#Full sample
q31.32.spear.full.list <- 
  trip.costs.function(gear.type.trip = "1", breakdown = "Full.sample")

#Island group
q31.32.spear.island.list <- 
  trip.costs.function(gear.type.trip = "1", breakdown = "Island")

# #Bottomfish
# q31.32.spear.bottomfish.list <- 
#   trip.costs.function(gear.type.trip = "1", breakdown = "Q4.bottomfish")
# 
# #Fisher type
# q31.32.spear.crew.list <- 
#   trip.costs.function(gear.type.trip = "3", breakdown = "Q1.combo")
# 
# #Samoan/Non-Samoan
# q31.32.spear.samoan.list <- 
#   trip.costs.function(gear.type.trip = "1", breakdown = "Q38.combo")
# 
# #Age group
# q31.32.spear.age.list <- 
#   trip.costs.function(gear.type.trip = "1", breakdown = "Q35.combo")
# 
# #Primary motivation
# q31.32.spear.pri.mot.list <- 
#   trip.costs.function(gear.type.trip = "1", breakdown = "Q14.combo")



#-------------------------------
# GALLONS / LBS / CASES
#-------------------------------
#Code to analyze if there are significant findings for gallons/lbs/cases 
  #reported as part of trip costs.

q31.32.gallons.lbs.cases <- as.sbs.data.cleaned %>% 
  select(Q31:Q32C)

write.csv(q31.32.gallons.lbs.cases, "Tables/Q31.32_gallonslbscases.csv", 
          row.names = F)
