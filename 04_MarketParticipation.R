#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# Market Participation section
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
source("Functions/WhereSellFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q14. What is your motivation for fishing? 
  #Determine the primary. For those you can’t determine, exclude from calculations.

#DISTRIBUTION
q14.distr.list <- distribution.tables.function(q.number = "Q14A.1",
                                               categories = 1:8) 


#--------------------------------------------------------------------------

# Q16. In 2020, what percent of your catch was...
  #Include 0% in calculations

#-------------
#DISTRIBUTION
#-------------

#CONSUMED AT HOME
q16a.distr.list <- distribution.tables.function(q.number = "Q16A",
                                                categories = 1:6)

#GIVEN AWAY
q16b.distr.list <- distribution.tables.function(q.number = "Q16B",
                                                categories = 1:6)

#CAUGHT AND RELEASED
q16c.distr.list <- distribution.tables.function(q.number = "Q16C",
                                                categories = 1:6) 

#SOLD
q16d.distr.list <- distribution.tables.function(q.number = "Q16D",
                                                categories = 1:6) 


#-------------
#DATA SUMMARIES
#-------------  

#CONSUMED AT HOME
q16a.data.sum.list <- data.summaries.function(q.number = "Q16A.mid")

#GIVEN AWAY
q16b.data.sum.list <- data.summaries.function(q.number = "Q16B.mid")

#CAUGHT AND RELEASED
q16c.data.sum.list <- data.summaries.function(q.number = "Q16C.mid")

#SOLD
q16d.data.sum.list <- data.summaries.function(q.number = "Q16D.mid")


#--------------------------------------------------------------------------

# Q17. In 2020, did you ever sell any of the fish you caught?

#DISTRIBUTION
q17.distr.list <- distribution.tables.function(q.number = "Q17",
                                               categories = 1:2)


#CHARACTERISTICS ABOUT THOSE WHO SELL FISH
sell.fish.characteristics <- as.sbs.data.cleaned %>% 
  filter(Q17.chr == "yes") %>% 
  select(Q3.mid.ifelse, Island, Q35.combo, Q14.combo, Q31, Q32, Q40, 
         Q1.combo, Q38.combo, Q39, Q2, Q7.combo, Q8)

no.sell.fish.avg.trips <- as.sbs.data.cleaned %>% 
  filter(Q17.chr == "no") %>% 
  select(Q3.mid.ifelse) %>%
  summarize(no.sell.mean = mean(Q3.mid.ifelse))


#--------------------------------------------------------------------------

# Q18. In 2020, where did you sell your fish?
  #Only for those who sold fish

#FULL SAMPLE
q18.full.sample.where.sell <- where.sell.function(breakdown = "Full.sample")

#ISLAND GROUP
q18.isl.where.sell <- where.sell.function(breakdown = "Island")

#BOTTOMFISH
q18.bottomfish.where.sell <- where.sell.function(breakdown = "Q4.bottomfish")

#FISHER TYPE
q18.crew.where.sell <- where.sell.function(breakdown = "Q1.combo")

#SAMOAN/NON-SAMOAN
q18.samoan.where.sell <- where.sell.function(breakdown = "Q38.combo")

#AGE GROUP
q18.age.where.sell <- where.sell.function(breakdown = "Q35.combo")

#PRIMARY MOTIVATION
q18.pri.mot.where.sell <- where.sell.function(breakdown = "Q14.combo")



#Create output table, just need to get number of observations from above objects
q18.table <- rbind(q18.full.sample.where.sell,
                   q18.isl.where.sell,
                   q18.crew.where.sell,
                   q18.pri.mot.where.sell,
                   q18.age.where.sell,
                   q18.samoan.where.sell,
                   q18.bottomfish.where.sell) %>%
  unique() 

  write.csv(q18.table, paste("Tables/Q18_wheresell.csv"), row.names = F)
  
  
  
  #------------
  #codebook note: 1 = yes, 2 = no
  
  #Fagatogo marketplace
  q18a.distr.list <- distribution.tables.function(q.number = "Q18A.ifelse",
                                                  categories = 1:2) 
  
  #restaurants/stores
  q18b.distr.list <- distribution.tables.function(q.number = "Q18B.ifelse",
                                                  categories = 1:2)
  
  #roadside/farmer's markets
  q18c.distr.list <- distribution.tables.function(q.number = "Q18C.ifelse",
                                                  categories = 1:2)
  
  #friends/neighbors/coworkers
  q18d.distr.list <- distribution.tables.function(q.number = "Q18D.ifelse",
                                                  categories = 1:2)
  
  #other
  q18e.distr.list <- distribution.tables.function(q.number = "Q18E.ifelse",
                                                  categories = 1:2)


#--------------------------------------------------------------------------

# Q19. In 2020, what was the approximate value of all the fish you sold?
  #Only for those who sold fish

#DATA SUMMARIES
q19.data.sum.list <- data.summaries.function(q.number = "Q19A.mid.ifelse")


#--------------------------------------------------------------------------

# Q20. In 2020, what percent of the value of fish sold (question 19) came from 
  # the sale of pelagic fish, deepwater bottomfish, nearshore/shallow bottomfish,
  # and reef fish?

  #Only for those who sold fish. Included 0%

#-------------
#DATA SUMMARIES
#-------------  
#PELAGIC
q20a.data.sum.list <- data.summaries.function(q.number = "Q20A.mid.ifelse")

#DEEPWATER BOTTOMFISH
q20b.data.sum.list <- data.summaries.function(q.number = "Q20B.mid.ifelse")

#NEARSHORE/SHALLOW BOTTOMFISH
q20c.data.sum.list <- data.summaries.function(q.number = "Q20C.mid.ifelse")

#REEF FISH
q20d.data.sum.list <- data.summaries.function(q.number = "Q20D.mid.ifelse")


#--------------------------------------------------------------------------

# Q21. In 2020, after expenses, what percent of your personal income came from 
  # the sale of fish?

  #Include 0% in calculations
  #Only for those who sold fish

#DATA SUMMARIES
q21.data.sum.list <- data.summaries.function(q.number = "Q21.mid.ifelse")

#DISTRIBUTION
q21.distr.list <- distribution.tables.function(q.number = "Q21.ifelse",
                                               categories = 1:6) 

