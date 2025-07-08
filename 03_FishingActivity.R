#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# Fishing Activity section
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

# Q1. Are you a... boat owner/owner-operator/captain/crew?

#DISTRIBUTION
  #Returns an error with Q1.combo; variable is already used in the function
q1.distr.list <- distribution.tables.function(q.number = "Q1",
                                              categories = 1:4) 


#---------------------------------------------------------------------------

# Q2. What type of fishing trips did you take in 2020?

#DISTRIBUTION
q2.distr.list <- distribution.tables.function(q.number = "Q2",
                                              categories = 1:3) 


#---------------------------------------------------------------------------

# Q3. Approximately how many boat fishing trips did you take in 2020? 
  #Excluded 0's, must be at least 1 trip

#DISTRIBUTION
q3.distr.list <- distribution.tables.function(q.number = "Q3B",
                                              categories = 1:6)
  
#DATA SUMMARIES
q3.data.sum.list <- data.summaries.function(q.number = "Q3.mid.ifelse")


#---------------------------------------------------------------------------

# Q4. We understand you may use multiple gears in a trip, please estimate in 
    # 2020, what percent of your BOAT fishing trips were:

#------------------------
# Percentage of fishers using gear types on a boat fishing trip in 2020. 
#------------------------

#SPEARFISHING
q4.spear.yesno.list <- distribution.function(q.number = "Q4A.yesno")

#DEEP BOTTOMFISH
q4.deepbf.yesno.list <- distribution.function(q.number = "Q4B.yesno")

#SHALLOW BOTTOMFISH
q4.shbf.yesno.list <- distribution.function(q.number = "Q4C.yesno")

#TROLLING
q4.troll.yesno.list <- distribution.function(q.number = "Q4D.yesno")

#NETS
q4.nets.yesno.list <- distribution.function(q.number = "Q4E.yesno")

#OTHER
q4.other.yesno.list <- distribution.function(q.number = "Q4F.yesno")


#BOTTOMFISH COMBINED -- for brochure
q4.bf.combined.yesno.list <- distribution.function(q.number = "Q4.bf.yesno")


#------------------------
#DISTRIBUTION TABLES
#------------------------

#SPEARFISHING
q4.spear.distr.list <- distribution.tables.function(q.number = "Q4A",
                                                    categories = 1:6)

#DEEP BOTTOMFISH
q4.deepbf.distr.list <- distribution.tables.function(q.number = "Q4B",
                                                     categories = 1:6)

#SHALLOW BOTTOMFISH
q4.shbf.distr.list <- distribution.tables.function(q.number = "Q4C",
                                                   categories = 1:6)

#TROLLING
q4.troll.distr.list <- distribution.tables.function(q.number = "Q4D",
                                                    categories = 1:6)

#NETS
q4.nets.distr.list <- distribution.tables.function(q.number = "Q4E",
                                                   categories = 1:6)

#OTHER
q4.other.distr.list <- distribution.tables.function(q.number = "Q4F",
                                                    categories = 1:6)

#------------------------
#BOTTOMFISH TRIPS %
#------------------------

#Calculate average percent of bottomfish fisher trips that were bottomfishing
q4.bottomfish.trips <- as.sbs.data.cleaned %>%
  select(Q4.bottomfish, Survey, Q4B, Q4C, Q4B.mid, Q4C.mid, Q3.mid.ifelse) %>% 
  filter(Q4.bottomfish == "bottomfish") %>% 
  mutate(Q4BC.tot.percent = select(., Q4B.mid:Q4C.mid) %>% 
           rowSums(na.rm = T)) %>%
  mutate(Q4BC.tot.percent = Q4BC.tot.percent * .01) %>%
  mutate(bf.trips = Q4BC.tot.percent * Q3.mid.ifelse) %>%
  mutate(avg.bf.trips = round(mean(bf.trips)))


#---------------------------------------------------------------------------

# Q7. In 2020, what percent of your fishing time occurred in local and 
  # federal jurisdiction?
  #Michel included 0% in total calculations. 

#-------------------
# DATA SUMMARIES
#-------------------

#LOCAL WATERS
q7.local.data.sum.list <- data.summaries.function(q.number = "Q7A.mid")

#FEDERAL WATERS
q7.federal.data.sum.list <- data.summaries.function(q.number = "Q7B.mid")


#-------------------
# DISTRIBUTION
#-------------------

#LOCAL WATERS
q7.local.distr.list <- distribution.tables.function(q.number = "Q7A",
                                                    categories = 1:6)

#FEDERAL WATERS
q7.federal.distr.list <- distribution.tables.function(q.number = "Q7B",
                                                      categories = 1:6)

#-------------------
# COMBO DISTRIBUTION
#-------------------

#Local, federal, both local and federal waters
q7.combo.distr.list <- distribution.tables.function(q.number = "Q7.combo",
                                                    categories = 1:3)


#---------------------------------------------------------------------------

# Q8. How many people in total, including yourself, are on board for an average 
  # fishing trip? 

#DATA SUMMARIES
q8.data.sum.list <- data.summaries.function(q.number = "Q8")


#---------------------------------------------------------------------------

# Q9. In 2020, approximately how many total pounds of pelagic fish did you catch?

#DATA SUMMARIES
q9.data.sum.list <- data.summaries.function(q.number = "Q9.ifelse") 

#DISTRIBUTION
  #Include 0’s because they may be catching other types.
q9.distr.list <- distribution.tables.function(q.number = "Q9A",
                                              categories = 1:6)


#---------------------------------------------------------------------------

# Q10. In 2020, approx. how many total pounds of deepwater bottomfish did you catch?

#DATA SUMMARIES
q10.data.sum.list <- data.summaries.function(q.number = "Q10.ifelse")

#DISTRIBUTION
 #Include 0’s because they may be catching other types.
q10.distr.list <- distribution.tables.function(q.number = "Q10A",
                                               categories = 1:6)


#---------------------------------------------------------------------------

# Q11. In 2020, approximately how many total pounds of shallow bottomfish did you catch?

#DATA SUMMARIES
q11.data.sum.list <- data.summaries.function(q.number = "Q11.ifelse")

#DISTRIBUTION
 #Include 0’s because they may be catching other types.
q11.distr.list <- distribution.tables.function(q.number = "Q11A",
                                               categories = 1:6)


#---------------------------------------------------------------------------

# Q12. In 2020, approximately how many total pounds of reef fish did you catch?

#DATA SUMMARIES
q12.data.sum.list <- data.summaries.function(q.number = "Q12.ifelse")

#DISTRIBUTION
  #Include 0’s because they may be catching other types.
q12.distr.list <- distribution.tables.function(q.number = "Q12A",
                                               categories = 1:6) 


#---------------------------------------------------------------------------

#Q9-12: Aggregate catch sums, to compare to 2020 ACL's & creel estimates.

q9.12.aggregate.catch <- as.sbs.data.cleaned %>%
  select(Q9.ifelse:Q12.ifelse) %>%
  mutate(pelagics = round(sum(Q9.ifelse, na.rm = T)),
         deep.bf = round(sum(Q10.ifelse, na.rm = T)),
         shallow.bf = round(sum(Q11.ifelse, na.rm = T)),
         reef.fish = round(sum(Q12.ifelse, na.rm = T))) %>%
  select(-Q9.ifelse, -Q10.ifelse, -Q11.ifelse, -Q12.ifelse) %>%
  unique() %>%
  mutate(total.bf = sum(deep.bf, shallow.bf))
  

#---------------------------------------------------------------------------

# Q13. In 2020, what percent of your boat fishing trips were sampled by the DMWR 
  # creel survey program?
  #Include 0%

#DISTRIBUTION
q13.distr.list <- distribution.tables.function(q.number = "Q13",
                                               categories = 1:6)


#---------------------------------------------------------------------------

# Q31. In 2020, what was the primary gear usage for your most common trip? 

#DISTRIBUTION
q31.distr.list <- distribution.tables.function(q.number = "Q31",
                                               categories = 1:6) 


#---------------------------------------------------------------------------

# Q32. In 2020, what was the secondary gear usage for your most common trip? 

#DISTRIBUTION
q32.distr.list <- distribution.tables.function(q.number = "Q32",
                                               categories = 1:6) 


#---------------------------------------------------------------------------

# Q43. What are the top three (3) species you target… 

#-----------
#To sell?

#q43a.distr.list <- distribution.function(q.number = "Q43A") 
q43a.spp.distr.list <- distribution.function(q.number = "Q43A.spp") 

#q43b.distr.list <- distribution.function(q.number = "Q43B") 
q43b.spp.distr.list <- distribution.function(q.number = "Q43B.spp") 

#q43c.distr.list <- distribution.function(q.number = "Q43C") 
q43c.spp.distr.list <- distribution.function(q.number = "Q43C.spp") 


#-----------
#To keep for self-consumption?

#q43d.distr.list <- distribution.function(q.number = "Q43D") 
q43d.spp.distr.list <- distribution.function(q.number = "Q43D.spp") 

#q43e.distr.list <- distribution.function(q.number = "Q43E") 
q43e.spp.distr.list <- distribution.function(q.number = "Q43E.spp") 

#q43f.distr.list <- distribution.function(q.number = "Q43F") 
q43f.spp.distr.list <- distribution.function(q.number = "Q43F.spp") 


#-----------
# To give away?

#q43g.distr.list <- distribution.function(q.number = "Q43G") 
q43g.spp.distr.list <- distribution.function(q.number = "Q43G.spp") 

#q43h.distr.list <- distribution.function(q.number = "Q43H") 
q43h.spp.distr.list <- distribution.function(q.number = "Q43H.spp") 

#q43i.distr.list <- distribution.function(q.number = "Q43I") 
q43i.spp.distr.list <- distribution.function(q.number = "Q43I.spp") 


#--------------
#--------------
#Combined primary, secondary, tertiary for Mia//Fish Flow, July 2024

#Combine columns, individual species then groups
Q43.sell <- as.sbs.data.cleaned %>% 
  select(Q43A, Q43B, Q43C)


stacked_df <- gather(Q43.sell, value = "sell")

# Select only the Stacked_Column
stacked_df <- stacked_df[, "sell", drop = FALSE]

q43.sell.table <- distribution.function(q.number = "Q43I.spp") 

              
              
