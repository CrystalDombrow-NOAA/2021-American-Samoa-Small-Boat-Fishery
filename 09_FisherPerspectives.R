#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# FISHER PERSPECTIVES section
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
#-------------------------------------

# Q41. Given your experience, do you think in the next year more people will be 
  # going…


#Q41A. Pelagic fishing
q41a.distr.list <- distribution.tables.function(q.number = "Q41A",
                                                categories = 1:2) 

#Q41B. Deepwater bottomfish fishing
q41b.distr.list <- distribution.tables.function(q.number = "Q41B",
                                                categories = 1:2) 

#Q41C. Nearshore/shallow water bottomfish fishing
q41c.distr.list <- distribution.tables.function(q.number = "Q41C",
                                                categories = 1:2) 

#Q41D. Reef fishing
q41d.distr.list <- distribution.tables.function(q.number = "Q41D",
                                                categories = 1:2) 

#---------------------------

# Q42. Why do you feel this way?
q42.open.ended <- as.sbs.data.cleaned %>%
  select(Q41A:Q41D, Q42)

write.csv(q42.open.ended, "Tables/Q42_openended.csv", row.names = F)


#---------------------------------------------------------------------------

# Q45. How important are the following for managing fisheries in 
  # American Samoa?


# Q45A. Rules are followed and enforced
q45a.distr.list <- distribution.tables.function(q.number = "Q45A",
                                                categories = 1:5) 

# Q45B. My voice is included in decision making
q45b.distr.list <- distribution.tables.function(q.number = "Q45B",
                                                categories = 1:5) 

# Q45C. Managers know how many fish there are
q45c.distr.list <- distribution.tables.function(q.number = "Q45C",
                                                categories = 1:5) 

# Q45D. Managers know how healthy the reef / other habitats are
q45d.distr.list <- distribution.tables.function(q.number = "Q45D",
                                                categories = 1:5) 

# Q45E. Managers know about the fisher(men) and fishing community 
  # (income, culture, etc.)
q45e.distr.list <- distribution.tables.function(q.number = "Q45E",
                                                categories = 1:5) 

# Q45F. Managers build or maintain fisheries infrastructure 
  # (boat ramps, harbors, etc.)
q45f.distr.list <- distribution.tables.function(q.number = "Q45F",
                                                categories = 1:5) 

# Q45G. Managers build or maintain fisheries infrastructure 
  # (boat ramps, harbors, etc.)
  #Only 1 response, nothing filled in for Q45H to explain the "other"
q45g.distr.list <- distribution.tables.function(q.number = "Q45G",
                                                categories = 1:5) 



#-------------------------------------------------------------------------

# Q46. Please state how much you agree or disagree that the following management 
  # is being done well


# Q46A. Rules are followed and enforced
q46a.distr.list <- distribution.tables.function(q.number = "Q46A",
                                                categories = 1:5) 

# Q46B. My voice is included in decision making
q46b.distr.list <- distribution.tables.function(q.number = "Q46B",
                                                categories = 1:5) 

# Q46C. Managers know how many fish there are
q46c.distr.list <- distribution.tables.function(q.number = "Q46C",
                                                categories = 1:5) 

# Q46D. Managers know how healthy the reef / other habitats are
q46d.distr.list <- distribution.tables.function(q.number = "Q46D",
                                                categories = 1:5) 

# Q46E. Managers know about the fisher(men) and fishing community 
  # (income, culture, etc.)
q46e.distr.list <- distribution.tables.function(q.number = "Q46E",
                                                categories = 1:5) 

# Q46F. Managers build or maintain fisheries infrastructure 
  # (boat ramps, harbors, etc.)
q46f.distr.list <- distribution.tables.function(q.number = "Q46F",
                                                categories = 1:5) 

# Q46G. Other
  #Only 2 responses, nothing filled in for Q46H to explain the "other"
q46g.distr.list <- distribution.tables.function(q.number = "Q46G",
                                                categories = 1:5) 

#---------------------------------------------------------------------------

#Need gap analysis for report, Q45-46.

q45.46.need.gap.analysis <- as.sbs.data.cleaned %>%
  select(Q45A:Q46H) %>%
  mutate(Q45A = round(mean(Q45A, na.rm = T), 1),
         Q45B = round(mean(Q45B, na.rm = T), 1),
         Q45C = round(mean(Q45C, na.rm = T), 1),
         Q45D = round(mean(Q45D, na.rm = T), 1),
         Q45E = round(mean(Q45E, na.rm = T), 1),
         Q45F = round(mean(Q45F, na.rm = T), 1),
         Q45G = round(mean(Q45G, na.rm = T), 1),
         Q45H = round(mean(Q45H, na.rm = T), 1),
         Q46A = round(mean(Q46A, na.rm = T), 1),
         Q46B = round(mean(Q46B, na.rm = T), 1),
         Q46C = round(mean(Q46C, na.rm = T), 1),
         Q46D = round(mean(Q46D, na.rm = T), 1),
         Q46E = round(mean(Q46E, na.rm = T), 1),
         Q46F = round(mean(Q46F, na.rm = T), 1),
         Q46G = round(mean(Q46G, na.rm = T), 1),
         Q46H = round(mean(Q46H, na.rm = T), 1)) %>%
  unique() %>%
  mutate(avg.importance = mean(Q45A:Q45F),
         avg.satisfaction = mean(Q46A:Q46G))


#---------------------------------------------------------------------------

# Q42. Why do you feel this way? (more or less future fishing from Q41)

# Q47. Do you have any suggestions for how American Samoa’s fisheries should be 
  # managed or topics that you feel need further study?

# Q48. How have you changed your fishing activities due to COVID-19? 
  # For example, were any of your survey responses different than they would 
  # have been in a normal year?

# Q49. What are the main reasons you made those changes?


q42.47.48.49.open.ended <- as.sbs.data.cleaned %>% 
  select(Survey, Q42, Q47:Q49)

write.csv(q42.47.48.49.open.ended, "Tables/Q42.47.48.49_openended.csv", 
          row.names = F)

