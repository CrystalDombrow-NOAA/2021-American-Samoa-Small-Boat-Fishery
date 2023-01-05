#######################
# 2021 American Samoa small boat fishery cost-earnings survey
# CREW CONSIDERATIONS section
# Calculations for section paragraphs
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------
#General crew stats

crew.stats <- as.sbs.data.cleaned %>% 
  select(Survey, Island, Q35.combo, Q14.combo, Q15A:Q15D, Q31C, Q32C, Q33A:Q33L,
         Q21, Q40, Q1.combo, Q4.bottomfish, Q38.combo) %>% 
  filter(Q1.combo == "crew")


#---------------------------------------------------------------------------

#Q15. In 2020, I: kept all the fish I caught; kept/received some % of total fish 
  #caught; kept/received some % of trip revenue; Don’t know; Different every 
  #time; Other

q15.share.with.crew <- as.sbs.data.cleaned %>% 
  select(Q1.combo, Q31C, Q32C, Q15A:Q15D) %>% 
  filter(Q1.combo == "boat owner")

