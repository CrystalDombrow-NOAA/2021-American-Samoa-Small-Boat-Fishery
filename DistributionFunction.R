#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
  #2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.function <- function(q.number){
  
  #Rename argument for group_by to work below
  as.cleaned.dist.fun <- as.sbs.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #--------------------
  #FULL SAMPLE DISTRIBUTION
  
  #Create object to calculate percentages below
  q.full.sample <- as.cleaned.dist.fun %>%
    select(q.num) %>%
    drop_na()
  
  #Calculate full sample distribution
  q.full.sample.per <-  as.cleaned.dist.fun %>%
    select(q.num) %>%
    drop_na() %>%
    group_by(q.num) %>%
    mutate(percent = round(100 * n() / nrow(q.full.sample), 1)) 

  
  #--------------------
  #ISLAND GROUP DISTRIBUTION
  
  #Create objects for percentages below
  q.tutuila <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Island == "Tutuila") 
  
  q.manua.isl <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Island == "Manua Islands") 
  
  
  #Calculate distribution, by island group
  q.tutuila.per <-  as.cleaned.dist.fun %>%
    select(Island, q.num) %>%
    filter(Island == "Tutuila") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.tutuila), 1))
  
  q.manua.isl.per <-  as.cleaned.dist.fun %>%
    select(Island, q.num) %>%
    filter(Island == "Manua Islands") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.manua.isl), 1))
  
  
  
  #--------------------
  #BOTTOMFISH DISTRIBUTION
  
  #Create objects for percentages below
  q.bottomfish <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>%
    filter(Q4.bottomfish >= 4)


  #Calculate distribution, by bottomfish
  q.bottomfish.per <-  as.cleaned.dist.fun %>%
    select(q.num, Q4.bottomfish) %>%
    filter(Q4.bottomfish >= 4) %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.bottomfish), 1))

  
  
  #--------------------
  #FISHER TYPE DISTRIBUTION
  
  #Create objects for percentages below
  q.crew <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q1.combo == "crew") 
  
  q.boat.owner <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q1.combo == "boat owner") 
  
  
  #Calculate distribution, by fisher type
  q.crew.per <-  as.cleaned.dist.fun %>%
    select(Q1.combo, q.num) %>%
    filter(Q1.combo == "crew") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.crew), 1))
  
  q.boat.owner.per <-  as.cleaned.dist.fun %>%
    select(Q1.combo, q.num) %>%
    filter(Q1.combo == "boat owner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.boat.owner), 1))
  
  
  
  #--------------------
  #SAMOAN DISTRIBUTION
  
  #Create objects for percentages below
  q.samoan <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q38.combo == "samoan") 
  
  q.non.samoan <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q38.combo == "non-samoan") 
  
  
  #Calculate distribution, by Samoan or non-Samoan
  q.samoan.per <-  as.cleaned.dist.fun %>%
    select(Q38.combo, q.num) %>%
    filter(Q38.combo == "samoan") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.samoan), 1))
  
  q.non.samoan.per <-  as.cleaned.dist.fun %>%
    select(Q38.combo, q.num) %>%
    filter(Q38.combo == "non-samoan") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.non.samoan), 1))
  
  
  
  #--------------------
  #AGE DISTRIBUTION
  
  #Create objects for percentages below
  q.under.45 <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q35.combo == "44 and under") 
  
  q.over.45 <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q35.combo == "45 and over") 
  
  
  #Calculate distribution, by age group
  q.under.45.per <-  as.cleaned.dist.fun %>%
    select(Q35.combo, q.num) %>%
    filter(Q35.combo == "44 and under") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.under.45), 1))
  
  q.over.45.per <-  as.cleaned.dist.fun %>%
    select(Q35.combo, q.num) %>%
    filter(Q35.combo == "45 and over") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.over.45), 1))
  
  
  
  #--------------------
  #PRIMARY MOTIVATION DISTRIBUTION
  
  #Create objects for percentages below
  q.commercial <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q14.combo == "commercial") 
  
  q.non.commercial <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q14.combo == "non-commercial") 

  
  
  #Calculate distribution, by primary motivation
  q.commercial.per <-  as.cleaned.dist.fun %>%
    select(Q14.combo, q.num) %>%
    filter(Q14.combo == "commercial") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.commercial), 1))
           
  q.non.commercial.per <- as.cleaned.dist.fun %>%
    select(Q14.combo, q.num) %>%
    filter(Q14.combo == "non-commercial") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.non.commercial), 1))

  
  
  #--------------------
  #Rename q.num to as.sbs.data.cleaned question number 
  
  q.full.sample.per <- q.full.sample.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.tutuila.per <- q.tutuila.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.manua.isl.per <- q.manua.isl.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.bottomfish.per <- q.bottomfish.per %>%
    doBy::renameCol("q.num", q.number)
  
  q.crew.per <- q.crew.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.boat.owner.per <- q.boat.owner.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.samoan.per <- q.samoan.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.non.samoan.per <- q.non.samoan.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.under.45.per <- q.under.45.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.over.45.per <- q.over.45.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.commercial.per <- q.commercial.per %>% 
    doBy::renameCol("q.num", q.number)
  
  q.non.commercial.per <- q.non.commercial.per %>% 
    doBy::renameCol("q.num", q.number)
  
  
  
  #--------------------
  #Return final list of above objects
  q.output.list <- list("q.full.sample.per" = q.full.sample.per, 
                        "q.tutuila.per" = q.tutuila.per,
                        "q.manua.isl.per" = q.manua.isl.per,
                        "q.bottomfish.per" = q.bottomfish.per,
                        "q.crew.per" = q.crew.per,
                        "q.boat.owner.per" = q.boat.owner.per,
                        "q.samoan.per" = q.samoan.per,
                        "q.non.samoan.per" = q.non.samoan.per,
                        "q.under.45.per" = q.under.45.per,
                        "q.over.45.per" = q.over.45.per,
                        "q.commercial.per" = q.commercial.per,
                        "q.non.commercial.per" = q.non.commercial.per)
  
  
  return(q.output.list)

}

