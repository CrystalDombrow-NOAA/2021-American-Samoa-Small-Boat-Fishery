#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
  #2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.tables.function <- function(q.number, categories){
  
  #Rename argument for group_by to work below
  as.cleaned.dist.fun <- as.sbs.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
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
    mutate(percent = round(100 * n() / nrow(q.full.sample), 1)) %>%
    arrange(q.num)
    
  
  
  #Calculate number of observations for table
  q.full.sample.rows <- q.full.sample.per %>%
    nrow()
  
  #Create data frame to later rbind into a table
  q.full.sample.sum <- q.full.sample.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(full.sample = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1, 2)) %>%
    mutate(n = q.full.sample.rows, .before = V1)

  
  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.manua.isl), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.tutuila.rows <- q.tutuila.per %>%
    nrow()
  
  q.manua.isl.rows <- q.manua.isl.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.tutuila.sum <- q.tutuila.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(tutuila = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.tutuila.rows, .before = V1)
  
  q.manua.isl.sum <- q.manua.isl.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(manua.isl = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.manua.isl.rows, .before = V1)

  
  
  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.crew), 1)) %>%
    arrange(q.num)
  
  q.boat.owner.per <-  as.cleaned.dist.fun %>%
    select(Q1.combo, q.num) %>%
    filter(Q1.combo == "boat owner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.boat.owner), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.crew.rows <- q.crew.per %>%
    nrow()
  
  q.boat.owner.rows <- q.boat.owner.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table 
  q.crew.sum <- q.crew.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(crew = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.crew.rows, .before = V1)
  
  q.boat.owner.sum <- q.boat.owner.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(boat.owner = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.boat.owner.rows, .before = V1)
  
  
  
  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.commercial), 1)) %>%
    arrange(q.num)
  
  q.non.commercial.per <- as.cleaned.dist.fun %>%
    select(Q14.combo, q.num) %>%
    filter(Q14.combo == "non-commercial") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.non.commercial), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.commercial.rows <- q.commercial.per %>%
    nrow()
  
  q.non.commercial.rows <- q.non.commercial.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table  
  q.commercial.sum <- q.commercial.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(commercial = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.commercial.rows, .before = V1)
  
  q.non.commercial.sum <- q.non.commercial.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(non.commercial = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.non.commercial.rows, .before = V1)
  
  
  
  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.under.45), 1)) %>%
    arrange(q.num)
  
  q.over.45.per <-  as.cleaned.dist.fun %>%
    select(Q35.combo, q.num) %>%
    filter(Q35.combo == "45 and over") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.over.45), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.under.45.rows <- q.under.45.per %>%
    nrow()
  
  q.over.45.rows <- q.over.45.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table  
  q.under.45.sum <- q.under.45.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(fortyfour.under = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.under.45.rows, .before = V1)
  
  q.over.45.sum <- q.over.45.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(fortyfive.over = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.over.45.rows, .before = V1)
  
  
  
  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.samoan), 1)) %>%
    arrange(q.num)
  
  q.non.samoan.per <-  as.cleaned.dist.fun %>%
    select(Q38.combo, q.num) %>%
    filter(Q38.combo == "non-samoan") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q.non.samoan), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.samoan.rows <- q.samoan.per %>%
    nrow()
  
  q.non.samoan.rows <- q.non.samoan.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table 
  q.samoan.sum <- q.samoan.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(samoan = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.samoan.rows, .before = V1)
  
  q.non.samoan.sum <- q.non.samoan.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(non.samoan = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.non.samoan.rows, .before = V1)
  
  

  #-----------------------------------------------------
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
    mutate(percent = round(100 * n() / nrow(q.bottomfish), 1)) %>%
    arrange(q.num)
  
  
  
  #Calculate number of observations for table
  q.bottomfish.rows <- q.bottomfish.per %>%
    nrow()
  
  
  #Create data frame to later rbind into a table  
  q.bottomfish.sum <- q.bottomfish.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = q.bottomfish.rows, .before = V1)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  #Rbind summary objects together for glory
  q.final.table <- rbind(q.full.sample.sum,
                         q.tutuila.sum,
                         q.manua.isl.sum,
                         q.boat.owner.sum,
                         q.crew.sum,
                         q.commercial.sum,
                         q.non.commercial.sum,
                         q.under.45.sum,
                         q.over.45.sum,
                         q.samoan.sum,
                         q.non.samoan.sum,
                         q.bottomfish.sum)
  
  
  #Save final table
  write.csv(q.final.table, paste("Tables/", q.number, sep = "", 
                                 "_distribution.csv"), 
            row.names = TRUE)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #Return final list of above objects
  q.output.list <- list("q.full.sample.per" = q.full.sample.per, 
                        "q.tutuila.per" = q.tutuila.per,
                        "q.manua.isl.per" = q.manua.isl.per,
                        "q.boat.owner.per" = q.boat.owner.per,
                        "q.crew.per" = q.crew.per,
                        "q.commercial.per" = q.commercial.per,
                        "q.non.commercial.per" = q.non.commercial.per,
                        "q.under.45.per" = q.under.45.per,
                        "q.over.45.per" = q.over.45.per,
                        "q.samoan.per" = q.samoan.per,
                        "q.non.samoan.per" = q.non.samoan.per,
                        "q.bottomfish.per" = q.bottomfish.per)
  
  
  return(q.output.list)

}

