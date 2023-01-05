#----------------
# FUNCTION FOR DATA SUMMARY CALCULATIONS
# 2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------


#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)


#Write function
data.summaries.function <- function(q.number){
  
  #Rename argument for group_by to work below
  as.cleaned.data.sum.fun <- as.sbs.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-------------------------------------
  #-------------------------------------
  #FULL SAMPLE
  
  #Calculate data summaries
  q.data.sum.full.sample <- as.cleaned.data.sum.fun %>%
    select(q.num, Full.sample) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of observations for table
  q.full.sample.rows <- q.data.sum.full.sample %>%
    drop_na() %>%
    nrow()
  
  #Create data frame to later rbind into a table
  q.full.sample.sum <- q.data.sum.full.sample %>%
    mutate(n = q.full.sample.rows, .before = mean) %>%
    mutate(breakdown = Full.sample, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.full.sample.sum <- q.full.sample.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #ISLAND GROUP
  
  #Calculate data summaries
  q.data.sum.isl <- as.cleaned.data.sum.fun %>%
    select(q.num, Island) %>%
    group_by(Island) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))

  
  #Calculate number of Tutuila observations for table
  q.tutuila.rows <- q.data.sum.isl %>%
    filter(Island == "Tutuila") %>%
    drop_na() %>%
    nrow() 
    
  #Calculate number of Manu'a Islands observations for table
  q.manua.isl.rows <- q.data.sum.isl %>%
    filter(Island == "Manua Islands") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.islands.sum <- q.data.sum.isl %>%
    mutate(n = (ifelse(Island == "Tutuila", q.tutuila.rows, q.manua.isl.rows))) %>%
    mutate(breakdown = Island, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.islands.sum <- q.islands.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #BOTTOMFISH
  
  #Calculate data summaries
  q.data.sum.bottomfish <- as.cleaned.data.sum.fun %>%
    select(q.num, Q4.bottomfish) %>%
    drop_na() %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of bottomfish observations for table
  q.bottomfish.rows <- q.data.sum.bottomfish %>%
    drop_na() %>%
    nrow() 

  #Create data frame to later rbind into a table
  q.bottomfish.sum <- q.data.sum.bottomfish %>%
    mutate(n = q.bottomfish.rows, .before = mean) %>%
    mutate(breakdown = Q4.bottomfish, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.bottomfish.sum <-  q.bottomfish.sum[-c(1:2)] %>%
    unique()
  

  #-------------------- 
  #FISHER TYPE
  
  #Calculate data summaries
  q.data.sum.crew <- as.cleaned.data.sum.fun %>%
    select(q.num, Q1.combo) %>%
    drop_na() %>%
    group_by(Q1.combo) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of boat owner observations for table
  q.boat.owner.rows <- q.data.sum.crew %>%
    filter(Q1.combo == "boat owner") %>%
    drop_na() %>%
    nrow() 
  
  #Calculate number of crew observations for table
  q.crew.rows <- q.data.sum.crew %>%
    filter(Q1.combo == "crew") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.crew.sum <- q.data.sum.crew %>%
    mutate(n = (ifelse(Q1.combo == "crew", q.crew.rows, q.boat.owner.rows))) %>%
    mutate(breakdown = Q1.combo, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.crew.sum <- q.crew.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #RACE
  
  #Calculate data summaries
  q.data.sum.race <- as.cleaned.data.sum.fun %>%
    select(q.num, Q38.combo) %>%
    drop_na() %>%
    group_by(Q38.combo) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of Samoan observations for table
  q.samoan.rows <- q.data.sum.race %>%
    filter(Q38.combo == "samoan") %>%
    drop_na() %>%
    nrow() 
  
  #Calculate number of Non-Samoan observations for table
  q.non.samoan.rows <- q.data.sum.race %>%
    filter(Q38.combo == "non-samoan") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.race.sum <- q.data.sum.race %>%
    mutate(n = (ifelse(Q38.combo == "samoan", q.samoan.rows, q.non.samoan.rows))) %>%
    mutate(breakdown = Q38.combo, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.race.sum <- q.race.sum [-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #AGE GROUP
  
  #Calculate data summaries
  q.data.sum.age <- as.cleaned.data.sum.fun %>%
    select(q.num, Q35.combo) %>%
    drop_na() %>%
    group_by(Q35.combo) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of 44 and under observations for table
  q.44.under.rows <- q.data.sum.age %>%
    filter(Q35.combo == "44 and under") %>%
    drop_na() %>%
    nrow() 
  
  #Calculate number of 45 and over observations for table
  q.45.over.rows <- q.data.sum.age %>%
    filter(Q35.combo == "45 and over") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.age.sum <- q.data.sum.age %>%
    mutate(n = (ifelse(Q35.combo == "45 and over", q.45.over.rows, q.44.under.rows))) %>%
    mutate(breakdown = Q35.combo, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.age.sum <- q.age.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #PRIMARY MOTIVATION
  
  #Calculate data summaries
  q.data.sum.pri.mot <- as.cleaned.data.sum.fun %>%
    select(q.num, Q14.combo) %>%
    drop_na() %>%
    group_by(Q14.combo) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))

  
  #Calculate number of commercial observations for table
  q.commercial.rows <- q.data.sum.pri.mot  %>%
    filter(Q14.combo == "commercial") %>%
    drop_na() %>%
    nrow() 
  
  #Calculate number of non-commercial observations for table
  q.non.commercial.rows <- q.data.sum.pri.mot %>%
    filter(Q14.combo == "non-commercial") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.pri.mot.sum <- q.data.sum.pri.mot %>%
    mutate(n = (ifelse(Q14.combo == "commercial", q.commercial.rows, q.non.commercial.rows))) %>%
    mutate(breakdown = Q14.combo, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.pri.mot.sum <- q.pri.mot.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------------------------
  #-------------------------------------
  #CREATE OUTPUT TABLE
  
  #Rbind together, for glory
  q.full.isl.sum <- rbind(q.full.sample.sum, 
                          q.islands.sum,
                          q.crew.sum,
                          q.pri.mot.sum,
                          q.age.sum,
                          q.race.sum,
                          q.bottomfish.sum) %>%
    t()
  
  #Save final table 
  write.csv(q.full.isl.sum, na = "0.0", paste("Tables/", q.number, sep = "", 
                                              "_datasummaries.csv"), 
            row.names = T)
  

  #-------------------------------------
  #-------------------------------------
  #Return final list of above objects
  q.output.list <- list("q.data.sum.full.sample" = q.data.sum.full.sample,
                        "q.data.sum.isl" = q.data.sum.isl,
                        "q.data.sum.bottomfish" = q.data.sum.bottomfish,
                        "q.data.sum.crew" = q.data.sum.crew,
                        "q.data.sum.race" = q.data.sum.race,
                        "q.data.sum.age" = q.data.sum.age,
                        "q.data.sum.pri.mot" = q.data.sum.pri.mot)
  
  
  return(q.output.list)
  
}

