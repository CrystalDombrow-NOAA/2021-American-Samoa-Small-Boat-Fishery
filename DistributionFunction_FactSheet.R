#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.function.factsheet <- function(q.number, categories){
  
  #Rename argument for group_by to work below
  as.cleaned.dist.fun <- as.sbs.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #--------------------
  #Q4.primary.bottomfish
  
  #FULL SAMPLE
  
  #Create objects for percentages below
  q4.pri.bf <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") 
  
  
  #Calculate distribution, by FULL SAMPLE
  q4.pri.bf.full.sample.per <- as.cleaned.dist.fun %>%
    select(Q4.primary.bottomfish, q.num) %>%
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q4.pri.bf), 1))
  
  #Calculate number of observations for table
  q4.pri.bf.full.sample.rows <- q4.pri.bf.full.sample.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q4.pri.bf.full.sample <- q4.pri.bf.full.sample.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q4.primary.bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q4.primary.bottomfish) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q4.pri.bf.full.sample.rows, .before = 1)
  rownames(q4.pri.bf.full.sample) <- "q4.pri.bf.full.sample"
  
  
 #--------- 
  #TUTUILA
  
  #Create objects for percentages below
  q4.pri.bf.tut <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
    filter(Island == "Tutuila")
  
  #Calculate distribution
  q4.pri.bf.tutuila.per <- as.cleaned.dist.fun %>%
    select(Q4.primary.bottomfish, Island, q.num) %>%
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
    filter(Island == "Tutuila") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q4.pri.bf.tut), 1))
  
  #Calculate number of observations for table
  q4.pri.bf.tutuila.rows <- q4.pri.bf.tutuila.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q4.pri.bf.tutuila <- q4.pri.bf.tutuila.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q4.primary.bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q4.primary.bottomfish) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q4.pri.bf.tutuila.rows, .before = 1)
  rownames(q4.pri.bf.tutuila) <- "q4.pri.bf.tutuila"
  
  
  #--------- 
  #MANU'A ISLANDS
  
  #Create objects for percentages below
  q4.pri.bf.man <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
    filter(Island == "Manua Islands")
  
  #Calculate distribution
  q4.pri.bf.manua.isl.per <- as.cleaned.dist.fun %>%
    select(Q4.primary.bottomfish, Island, q.num) %>%
    filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
    filter(Island == "Manua Islands") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q4.pri.bf.man), 1))
  
  #Calculate number of observations for table
  q4.pri.bf.manua.isl.rows <- q4.pri.bf.manua.isl.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q4.pri.bf.manua.isl <- q4.pri.bf.manua.isl.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q4.primary.bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q4.primary.bottomfish) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q4.pri.bf.manua.isl.rows, .before = 1)
  rownames(q4.pri.bf.manua.isl) <- "q4.pri.bf.manua.isl"
  
  
  #--------------------
  #Q10.Q11.any DISTRIBUTION
  
  #FULL SAMPLE
  
  #Create objects for percentages below
  q10.11.any <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") 
  
  #Calculate distribution, by FULL SAMPLE
  q10.11.any.full.sample.per <- as.cleaned.dist.fun %>%
    select(Q10.Q11.any, q.num) %>%
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q10.11.any), 1))
  
  #Calculate number of observations for table
  q10.11.any.full.sample.rows <- q10.11.any.full.sample.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q10.11.any.full.sample <- q10.11.any.full.sample.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q10.11.bottomfish.catch.lbs = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q10.11.bottomfish.catch.lbs) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q10.11.any.full.sample.rows, .before = 1)
  rownames(q10.11.any.full.sample) <- "q10.11.any.full.sample"
  
  
  #--------- 
  #TUTUILA
  
  #Create objects for percentages below
  q10.11.any.tut <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
    filter(Island == "Tutuila")
  
  #Calculate distribution
  q10.11.any.tutuila.per <- as.cleaned.dist.fun %>%
    select(Q10.Q11.any, Island, q.num) %>%
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
    filter(Island == "Tutuila") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q10.11.any.tut), 1))
  
  #Calculate number of observations for table
  q10.11.any.tutuila.rows <- q10.11.any.tutuila.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q10.11.any.tutuila <- q10.11.any.tutuila.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q10.11.bottomfish.catch.lbs = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q10.11.bottomfish.catch.lbs) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q10.11.any.tutuila.rows, .before = 1)
  rownames(q10.11.any.tutuila) <- "q10.11.any.tutuila"
  
  
  #--------- 
  #MANU'A ISLANDS
  
  #Create objects for percentages below
  q10.11.any.man <- as.cleaned.dist.fun %>%
    filter(!is.na(q.num)) %>% 
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
    filter(Island == "Manua Islands")
  
  #Calculate distribution
  q10.11.any.manua.isl.per <- as.cleaned.dist.fun %>%
    select(Q10.Q11.any, Island, q.num) %>%
    filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
    filter(Island == "Manua Islands") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(q10.11.any.man), 1))
  
  #Calculate number of observations for table
  q10.11.any.manua.isl.rows <- q10.11.any.manua.isl.per %>%
    nrow()
  
  #Create data frame to later rbind into a table  
  q10.11.any.manua.isl <- q10.11.any.manua.isl.per %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(Q10.11.bottomfish.catch.lbs = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    select(q.num, Q10.11.bottomfish.catch.lbs) %>%
    tibble::column_to_rownames("q.num") %>%
    t() %>%
    as.data.frame() %>%
    mutate(n = q10.11.any.manua.isl.rows, .before = 1)
  rownames(q10.11.any.manua.isl) <- "q10.11.any.manua.isl"
  
  
  #-----------------------------------------------------
  
  #Rbind summary objects together for glory
  q.final.table <- rbind(q4.pri.bf.full.sample,
                         q4.pri.bf.tutuila,
                         q4.pri.bf.manua.isl,
                         q10.11.any.full.sample,
                         q10.11.any.tutuila,
                         q10.11.any.manua.isl)
  
  
  #Save final table
  write.csv(q.final.table, paste("Tables/", q.number, sep = "", 
                                 "_distribution_factsheet.csv"), 
            row.names = TRUE)
  
  
  #--------------------
  #Return final list of above objects
  q.output.list <- list("q4.pri.bf.full.sample" = q4.pri.bf.full.sample,
                        "q4.pri.bf.tutuila" = q4.pri.bf.tutuila,
                        "q4.pri.bf.manua.isl" = q4.pri.bf.manua.isl,
                        "q10.11.any.full.sample" = q10.11.any.full.sample,
                        "q10.11.any.tutuila" = q10.11.any.tutuila,
                        "q10.11.any.manua.isl" = q10.11.any.manua.isl)
  
  
  return(q.output.list)
  
}

