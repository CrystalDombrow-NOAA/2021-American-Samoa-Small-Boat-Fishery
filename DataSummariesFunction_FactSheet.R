#----------------
# FUNCTION FOR DATA SUMMARY CALCULATIONS
# 2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------


#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)


#Write function
data.summaries.function.factsheet <- function(q.number){
  
  #Rename argument for group_by to work below
  as.cleaned.data.sum.fun <- as.sbs.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-------------------------------------
  #-------------------------------------
  #Q4.primary.bottomfish
  
  #FULL SAMPLE
    #Calculate data summaries
    q4.pri.bf.full.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q4.primary.bottomfish) %>%
      filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    
    #Calculate number of observations for table
    q4.pri.bf.rows <- q4.pri.bf.full.sum %>%
      drop_na() %>%
      nrow()
    
    #Create data frame to later rbind into a table
    q4.pri.bf.full.sample <- q4.pri.bf.full.sum %>%
      mutate(n = q4.pri.bf.rows, .before = mean) %>%
      mutate(breakdown = Q4.primary.bottomfish, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q4.pri.bf.full.sample <- q4.pri.bf.full.sample[-c(1:2)] %>%
      unique()
    
  
#--------
    
  #TUTILA
    #Calculate data summaries
    q4.pri.bf.tutuila.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q4.primary.bottomfish, Island) %>%
      filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
      filter(Island == "Tutuila") %>%     
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    #Calculate number of Tutuila observations for table
    q4.tutuila.rows <- q4.pri.bf.tutuila.sum %>%
      filter(Island == "Tutuila") %>%
      drop_na() %>%
      nrow() 
    
    #Create data frame to later rbind into a table
    q4.pri.bf.tutuila <- q4.pri.bf.tutuila.sum %>%
      mutate(n = q4.tutuila.rows, .before = mean) %>%
      mutate(breakdown = Island, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q4.pri.bf.tutuila <- q4.pri.bf.tutuila[-c(1:2)] %>%
      select(-Island) %>%
      unique()
    

 #--------
    
    #MANU'A ISLANDS
    
    #Calculate data summaries
    q4.pri.bf.manua.isl.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q4.primary.bottomfish, Island) %>%
      filter(Q4.primary.bottomfish == "Q4.primary.bottomfish") %>%
      filter(Island == "Manua Islands") %>%     
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    #Calculate number of Manu'a Islands observations for table
    q4.manua.isl.rows <- q4.pri.bf.manua.isl.sum %>%
      filter(Island == "Manua Islands") %>%
      drop_na() %>%
      nrow()
    
    #Create data frame to later rbind into a table
    q4.pri.bf.manua.isl <- q4.pri.bf.manua.isl.sum %>%
      mutate(n = q4.manua.isl.rows, .before = mean) %>%
      mutate(breakdown = Island, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q4.pri.bf.manua.isl <- q4.pri.bf.manua.isl[-c(1:2)] %>%
      select(-Island) %>%
      unique()
    
    #----------------------------------------------------------
    
    #Q10.Q11.any
    
    #FULL SAMPLE
    #Calculate data summaries
    q10.11.any.full.sample.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q10.Q11.any) %>%
      filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    
    #Calculate number of observations for table
    q10.11.any.full.sample.rows <- q10.11.any.full.sample.sum %>%
      drop_na() %>%
      nrow()
    
    #Create data frame to later rbind into a table
    q10.11.any.full.sample <- q10.11.any.full.sample.sum %>%
      mutate(n = q10.11.any.full.sample.rows, .before = mean) %>%
      mutate(breakdown = Q10.Q11.any, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q10.11.any.full.sample <- q10.11.any.full.sample[-c(1:2)] %>%
      unique()
    
    
    #--------
    
    #TUTILA
    #Calculate data summaries
    q10.11.any.tutuila.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q10.Q11.any, Island) %>%
      filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
      filter(Island == "Tutuila") %>%     
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    #Calculate number of Tutuila observations for table
    q10.11.any.tutuila.rows <- q10.11.any.tutuila.sum %>%
      filter(Island == "Tutuila") %>%
      drop_na() %>%
      nrow() 
    
    #Create data frame to later rbind into a table
    q10.11.any.tutuila <- q10.11.any.tutuila.sum %>%
      mutate(n = q10.11.any.tutuila.rows, .before = mean) %>%
      mutate(breakdown = Island, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q10.11.any.tutuila <- q10.11.any.tutuila[-c(1:2)] %>%
      select(-Island) %>%
      unique()
    
    
    #--------
    
    #MANU'A ISLANDS
    
    #Calculate data summaries
    q10.11.any.manua.isl.sum <- as.cleaned.data.sum.fun %>%
      select(q.num, Q10.Q11.any, Island) %>%
      filter(Q10.Q11.any == "Q10.11.bottomfish.catch.lbs") %>%
      filter(Island == "Manua Islands") %>%     
      mutate(mean = round(mean(q.num, na.rm = T), 1),
             std.err = round(std.error(q.num, na.rm = T), 1),
             med = round(median(q.num, na.rm = T), 1),
             min = round(min(q.num, na.rm = T), 1),
             max = round(max(q.num, na.rm = T), 1))
    
    #Calculate number of Manu'a Islands observations for table
    q10.11.any.manua.isl.rows <- q10.11.any.manua.isl.sum %>%
      filter(Island == "Manua Islands") %>%
      drop_na() %>%
      nrow()
    
    #Create data frame to later rbind into a table
    q10.11.any.manua.isl <- q10.11.any.manua.isl.sum %>%
      mutate(n = q10.11.any.manua.isl.rows, .before = mean) %>%
      mutate(breakdown = Island, .before = n)
    
    #Remove unneeded rows and select unique breakdown values
    q10.11.any.manua.isl <-  q10.11.any.manua.isl[-c(1:2)] %>%
      select(-Island) %>%
      unique()
  
  #-------------------------------------
  #-------------------------------------
  #CREATE OUTPUT TABLE
  
  #Rbind together, for glory
  q.full.isl.sum <- rbind(q4.pri.bf.full.sample,
                          q4.pri.bf.tutuila,
                          q4.pri.bf.manua.isl,
                          q10.11.any.full.sample,
                          q10.11.any.tutuila,
                          q10.11.any.manua.isl) %>%
    t()
  
  #Save final table 
  write.csv(q.full.isl.sum, na = "0.0", paste("Tables/", q.number, sep = "", 
                                              "_datasummaries_factsheet.csv"), 
            row.names = T)
  
  
  #-------------------------------------
  #-------------------------------------
  #Return final list of above objects
  q.output.list <- list("q4.pri.bf.full.sample" = q4.pri.bf.full.sample,
                        "q4.pri.bf.tutuila" = q4.pri.bf.tutuila,
                        "q4.pri.bf.manua.isl" = q4.pri.bf.manua.isl,
                        "q10.11.any.full.sample" = q10.11.any.full.sample,
                        "q10.11.any.tutuila" = q10.11.any.tutuila,
                        "q10.11.any.manua.isl" = q10.11.any.manua.isl)
  
  
  return(q.output.list)
  
}

