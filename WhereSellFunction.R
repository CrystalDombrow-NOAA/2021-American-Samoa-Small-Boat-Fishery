#----------------
#FUNCTION FOR Q18 DISTRIBUTION CALCULATIONS: "WHERE DID YOU SELL YOUR FISH?"
#2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------

#Load libraries
library(dplyr)
library(tidyverse)


#Write function
where.sell.function <- function(breakdown){
  
  
  #Rename argument for group_by to work below
  as.where.sell.cleaned <- as.sbs.data.cleaned %>% 
    rename(brk.down = breakdown)
    
  
  #Distribution calculation
  where.sell <- as.where.sell.cleaned %>%
    select(Q18A.ifelse:Q18E.ifelse, brk.down) %>%
    drop_na() %>%
    mutate(across(everything(), ~replace(., . ==  2, 0))) %>%
    group_by(brk.down) %>%
    mutate(tot.faga = sum(Q18A.ifelse),
           tot.rest.st = sum(Q18B.ifelse),
           tot.rd.fm = sum(Q18C.ifelse),
           tot.friends = sum(Q18D.ifelse),
           tot.else = sum(Q18E.ifelse)) %>%
    mutate(total.q18 = (tot.faga + tot.rest.st + tot.rd.fm +
                          tot.friends + tot.else)) %>%
    mutate(faga.perc = round(tot.faga / total.q18 * 100, 1),
           rest.st.perc = round(tot.rest.st / total.q18 * 100, 1),
           rd.fm.perc = round(tot.rd.fm / total.q18 * 100, 1),
           friends.perc = round(tot.friends / total.q18 * 100, 1),
           else.perc = round(tot.else / total.q18 * 100, 1))


    #Rename brk.down to as.sbs.data.cleaned column names, remove unneeded columns,
      #set up dataframe to later rbind in section script for table.
    where.sell <- where.sell %>% 
      select(-Q18A.ifelse, -Q18B.ifelse, -Q18C.ifelse, -Q18D.ifelse, 
             -Q18E.ifelse, -tot.faga, -tot.rest.st, -tot.rd.fm, -tot.friends, 
             -tot.else, -total.q18) %>%
      ungroup()

    
    #Return final dataframe.
    return(where.sell)

}
