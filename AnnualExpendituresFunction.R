#----------------
#FUNCTION FOR ANNUAL EXPENDITURES CALCULATIONS
#2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------

#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)


#Write function
annual.expenditures.function <- function(breakdown){
  
  
  #Rename argument for group_by to work below
  as.annual.exp.cleaned <- as.sbs.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  
  #Set up dataframe for calculations below
  annual.expenditures.colnames <- as.annual.exp.cleaned %>%
    select(Q22.chr, Q33A, Q33D, Q33E, Q33G, Q33H:Q33J, brk.down) %>%
    #Filter to boat owners only
    filter(Q22.chr == "yes") %>%
    drop_na() %>%
    rename(boat.ins = Q33A,
           loan.pmt = Q33D,
           mooring = Q33E,
           gear = Q33G,
           boat.trailer = Q33H,
           fees = Q33I,
           fin.services = Q33J)

  
  #--------------------
  #Calculate data summaries.
  
  #EXCLUDING ZEROS
  q33.excluding.zeros <- annual.expenditures.colnames %>%
    na_if("0") %>%
    group_by(brk.down) %>%
           #LOAN PAYMENTS
    mutate(med.loan.pmt = round(median(loan.pmt, na.rm = T), 1),
           loan.pmt.std.err = round(std.error(loan.pmt, na.rm = T), 1),
           avg.loan.pmt = round(mean(loan.pmt, na.rm = T), 1),
           min.loan.pmt = round(min(loan.pmt, na.rm = T), 1),
           max.loan.pmt = round(max(loan.pmt, na.rm = T), 1),
           #GEAR REPLACEMENT/REPAIR FROM WEAR & TEAR
           med.gear = round(median(gear, na.rm = T), 1),
           gear.std.err = round(std.error(gear, na.rm = T), 1),
           avg.gear = round(mean(gear, na.rm = T), 1),
           min.gear = round(min(gear, na.rm = T), 1),
           max.gear = round(max(gear, na.rm = T), 1),
           #BOAT & TRAILER REPAIR, MAINTENANCE, & IMPROVEMENTS
           med.boat.trail = round(median(boat.trailer, na.rm = T), 1),
           boat.tr.std.err = round(std.error(boat.trailer, na.rm = T), 1),
           avg.boat.trail = round(mean(boat.trailer, na.rm = T), 1),
           min.boat.trail = round(min(boat.trailer, na.rm = T), 1),
           max.boat.trail = round(max(boat.trailer, na.rm = T), 1),
           #FEES
           med.fees = round(median(fees, na.rm = T), 1),
           fees.std.err = round(std.error(fees, na.rm = T), 1),
           avg.fees = round(mean(fees, na.rm = T), 1),
           min.fees = round(min(fees, na.rm = T), 1),
           max.fees = round(max(fees, na.rm = T), 1),
           #FINANCIAL SERVICES
           med.fin.serv = round(median(fin.services, na.rm = T), 1),
           fin.serv.std.err = round(std.error(fin.services, na.rm = T), 1),
           avg.fin.serv = round(mean(fin.services, na.rm = T), 1),  
           min.fin.serv = round(min(fin.services, na.rm = T), 1),
           max.fin.serv = round(max(fin.services, na.rm = T), 1)) %>%
           #TOTAL ANNUAL EXPENDITURES
           #Leaves out boat insurance and mooring because no responses.
           #Code for Manu'a Islands and others with full responses.
    mutate(med.tot = sum(unique(med.loan.pmt + med.gear + med.boat.trail +
                                 med.fees + med.fin.serv))) %>%
    mutate(tot.std.err = std.error(loan.pmt + gear + boat.trailer + fees +
                                         fin.services)) %>%
    mutate(avg.tot = sum(unique(avg.loan.pmt + avg.gear + avg.boat.trail +
                                 avg.fees + avg.fin.serv))) %>%
    mutate(min.tot = sum(unique(min.loan.pmt + min.gear + min.boat.trail +
                                 min.fees + min.fin.serv))) %>%
    mutate(max.tot = sum(unique(max.loan.pmt + max.gear + max.boat.trail +
                                 max.fees + max.fin.serv)))
  
  #Code for Tutuila; so many NAs that totals aren't running. Comment out when 
    #not using.
  # mutate(med.tot = sum(unique(med.gear + med.boat.trail + med.fees))) %>%
  # mutate(tot.std.err = std.error(gear + boat.trailer + fees)) %>%
  # mutate(avg.tot = sum(unique(avg.gear + avg.boat.trail + avg.fees))) %>%
  # mutate(min.tot = sum(unique(min.gear + min.boat.trail + min.fees))) %>%
  # mutate(max.tot = sum(unique(max.gear + max.boat.trail + max.fees)))


  
  #------------------
  #INCLUDING ZEROS
  
  q33.including.zeros <- annual.expenditures.colnames %>%
    group_by(brk.down) %>%
           #BOAT INSURANCE
    mutate(med.loan.pmt = round(median(loan.pmt, na.rm = T), 1),
           loan.pmt.std.err = round(std.error(loan.pmt, na.rm = T), 1),
           avg.loan.pmt = round(mean(loan.pmt, na.rm = T), 1),
           min.loan.pmt = round(min(loan.pmt, na.rm = T), 1),
           max.loan.pmt = round(max(loan.pmt, na.rm = T), 1),
           #GEAR REPLACEMENT/REPAIR FROM WEAR & TEAR
           med.gear = round(median(gear, na.rm = T), 1),
           gear.std.err = round(std.error(gear, na.rm = T), 1),
           avg.gear = round(mean(gear, na.rm = T), 1),
           min.gear = round(min(gear, na.rm = T), 1),
           max.gear = round(max(gear, na.rm = T), 1),
           #BOAT & TRAILER REPAIR, MAINTENANCE, & IMPROVEMENTS
           med.boat.trail = round(median(boat.trailer, na.rm = T), 1),
           boat.tr.std.err = round(std.error(boat.trailer, na.rm = T), 1),
           avg.boat.trail = round(mean(boat.trailer, na.rm = T), 1),
           min.boat.trail = round(min(boat.trailer, na.rm = T), 1),
           max.boat.trail = round(max(boat.trailer, na.rm = T), 1),
           #FEES
           med.fees = round(median(fees, na.rm = T), 1),
           fees.std.err = round(std.error(fees, na.rm = T), 1),
           avg.fees = round(mean(fees, na.rm = T), 1),
           min.fees = round(min(fees, na.rm = T), 1),
           max.fees = round(max(fees, na.rm = T), 1),
           #FINANCIAL SERVICES
           med.fin.serv = round(median(fin.services, na.rm = T), 1),
           fin.serv.std.err = round(std.error(fin.services, na.rm = T), 1),
           avg.fin.serv = round(mean(fin.services, na.rm = T), 1),  
           min.fin.serv = round(min(fin.services, na.rm = T), 1),
           max.fin.serv = round(max(fin.services, na.rm = T), 1),
           #TOTAL ANNUAL EXPENDITURES
           #Leaves out boat insurance and mooring because no responses.
           med.tot = sum(unique(med.loan.pmt + med.gear + med.boat.trail + 
                                 med.fees + med.fin.serv))) %>%
    mutate(tot.std.err = std.error(loan.pmt + gear + boat.trailer + fees + 
                                           fin.services)) %>%
    mutate(avg.tot = sum(unique(avg.loan.pmt + avg.gear + avg.boat.trail + 
                                 avg.fees + avg.fin.serv))) %>%
    mutate(min.tot = sum(unique(min.loan.pmt + min.gear + min.boat.trail +
                                 min.fees + min.fin.serv))) %>%
    mutate(max.tot = sum(unique(max.loan.pmt + max.gear + max.boat.trail +
                                 max.fees + max.fin.serv)))

  
  
  #-------------------- 
  #Clean up objects for tables
  q33.excluding.zeros.table <- q33.excluding.zeros %>% 
    select(-Q22.chr, -loan.pmt, -gear, -boat.trailer, -fees, -fin.services) %>%
    unique() %>%
    mutate(n = nrow(q33.excluding.zeros), .before = med.loan.pmt) %>%
    t()
  
  q33.including.zeros.table <- q33.including.zeros %>% 
    select(-Q22.chr, -boat.ins, -loan.pmt, -mooring, -gear, -boat.trailer,
           -fees, -fin.services) %>%
    unique() %>%
    mutate(n = nrow(q33.excluding.zeros), .before = med.loan.pmt) %>%
    t()
  
  
  #-------------------- 
  #Save final tables 
  write.csv(q33.excluding.zeros.table, na = "0.0", paste("Tables/Q33_",
                                                breakdown, 
                                                sep = "", 
                                                "_annualexpenditures_excludingzeros.csv"), 
            row.names = T)
  
  
  write.csv(q33.including.zeros.table, na = "0.0", paste("Tables/Q33_",
                                                    breakdown, 
                                                    sep = "", 
                                                    "_annualexpenditures_includingzeros.csv"), 
            row.names = T)
  
  
  
  #--------------------
  #Create final list of above objects
  breakdown.output.list <- list("q33.excluding.zeros" = q33.excluding.zeros, 
                                "q33.including.zeros" = q33.including.zeros)
  
  
  #Return final list
  return(breakdown.output.list)
  
}

