#----------------
#FUNCTION FOR TRIP COSTS CALCULATIONS
#2021 AMERICAN SAMOA SMALL BOAT FISHERY COST-EARNINGS SURVEY
#----------------
  #Only one survey response for "Other" category, but respondent was dropped in
    #data cleaning so did not include category in the code.

#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)



#Write function
trip.costs.function <- function(gear.type.trip, breakdown){
  
  
  #Rename argument for group_by to work below
  as.cleaned.trip.costs <- as.sbs.data.cleaned %>% 
    rename(brkdwn = breakdown)

  #-------------------------------------
  #-------------------------------------
  #CORE OPERATING COSTS -- COMBINED CALCULATIONS
  
  #Set up dataframe for primary gear so can rbind later
  q31.32.primary <- as.cleaned.trip.costs %>%
    select(Q31, Q31.2A, Q31.2C, Q31.2E, Q31.2F, Q31.2H, Q31.2J, brkdwn) %>%
    rename(gear = Q31,
           boat.fuel = Q31.2A,
           truck.fuel = Q31.2C,
           oil = Q31.2E,
           ice = Q31.2F,
           bait = Q31.2H,
           food.bev = Q31.2J)
  
  
  #Set up dataframe for secondary gear so can rbind later
  q31.32.secondary <- as.cleaned.trip.costs %>%
    select(Q32, Q32.2A, Q32.2C, Q32.2E, Q32.2F, Q32.2H, Q32.2J, brkdwn) %>%
    rename(gear = Q32,
           boat.fuel = Q32.2A,
           truck.fuel = Q32.2C,
           oil = Q32.2E,
           ice = Q32.2F,
           bait = Q32.2H,
           food.bev = Q32.2J)
  
  
  #Combine both dataframes, but lose indication of primary and secondary
  primary.plus.secondary <- rbind(q31.32.primary, q31.32.secondary)
  
  
  #Combined calculations
  trip.costs.combined <- primary.plus.secondary %>%
    filter(gear == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #BOAT FUEL
    mutate(med.boat.fuel = round(median(boat.fuel, na.rm = T), 1),
           boat.fuel.std.err = round(std.error(boat.fuel, na.rm = T), 1),
           avg.boat.fuel = round(mean(boat.fuel, na.rm = T), 1),
           min.boat.fuel = round(min(boat.fuel, na.rm = T), 1),
           max.boat.fuel = round(max(boat.fuel, na.rm = T), 1),
           #TRUCK FUEL
           med.truck.fuel = round(median(truck.fuel, na.rm = T), 1),
           truck.fuel.std.err = round(std.error(truck.fuel, na.rm = T), 1),
           avg.truck.fuel = round(mean(truck.fuel, na.rm = T), 1),
           min.truck.fuel = round(min(truck.fuel, na.rm = T), 1),
           max.truck.fuel = round(max(truck.fuel, na.rm = T), 1),
           #OIL
           med.oil = round(median(oil, na.rm = T), 1),
           oil.std.err = round(std.error(oil, na.rm = T), 1),
           avg.oil = round(mean(oil, na.rm = T), 1),
           min.oil = round(min(oil, na.rm = T), 1),
           max.oil = round(max(oil, na.rm = T), 1),
           #ICE
           med.ice = round(median(ice, na.rm = T), 1),
           ice.std.err = round(std.error(ice, na.rm = T), 1),
           avg.ice = round(mean(ice, na.rm = T), 1),
           min.ice = round(min(ice, na.rm = T), 1),
           max.ice = round(max(ice, na.rm = T), 1),
           #BAIT
           med.bait = round(median(bait, na.rm = T), 1),
           bait.std.err = round(std.error(bait, na.rm = T), 1),
           avg.bait = round(mean(bait, na.rm = T), 1),
           min.bait = round(min(bait, na.rm = T), 1),
           max.bait = round(max(bait, na.rm = T), 1),
           #FOOD & BEVERAGE
           med.food.bev = round(median(food.bev, na.rm = T), 1), 
           food.bev.std.err = round(std.error(food.bev, na.rm = T), 1),
           avg.food.bev = round(mean(food.bev, na.rm = T), 1),
           min.food.bev = round(min(food.bev, na.rm = T), 1),
           max.food.bev = round(max(food.bev, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.oil + med.ice +
                         med.food.bev + med.bait)) %>% 
    mutate(std.err.trip = std.error(boat.fuel + truck.fuel + oil + ice + bait + 
                                      food.bev)) %>%
    mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.oil + avg.ice +
                              avg.food.bev + avg.bait)) %>%
    mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.oil + min.ice +
                              min.food.bev + min.bait)) %>%
    mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.oil + max.ice +
                              max.food.bev + max.bait)) %>%
    mutate(bt.fuel.per = round(unique(med.boat.fuel / med.trip * 100), 1),
           truck.fuel.per = round(unique(med.truck.fuel / med.trip * 100), 1),
           oil.per = round(unique(med.oil / med.trip * 100), 1),
           ice.per = round(unique(med.ice / med.trip * 100), 1),
           bait.per = round(unique(med.bait / med.trip * 100), 1),
           food.bev.per = round(unique(med.food.bev / med.trip * 100), 1))
  
  
  
  #----------------------------------------------
  #CORE OPERATING COSTS -- PRIMARY CALCULATIONS
  
  trip.costs.primary <- as.cleaned.trip.costs %>%
    select(Q31, Q31.2A, Q31.2C, Q31.2E, Q31.2F, Q31.2H, Q31.2J, brkdwn) %>%
    filter(Q31 == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #BOAT FUEL
    mutate(med.boat.fuel = round(median(Q31.2A, na.rm = T), 1),
           boat.fuel.std.err = round(std.error(Q31.2A, na.rm = T), 1),
           avg.boat.fuel = round(mean(Q31.2A, na.rm = T), 1),
           min.boat.fuel = round(min(Q31.2A, na.rm = T), 1),
           max.boat.fuel = round(max(Q31.2A, na.rm = T), 1),
           #TRUCK FUEL
           med.truck.fuel = round(median(Q31.2C, na.rm = T), 1),
           truck.fuel.std.err = round(std.error(Q31.2C, na.rm = T), 1),
           avg.truck.fuel = round(mean(Q31.2C, na.rm = T), 1),
           min.truck.fuel = round(min(Q31.2C, na.rm = T), 1),
           max.truck.fuel = round(max(Q31.2C, na.rm = T), 1),
           #OIL
           med.oil = round(median(Q31.2E, na.rm = T), 1),
           oil.std.err = round(std.error(Q31.2E, na.rm = T), 1),
           avg.oil = round(mean(Q31.2E, na.rm = T), 1),
           min.oil = round(min(Q31.2E, na.rm = T), 1),
           max.oil = round(max(Q31.2E, na.rm = T), 1),
           #ICE
           med.ice = round(median(Q31.2F, na.rm = T), 1),
           ice.std.err = round(std.error(Q31.2F, na.rm = T), 1),
           avg.ice = round(mean(Q31.2F, na.rm = T), 1),
           min.ice = round(min(Q31.2F, na.rm = T), 1),
           max.ice = round(max(Q31.2F, na.rm = T), 1),
           #BAIT
           med.bait = round(median(Q31.2H, na.rm = T), 1),
           bait.std.err = round(std.error(Q31.2H, na.rm = T), 1),
           avg.bait = round(mean(Q31.2H, na.rm = T), 1),
           min.bait = round(min(Q31.2H, na.rm = T), 1),
           max.bait = round(max(Q31.2H, na.rm = T), 1),
           #FOOD & BEVERAGE
           med.food.bev = round(median(Q31.2J, na.rm = T), 1), 
           food.bev.std.err = round(std.error(Q31.2J, na.rm = T), 1),
           avg.food.bev = round(mean(Q31.2J, na.rm = T), 1),
           min.food.bev = round(min(Q31.2J, na.rm = T), 1),
           max.food.bev = round(max(Q31.2J, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.oil + med.ice +
                               med.food.bev + med.bait)) %>% 
    mutate(std.err.trip = std.error(Q31.2A + Q31.2C + Q31.2E + Q31.2F + Q31.2H
                                    + Q31.2J)) %>%
    mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.oil + avg.ice +
                               avg.food.bev + avg.bait)) %>%
    mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.oil + min.ice +
                               min.food.bev + min.bait)) %>%
    mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.oil + max.ice +
                               max.food.bev + max.bait)) %>%
    mutate(bt.fuel.per = round(unique(med.boat.fuel / med.trip * 100), 1),
           truck.fuel.per = round(unique(med.truck.fuel / med.trip * 100), 1),
           oil.per = round(unique(med.oil / med.trip * 100), 1),
           ice.per = round(unique(med.ice / med.trip * 100), 1),
           bait.per = round(unique(med.bait / med.trip * 100), 1),
           food.bev.per = round(unique(med.food.bev / med.trip * 100), 1))
  
  
  
  #----------------------------------------------
  #CORE OPERATING COSTS -- SECONDARY CALCULATIONS
  
  trip.costs.secondary <- as.cleaned.trip.costs %>%
    select(Q32, Q32.2A, Q32.2C, Q32.2E, Q32.2F, Q32.2H, Q32.2J, brkdwn) %>%
    filter(Q32 == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #BOAT FUEL
    mutate(med.boat.fuel = round(median(Q32.2A, na.rm = T), 1),
           boat.fuel.std.err = round(std.error(Q32.2A, na.rm = T), 1),
           avg.boat.fuel = round(mean(Q32.2A, na.rm = T), 1),
           min.boat.fuel = round(min(Q32.2A, na.rm = T), 1),
           max.boat.fuel = round(max(Q32.2A, na.rm = T), 1),
           #TRUCK FUEL
           med.truck.fuel = round(median(Q32.2C, na.rm = T), 1),
           truck.fuel.std.err = round(std.error(Q32.2C, na.rm = T), 1),
           avg.truck.fuel = round(mean(Q32.2C, na.rm = T), 1),
           min.truck.fuel = round(min(Q32.2C, na.rm = T), 1),
           max.truck.fuel = round(max(Q32.2C, na.rm = T), 1),
           #OIL
           med.oil = round(median(Q32.2E, na.rm = T), 1),
           oil.std.err = round(std.error(Q32.2E, na.rm = T), 1),
           avg.oil = round(mean(Q32.2E, na.rm = T), 1),
           min.oil = round(min(Q32.2E, na.rm = T), 1),
           max.oil = round(max(Q32.2E, na.rm = T), 1),
           #ICE
           med.ice = round(median(Q32.2F, na.rm = T), 1),
           ice.std.err = round(std.error(Q32.2F, na.rm = T), 1),
           avg.ice = round(mean(Q32.2F, na.rm = T), 1),
           min.ice = round(min(Q32.2F, na.rm = T), 1),
           max.ice = round(max(Q32.2F, na.rm = T), 1),
           #BAIT
           med.bait = round(median(Q32.2H, na.rm = T), 1),
           bait.std.err = round(std.error(Q32.2H, na.rm = T), 1),
           avg.bait = round(mean(Q32.2H, na.rm = T), 1),
           min.bait = round(min(Q32.2H, na.rm = T), 1),
           max.bait = round(max(Q32.2H, na.rm = T), 1),
           #FOOD & BEVERAGE
           med.food.bev = round(median(Q32.2J, na.rm = T), 1), 
           food.bev.std.err = round(std.error(Q32.2J, na.rm = T), 1),
           avg.food.bev = round(mean(Q32.2J, na.rm = T), 1),
           min.food.bev = round(min(Q32.2J, na.rm = T), 1),
           max.food.bev = round(max(Q32.2J, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.oil + med.ice +
                               med.food.bev + med.bait)) %>% 
    mutate(std.err.trip = std.error(Q32.2A + Q32.2C + Q32.2E + Q32.2F + Q32.2H
                                    + Q32.2J)) %>%
    mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.oil + avg.ice +
                               avg.food.bev + avg.bait)) %>%
    mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.oil + min.ice +
                               min.food.bev + min.bait)) %>%
    mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.oil + max.ice +
                               max.food.bev + max.bait)) %>%
    mutate(bt.fuel.per = round(unique(med.boat.fuel / med.trip * 100), 1),
           truck.fuel.per = round(unique(med.truck.fuel / med.trip * 100), 1),
           oil.per = round(unique(med.oil / med.trip * 100), 1),
           ice.per = round(unique(med.ice / med.trip * 100), 1),
           bait.per = round(unique(med.bait / med.trip * 100), 1),
           food.bev.per = round(unique(med.food.bev / med.trip * 100), 1))
  
  
  
  #---------------------------------------------
  #DISTRIBUTION OF PRIMARY/SECONDARY GEAR TYPE TRIPS
  
  distribution.primary.secondary <- trip.costs.combined %>%
    mutate(per.primary = nrow(trip.costs.primary) / nrow(trip.costs.combined) * 100,
           per.secondary = nrow(trip.costs.secondary) / nrow(trip.costs.combined) * 100) %>%
    select(per.primary, per.secondary)
  

  
  #----------------------------------------------
  #----------------------------------------------
  
  #OCCASSIONAL MAINTENANCE COSTS -- COMBINED CALCULATIONS
  
  #Set up dataframe for primary gear so can rbind later
  q31.32.primary.occ <- as.cleaned.trip.costs %>%
    select(Q31, Q31.2K, Q31.2L, brkdwn) %>%
    rename(gear = Q31,
           main.rep = Q31.2K,
           lost.gear = Q31.2L)
  
  
  #Set up dataframe for secondary gear so can rbind later
  q31.32.secondary.occ <- as.cleaned.trip.costs %>%
    select(Q32, Q32.2K, Q32.2L, brkdwn) %>%
    rename(gear = Q32,
           main.rep = Q32.2K,
           lost.gear = Q32.2L)
  
  
  #Combine both dataframes, but lose indication of primary and secondary
  primary.plus.secondary.occ <- rbind(q31.32.primary.occ, q31.32.secondary.occ)
  
  
  #Combined calculations
  trip.costs.combined.occ <- primary.plus.secondary.occ %>%
    filter(gear == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #DAILY MAINTENANCE & REPAIR
    mutate(med.main.rep = round(median(main.rep, na.rm = T), 1),
           main.rep.std.err = round(std.error(main.rep, na.rm = T), 1),
           avg.main.rep = round(mean(main.rep, na.rm = T), 1),
           min.main.rep = round(min(main.rep, na.rm = T), 1),
           max.main.rep = round(max(main.rep, na.rm = T), 1),
           #LOST GEAR
           med.lost.gear = round(median(lost.gear, na.rm = T), 1),
           lost.gear.std.err = round(std.error(lost.gear, na.rm = T), 1),
           avg.lost.gear = round(mean(lost.gear, na.rm = T), 1),
           min.lost.gear = round(min(lost.gear, na.rm = T), 1),
           max.lost.gear = round(max(lost.gear, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.main.rep + med.lost.gear)) %>% 
    mutate(std.err.trip = std.error(main.rep + lost.gear)) %>%
    mutate(avg.trip = unique(avg.main.rep + avg.lost.gear)) %>%
    mutate(min.trip = unique(min.main.rep + min.lost.gear)) %>%
    mutate(max.trip = unique(max.main.rep + max.lost.gear)) %>%
    mutate(main.rep.per = round(unique(med.main.rep / med.trip * 100), 1),
           lost.gear.per = round(unique(med.lost.gear / med.trip * 100), 1))
  
  
  
  #----------------------------------------------
  #OCCASSIONAL MAINTENANCE COSTS -- PRIMARY CALCULATIONS
  
  trip.costs.primary.occ <- as.cleaned.trip.costs %>%
    select(Q31, Q31.2K, Q31.2L, brkdwn) %>%
    filter(Q31 == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #DAILY MAINTENANCE & REPAIR
    mutate(med.main.rep = round(median(Q31.2K, na.rm = T), 1),
           main.rep.std.err = round(std.error(Q31.2K, na.rm = T), 1),
           avg.main.rep = round(mean(Q31.2K, na.rm = T), 1),
           min.main.rep = round(min(Q31.2K, na.rm = T), 1),
           max.main.rep = round(max(Q31.2K, na.rm = T), 1),
           #LOST GEAR
           med.lost.gear = round(median(Q31.2L, na.rm = T), 1),
           lost.gear.std.err = round(std.error(Q31.2L, na.rm = T), 1),
           avg.lost.gear = round(mean(Q31.2L, na.rm = T), 1),
           min.lost.gear = round(min(Q31.2L, na.rm = T), 1),
           max.lost.gear = round(max(Q31.2L, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.main.rep + med.lost.gear)) %>% 
    mutate(std.err.trip = std.error(Q31.2K + Q31.2L)) %>%
    mutate(avg.trip = unique(avg.main.rep + avg.lost.gear)) %>%
    mutate(min.trip = unique(min.main.rep + min.lost.gear)) %>%
    mutate(max.trip = unique(max.main.rep + max.lost.gear)) %>%
    mutate(main.rep.per = round(unique(med.main.rep / med.trip * 100), 1),
           lost.gear.per = round(unique(med.lost.gear / med.trip * 100), 1))
  
  
  
  #----------------------------------------------
  #OCCASSIONAL MAINTENANCE COSTS -- SECONDARY CALCULATIONS
  
  trip.costs.secondary.occ <- as.cleaned.trip.costs %>%
    select(Q32, Q32.2K, Q32.2L, brkdwn) %>%
    filter(Q32 == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
    #DAILY MAINTENANCE & REPAIR
    mutate(med.main.rep = round(median(Q32.2K, na.rm = T), 1),
           main.rep.std.err = round(std.error(Q32.2K, na.rm = T), 1),
           avg.main.rep = round(mean(Q32.2K, na.rm = T), 1),
           min.main.rep = round(min(Q32.2K, na.rm = T), 1),
           max.main.rep = round(max(Q32.2K, na.rm = T), 1),
           #LOST GEAR
           med.lost.gear = round(median(Q32.2L, na.rm = T), 1),
           lost.gear.std.err = round(std.error(Q32.2L, na.rm = T), 1),
           avg.lost.gear = round(mean(Q32.2L, na.rm = T), 1),
           min.lost.gear = round(min(Q32.2L, na.rm = T), 1),
           max.lost.gear = round(max(Q32.2L, na.rm = T), 1)) %>% 
    mutate(med.trip = unique(med.main.rep + med.lost.gear)) %>% 
    mutate(std.err.trip = std.error(Q32.2K + Q32.2L)) %>%
    mutate(avg.trip = unique(avg.main.rep + avg.lost.gear)) %>%
    mutate(min.trip = unique(min.main.rep + min.lost.gear)) %>%
    mutate(max.trip = unique(max.main.rep + max.lost.gear)) %>%
    mutate(main.rep.per = round(unique(med.main.rep / med.trip * 100, 1)),
           lost.gear.per = round(unique(med.lost.gear / med.trip * 100, 1)))
  
  
  
  #-------------------------------------
  #-------------------------------------
  #Clean up objects to later rbind into a table
  
  trip.costs.combined.table <- trip.costs.combined %>% 
    select(-gear, -boat.fuel, -truck.fuel, -oil, -ice, -bait, -food.bev) %>%
    unique() %>%
    mutate(n = nrow(trip.costs.combined), .before = med.boat.fuel) %>%
    mutate(trip.type = "combined", .before = n)
  
  trip.costs.primary.table <- trip.costs.primary %>% 
    select(-Q31, -Q31.2A, -Q31.2C, -Q31.2E, -Q31.2F, -Q31.2H, -Q31.2J) %>%
    unique() %>%
    mutate(n = nrow(trip.costs.primary), .before = med.boat.fuel) %>%
    mutate(trip.type = "primary", .before = n)

  trip.costs.secondary.table <- trip.costs.secondary %>% 
    select(-Q32, -Q32.2A, -Q32.2C, -Q32.2E, -Q32.2F, -Q32.2H, -Q32.2J) %>%
    unique() %>%
    mutate(n = nrow(trip.costs.secondary), .before = med.boat.fuel) %>%
    mutate(trip.type = "secondary", .before = n)
  
  
  #-------------------------------------
  
  trip.costs.combined.occ.table <- trip.costs.combined.occ %>%
    select(-gear, -main.rep, -lost.gear)  %>%
    unique() %>%
    mutate(n = nrow(trip.costs.combined.occ), .before = med.main.rep) %>%
    mutate(trip.type = "combined", .before = n)
  
  trip.costs.primary.occ.table <- trip.costs.primary.occ %>% 
    select(-Q31, -Q31.2K, -Q31.2L) %>%
    unique() %>%
    mutate(n = nrow(trip.costs.primary.occ), .before = med.main.rep) %>%
    mutate(trip.type = "primary", .before = n)
  
  trip.costs.secondary.occ.table <- trip.costs.secondary.occ %>% 
    select(-Q32, -Q32.2K, -Q32.2L)%>%
    unique() %>%
    mutate(n = nrow(trip.costs.secondary.occ), .before = med.main.rep) %>%
    mutate(trip.type = "secondary", .before = n)
  
  
  #-------------------------------------
  #-------------------------------------
  #CREATE OUTPUT TABLES
  
  #Core operating costs, rbind together
  q.trip.costs.sum <- rbind(trip.costs.combined.table,
                            trip.costs.primary.table,
                            trip.costs.secondary.table) %>%
    t()
  
  #Save final table 
  write.csv(q.trip.costs.sum, na = "0.0", paste("Tables/Q31.32_", 
                                                gear.type.trip,
                                                "_",
                                                breakdown, 
                                                sep = "", 
                                                "_tripcosts.csv"), 
            row.names = T)
  
  
  
  #-------------------------------------
  #Occasional maintenance costs, rbind together
  q.trip.costs.sum <- rbind(trip.costs.combined.occ.table,
                            trip.costs.primary.occ.table,
                            trip.costs.secondary.occ.table) %>%
    t()
  
  #Save final table 
  write.csv(q.trip.costs.sum, na = "0.0", paste("Tables/Q31.32_", 
                                                gear.type.trip,
                                                "_",
                                                breakdown, 
                                                sep = "", 
                                                "_occasionalmaintenancecosts.csv"), 
            row.names = T)
  
    
  
  #-------------------------------------
  #-------------------------------------
  #Return final list of above objects
  output.list <- list("trip.costs.combined" = trip.costs.combined,
                      "trip.costs.primary" = trip.costs.primary,
                      "trip.costs.secondary" = trip.costs.secondary,
                      "distribution.primary.secondary" = 
                        distribution.primary.secondary, 
                      "trip.costs.combined.occ" = trip.costs.combined.occ,
                      "trip.costs.primary.occ" = trip.costs.primary.occ,
                      "trip.costs.secondary.occ" = trip.costs.secondary.occ)
  
  
  return(output.list)
  
}
  
  