# Holistic decision analysis model of silvoarable mixed fruit syntropic agroforestry system of Hof Luetke ####
# Baseline: conventional agriculture
# Intervention: Part 1: Hedgerows with shrubs that are beneficial for bees to produce honey. 
# Part2: Organic Alley cropping with rowan-raspberry-plum trees and arable crops 
# Part 3: Apple streuobstwiese

# Packages needed ####
#install.packages("decisionSupport")
library(decisionSupport)
library(ggplot2)
library(dplyr)

# From Marcos:
# Modify the gompertz_yield function so that first_yield_estimate_percent is never equal or higher than second_yield_estimate_percent
ontogenic_growth_gompertz <- function (max_harvest, time_to_first_yield_estimate, time_to_second_yield_estimate, 
                                       first_yield_estimate_percent, second_yield_estimate_percent, 
                                       n_years_c, var_CV = 0, no_yield_before_first_estimate = TRUE) 
{
  a = max_harvest
  t1 = time_to_first_yield_estimate
  t2 = time_to_second_yield_estimate
  p2 = second_yield_estimate_percent/100
  if (p2 > 0.999) 
    p2 <- 0.999
  if (p2 < 0.001) 
    p2 <- 0.001
  if (first_yield_estimate_percent > 99) 
    first_yield_estimate_percent <- 99
  if (first_yield_estimate_percent < 1) 
    first_yield_estimate_percent <- 1
  p1 = p2*(first_yield_estimate_percent/100)
  if (t1 == t2) 
    t2 <- t1 + 1
  c <- sum(log(log(p2)/log(p1))/(t1 - t2)) # why to use sum()? The function only has one element, the output without function sum() is the same
  b <- (-log(p1)/exp(-c * t1))
  gompertz <- function(x) {
    a * exp(-b * exp(-c * x))
  }
  yield_n_years_ideal <- gompertz(1:n_years_c)
  yield_n_years_real <- unlist(lapply(yield_n_years_ideal, 
                                      vv, var_CV = var_cv_p, n = 1))
  if (no_yield_before_first_estimate & t1 > 1) {
    yield_n_years_real[1:min(c(n_years_c, t1 - 1))] <- 0
  }
  return(yield_n_years_real)
}

# Assign input table to object to easily use following "make_variables"-function
# input_file <- read.csv("Hof_luetke_input_table.csv")

#Use make_variables function to test chunks of the function code (AF_benefit) during the development process
# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
# }
# 
# generated_variables <- make_variables(as.estimate(input_file)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part
# 

#Defining the probabilisitc model#####
AF_benefit <- function(x, varnames) {
  #System modulators ####
  #yield failure due to pests or diseases
  # chance_perc_crop_fail <-
  #   chance_event(chance = chance_pest_diseases,
  #                value_if = value_if_pest_diseases,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 90% failure
  # AF_chance_perc_crop_fail <-
  #   chance_event(chance = chance_pest_diseases,
  #                value_if = af_value_if_pest_diseases,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 10% failure, due to presence of beneficial insects and a more resistant system with AF
  #yield failure due to weather events
  
  # Chance_perc_weather_fail <-
  #   chance_event(chance = chance_extreme_weather,
  #                value_if = value_if_extreme_weather,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 70% failure
  # AF_Chance_perc_weather_fail <-
  #   chance_event(chance = chance_extreme_weather,
  #                value_if = af_value_if_extreme_weather,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 10% failure --> NO VALUES AT THIS POINT, MODEL RUNS WITHOUT MODULTORS FOR NOW (11.10.2024)
  
  #risk of market fluctuations - chance event is applied by year
  # Market_fluc <- rep(0, n_years)
  # #no_market_fluc <- rep(1, n_years)
  # # accounting fluctuations in market price of all products
  # Market_fluc <- vv(per_market_price, var_CV = var_cv_p, n_years) --> NO VALUES AT THIS POINT, MODEL RUNS WITHOUT MODULTORS FOR NOW (11.10.2024)
  
  # accounting market falls, recession -> economically hard times
  # Chance_market_fluc <-
  #   chance_event(chance = chance_market_crash,
  #                value_if = Market_fluc,
  #                value_if_not = 1) #no_market_fluc) # 10% chance that the event will occur and 
  #result in use the value from vv function --> NO VALUES AT THIS POINT, MODEL RUNS WITHOUT MODULTORS FOR NOW (11.10.2024)
  # AF_Chance_market_fluc <-
  #   chance_event(chance = chance_market_crash,
  #                value_if = Market_fluc,
  #                value_if_not = 1) #no_market_fluc) #value_if_not = 1) # 10% chance that the event will occur and
  #result in use the value from vv function --> NO VALUES AT THIS POINT, MODEL RUNS WITHOUT MODULTORS FOR NOW (11.10.2024)
  
  #Treeless or Monoculture ####
  #baseline system against which AF systems will be compared
  
  # Arable system is managed with crop rotation of maize-wheat-barley-soy
  #one crop is grown once every 4 years
  
  #Define each variable as a vector of "n_years" elements. N_years corresponds to the length of the simulation in years. Here: n_years = 30 years
  # define indices that are part of annual arable production
  Maize_indices <- seq(from = 1, to = n_years_c, by = 4)
  Wheat_indices <- seq(from = 2, to = n_years_c, by = 4)
  Barley_indices <- seq(from = 3, to = n_years_c, by = 4)
  Soy_indices <- seq(from = 4, to = n_years_c, by = 4)
  #Labour
  
  Labour_costs <- vv(labour_cost_p, var_CV = var_cv_p, n_years_c) #, absolute_trend = 2) 
  #Introduce a trend to capture the change in minimum wage over time and the expected wage for agricultural employees
  
  Maize_labour <- rep(0, n_years_c)
  Wheat_labour <- rep(0, n_years_c)
  Barley_labour <- rep(0, n_years_c)
  Soy_labour <- rep(0, n_years_c)
  
  Maize_labour[Maize_indices] <- vv(maize_labour_p, cv_maize_labour_p, length(Maize_indices))
  Wheat_labour[Wheat_indices] <- vv(wheat_labour_p, cv_wheat_labour_p, length(Wheat_indices))
  Barley_labour[Barley_indices] <- vv(barley_labour_p, cv_barley_labour_p, length(Barley_indices))
  Soy_labour[Soy_indices] <- vv(soy_labour_p, cv_soy_labour_p, length(Soy_indices))
  
  
  #Treeless system costs ####  
  # Cost of sowing, harvesting, fertilizing, pesticide application, insurance, fixed and variable machine cost
  Maize_management <- rep(0, n_years_c)
  Wheat_management <- rep(0, n_years_c)
  Barley_management <- rep(0, n_years_c)
  Soy_management <- rep(0, n_years_c)
  Beehive_cost <- rep(0, n_years_c)
  Honey_packaging_cost <- rep(0, n_years_c)
  
  Maize_management[Maize_indices] <- vv(maize_management_p, cv_maize_management_p, length(Maize_indices)) 
  Wheat_management[Wheat_indices] <- vv(wheat_management_p, cv_wheat_management_p, length(Wheat_indices))
  Barley_management[Barley_indices] <- vv(barley_management_p, cv_barley_management_p, length(Barley_indices)) 
  Soy_management[Soy_indices] <- vv(soy_management_p, cv_soy_management_p, length(Soy_indices)) 

  # cost calculations
  Maize_total_cost <- (Maize_management * arable_area_treeless_c) + 
    (Maize_labour * Labour_costs) 
  Wheat_total_cost <- (Wheat_management * arable_area_treeless_c) + 
    (Wheat_labour * Labour_costs) 
  Barley_total_cost <- (Barley_management * arable_area_treeless_c) + 
    (Barley_labour * Labour_costs) 
  Soy_total_cost <- (Soy_management * arable_area_treeless_c) + 
    (Soy_labour * Labour_costs)
  
  
  #Treeless system benefits ####  
  Treeless_maize_yield <- rep(0, n_years_c)
  Treeless_wheat_yield <- rep(0, n_years_c)
  Treeless_barley_yield <- rep(0, n_years_c)
  Treeless_soy_yield <- rep(0, n_years_c)
  Treeless_honey_yield <- rep(0, n_years_c)
  
  Treeless_maize_yield[Maize_indices] <-
    vv(maize_yield_p, cv_maize_yield_p, length(Maize_indices)) * arable_area_treeless_c 
  Treeless_maize_benefit <- vv(maize_value_p, cv_maize_value_p, n_years_c) * Treeless_maize_yield #*
    #Chance_perc_weather_fail #chance_perc_crop_fail 
  
  Treeless_wheat_yield[Wheat_indices] <-
    vv(wheat_yield_p, cv_wheat_yield_p, length(Wheat_indices)) * arable_area_treeless_c 
  Treeless_wheat_benefit <- vv(wheat_value_p, cv_wheat_value_p, n_years_c) * Treeless_wheat_yield #* 
    #Chance_perc_weather_fail #chance_perc_crop_fail 
  
  Treeless_barley_yield[Barley_indices] <-
    vv(barley_yield_p, cv_barley_yield_p, length(Barley_indices)) * arable_area_treeless_c 
  Treeless_barley_benefit <- vv(barley_value_p, cv_barley_value_p, n_years_c) * Treeless_barley_yield #* 
    #Chance_perc_weather_fail #chance_perc_crop_fail 
  
  Treeless_soy_yield[Soy_indices] <-
    vv(soy_yield_p, cv_soy_yield_p, length(Soy_indices)) * arable_area_treeless_c 
  Treeless_soy_benefit <- vv(soy_value_p, cv_soy_value_p, n_years_c) * Treeless_soy_yield #* 
  #Chance_perc_weather_fail #chance_perc_crop_fail 
  
  # Honey potential in AF system
  Treeless_honey_yield[Soy_indices] <- chance_apple_irrigation_need_p * arable_area_treeless_c
  Treeless_honey_benefit <- vv(honey_value_conventional_p, cv_honey_value_p, n_years_c) * Treeless_honey_yield
  
  #Beekeeping related costs
  Treeless_beehive_cost <- rep(0, n_years_c)
  Treeless_honey_packaging_cost <- rep(0, n_years_c)
  
  Treeless_num_beehives <- Treeless_honey_yield/honey_yield_per_hive_p
  Treeless_num_beehives <- ifelse((Treeless_num_beehives %% 1) > 0.5, #%%->modulo, calculates rest
                            ceiling(Treeless_num_beehives),
                            floor(Treeless_num_beehives)) #if total number of suggested beehives is decimal number with rest of > 0.5,
  #then next higher number of beehives is suggested and chosen by farmer
  
  Treeless_beehive_cost <- Treeless_num_beehives * beehives_cost_p
  Treeless_honey_packaging_cost <- Treeless_honey_yield*honey_packaging_cost_p
  Total_beekeeping_cost <- Treeless_beehive_cost + Treeless_honey_packaging_cost
  
  # benefit calculations
  Treeless_total_benefit <- Treeless_maize_benefit + Treeless_wheat_benefit + 
    Treeless_barley_benefit + Treeless_soy_benefit + Treeless_honey_benefit
  
  #total costs calulation
  Treeless_total_cost <-  Maize_total_cost + Wheat_total_cost + 
    Barley_total_cost + Soy_total_cost + Total_beekeeping_cost
 
   #Monoculture output: system bottom line####
  # arable component 
  Treeless_bottom_line <- (Treeless_total_benefit - Treeless_total_cost) #* 
    #Chance_market_fluc
  
  #Agroforestry (AF) System ####
  #Calculating model parameters based on input table
  Arable_area_AF <- arable_area_treeless_c - tree_row_area_c
  
  #AF benefits ####
  Clover_indices <- c()
  for (i in seq(from = 1, to = n_years_c, by = 8)) {
    Clover_indices <- c(Clover_indices, i, i + 1) # Clover occupies two consecutive years
  }
  Org_wheat_indices <- seq(from = 3, to = n_years_c, by = 8)
  Buckwheat_indices <- seq(from = 4, to = n_years_c, by = 8)
  Org_soy_indices <- seq(from = 5, to = n_years_c, by = 8)
  Rye_indices <- seq(from = 6, to = n_years_c, by = 8)
  Flax_indices <- seq(from = 7, to = n_years_c, by = 8)
  Spelt_indices <- seq(from = 8, to = n_years_c, by = 8)
  
  #Annual arable crop component
  AF_clover_yield <- rep(0, n_years_c)
  AF_org_wheat_yield <- rep(0, n_years_c)
  AF_buckwheat_yield <- rep(0, n_years_c)
  AF_rye_yield <- rep(0, n_years_c)
  AF_flax_yield <- rep(0, n_years_c)
  AF_org_soy_yield <- rep(0, n_years_c)
  AF_spelt_yield <- rep(0, n_years_c)
  # account for yield reduction due to shading and competition from trees 
  Perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max,
    time_to_first_yield_estimate = time_to_first_reduction,
    time_to_second_yield_estimate = time_to_second_reduction,
    first_yield_estimate_percent = perc_max_first_reduction,
    second_yield_estimate_percent = perc_max_second_reduction,
    n_years = n_years_c)
  
  #Crop rotation in AF system
  AF_clover_yield[Clover_indices] <-
    vv(clover_ley_yield_p, cv_clover_ley_yield_p, length(Clover_indices)) *(1 - Perc_yield_reduction[Clover_indices]) * 
    Arable_area_AF #* AF_Chance_perc_weather_fail  #* AF_chance_perc_crop_fail
  
  AF_clover_benefit <- vv(clover_ley_value_p, cv_clover_value_p, n_years_c) * AF_clover_yield 
  
  AF_org_wheat_yield[Org_wheat_indices] <-
    vv(org_wheat_yield_p, cv_org_wheat_yield_p, length(Org_wheat_indices)) * (1 - Perc_yield_reduction[Org_wheat_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_org_wheat_benefit <- vv(org_wheat_value_p, cv_org_wheat_value_p, n_years_c) * AF_org_wheat_yield

  AF_buckwheat_yield[Buckwheat_indices] <-
    vv(buckwheat_yield_p, cv_buckwheat_yield_p, length(Buckwheat_indices)) * (1 - Perc_yield_reduction[Buckwheat_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_buckwheat_benefit <- vv(buckwheat_value_p, cv_buckwheat_value_p, n_years_c) * AF_buckwheat_yield
  
  AF_rye_yield[Rye_indices] <-
    vv(rye_yield_p, cv_rye_yield_p, length(Rye_indices)) * (1 - Perc_yield_reduction[Rye_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_rye_benefit <- vv(rye_value_p, cv_rye_value_p, n_years_c) * AF_rye_yield
  
  AF_flax_yield[Flax_indices] <-
    vv(flax_yield_p, cv_flax_yield_p, length(Flax_indices)) * (1 - Perc_yield_reduction[Flax_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_flax_benefit <- vv(flax_value_p, cv_flax_value_p, n_years_c) * AF_flax_yield
  
  AF_org_soy_yield[Org_soy_indices] <-
    vv(AF_org_soy_yield, cv_org_soy_yield_p, length(Org_soy_indices)) * (1 - Perc_yield_reduction[Org_soy_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_org_soy_benefit <- vv(org_soy_value_p, cv_org_soy_value_p, n_years_c) * AF_org_soy_yield
  
  AF_spelt_yield[Spelt_indices] <-
    vv(AF_spelt_yield, cv_spelt_yield_p, length(Spelt_indices)) * (1 - Perc_yield_reduction[Spelt_indices]) *
    Arable_area_AF #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_spelt_benefit <- vv(spelt_value_p, cv_spelt_value_p, n_years_c) * AF_spelt_yield
  
  # benefit calculations
  AF_arable_benefit <- AF_clover_benefit + AF_org_wheat_benefit + 
    AF_buckwheat_benefit + AF_rye_benefit + AF_flax_benefit + 
    AF_org_soy_benefit + AF_spelt_benefit
  
  #Tree component - fruits in AF system
  
  #Yield of one Rowan tree in the tree row [kg/tree]
  AF_rowan_yield <- rep(0, n_years_c)
  AF_rowan_yield <- gompertz_yield(max_harvest = rowan_yield_max_p,
                                   time_to_first_yield_estimate = time_to_first_rowan_c,
                                   time_to_second_yield_estimate = time_to_second_rowan_p,
                                   first_yield_estimate_percent = rowan_yield_first_p,
                                   second_yield_estimate_percent = rowan_yield_second_p,
                                   n_years=n_years_c,
                                   var_CV = cv_rowan_yield_p,
                                   no_yield_before_first_estimate = TRUE)
  
  #Yield from all rowan fruit trees [kg] considering risks
  AF_tot_rowan_yield <- AF_rowan_yield * num_rowan_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  AF_rowan_benefit <-  AF_tot_rowan_yield * rowan_value_p
  
  #Yield of one Raspberry shrub in the tree row [kg/tree]
  AF_raspberry_yield <- rep(0, n_years_c)
  Raspberry_row_legth <- raspberry_area_c*10000/raspberry_row_width_c #Raspberry yield is often estimated per meter of raspberry hedge -> in order for the yield to be adjustable by changing the "raspberry_area" slider in UI, Raspberry_row_length has to be dependend on rapsberry_area 
  AF_raspberry_yield[time_to_first_raspberry_c] <- raspberry_yield_max_p * Raspberry_row_legth * percentage_raspberry_first_yield_p
  AF_raspberry_yield[time_to_second_raspberry_c:n_years_c] <- vv(raspberry_yield_max_p, cv_raspberry_yield_max_p, time_to_second_raspberry_c:n_years_c) * Raspberry_row_legth #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail

  AF_raspberry_benefit <-  AF_raspberry_yield * raspberry_value_p
  
  #Yield of one Plum tree in the tree row [kg/tree]
  AF_plum_yield <- rep(0, n_years_c)
  AF_plum_yield <- gompertz_yield(max_harvest = plum_yield_max_p,
                                   time_to_first_yield_estimate = time_to_first_plum_c,
                                   time_to_second_yield_estimate = time_to_second_plum_p,
                                   first_yield_estimate_percent = plum_yield_first_p,
                                   second_yield_estimate_percent = plum_yield_second_p,
                                   n_years=n_years_c,
                                   var_CV = cv_plum_yield_p,
                                   no_yield_before_first_estimate = TRUE)
  
  #Yield from all rowan fruit trees [kg] considering risks
  AF_tot_plum_yield <- AF_plum_yield * num_plum_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  AF_plum_benefit <-  AF_tot_plum_yield * plum_value_p
  
  #Yield of one fruit tree in Apple streuobstwiese [kg/tree]
  AF_apple_yield <- rep(0, n_years_c)
  AF_apple_yield <- gompertz_yield(max_harvest = apple_yield_max_p,
                                   time_to_first_yield_estimate = time_to_first_apple_p,
                                   time_to_second_yield_estimate = time_to_second_apple_p,
                                   first_yield_estimate_percent = apple_yield_first_p,
                                   second_yield_estimate_percent = apple_yield_second_p,
                                   n_years=n_years_c,
                                   var_CV = cv_apple_yield_p,
                                   no_yield_before_first_estimate = TRUE)
  #Yield from all apple trees [kg] considering risks
  AF_tot_apple_yield <- AF_apple_yield * num_apple_trees_c #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  #Calculate different quality of fruits and their marketable benefit
  Pc_table_apple <- vv(perc_table_apple_p, var_CV = var_cv_p, n_years_c)/100
  Table_apple_yield <- AF_tot_apple_yield * Pc_table_apple #amount of highest quality fruits [kg]
  Lower_qual_apple_yield <- AF_tot_apple_yield * (1-Pc_table_apple) #rest of yield is classified as lower quality
  Pc_b_qual_apple <- vv(perc_bqual_apple_p, var_CV = var_cv_p, n_years_c)/100 #B-quality fruits can still be sold in direct selling operation, but at significantly lower price
  B_qual_table_apple_yield <- Lower_qual_apple_yield * Pc_b_qual_apple #amount of B-quality fruits [kg]
  Juice_apple_yield <- Lower_qual_apple_yield * (1-Pc_b_qual_apple) #Rest of the fruit yield can be sold as juicing fruit at lowest price 
  
  #The benefits from table apples and juice apples are calculated by multiplying their yields by their respective prices  
  Table_apple_benefit <- Table_apple_yield * table_apple_value_p
  B_qual_apple_benefit <- B_qual_table_apple_yield * bqual_apple_value_p
  Juice_apple_benefit <-  Juice_apple_yield * juice_apple_value_p
  
  AF_apple_benefit <- Table_apple_benefit + B_qual_apple_benefit + Juice_apple_benefit
  
  # Honey potential in AF system
  Clover_honey_yield <- rep(0, n_years_c)
  Org_soy_honey_yield <- rep(0, n_years_c)
  Flax_honey_yield <- rep(0, n_years_c)
  Buckwheat_honey_yield <- rep(0, n_years_c)
  Streuobstwiese_honey_yield <- rep(0, n_years_c)
  Rowan_honey_yield <- rep(0, n_years_c)
  Raspberry_honey_yield <- rep(0, n_years_c)
  Plum_honey_yield <- rep(0, n_years_c)
  Apple_honey_yield <- rep(0, n_years_c)
  Hedge_honey_yield <- rep(0, n_years_c)
  
  Clover_honey_yield[Clover_indices] <- honey_potential_clover_p * Arable_area_AF
  Org_soy_honey_yield[Org_soy_indices] <- chance_apple_irrigation_need_p * Arable_area_AF
  Flax_honey_yield[Flax_indices] <- honey_potential_flax_p * Arable_area_AF
  Buckwheat_honey_yield[Buckwheat_indices] <- honey_potential_buckwheat_p * Arable_area_AF
  Streuobstwiese_honey_yield[1:n_years_c] <- honey_potential_streuobstwiese_p
  
  Hedge_honey_yield <- gompertz_yield(max_harvest = honey_potential_hedge_p,
                                      time_to_first_yield_estimate = time_to_first_hedge_honey_p,
                                      time_to_second_yield_estimate = time_to_second_hedge_honey_p,
                                      first_yield_estimate_percent = hedge_honey_first_p,
                                      second_yield_estimate_percent = hedge_honey_second_p,
                                      n_years=n_years_c,
                                      var_CV = var_cv_p,
                                      no_yield_before_first_estimate = TRUE) 
  
  Tot_hedge_honey_yield <- rep(0, n_years_c)
  
  # First 9 years of honey yield from hedge are modeled followiing gompertz curve, without pruning
  Tot_hedge_honey_yield[1:9] <- Hedge_honey_yield[1:9] * hedge_row_area_c
  
  # Following years the honey yield is modelled with pruning of hedge considered (hedge is pruned 3 years in a row, 1/3 each year, every 10 years)
  
  Hedge_pruning_indices <- c()
  for (i in seq(from = 10, to = n_years_c, by = 10)) { #Hedgerow is pruned in three consecutive years (1/3 each year)
    #add only valid pruning years (i, i+1, i+2) if they are <= n_years_c to make sure resulting vector does not exceed n_years_c elements
    if (i <= n_years_c) {
      Hedge_pruning_indices <- c(Hedge_pruning_indices, i)
    }
    
    if (i + 1 <= n_years_c) {
      Hedge_pruning_indices <- c(Hedge_pruning_indices, i + 1)
    }
    
    if (i + 2 <= n_years_c) {
      Hedge_pruning_indices <- c(Hedge_pruning_indices, i + 2)
    }
  }
  # Pruning + Recovery phase of 4 years is assumed. -1/3, -2/3, -2/3, -1/3, back to normal
  for (i in seq(from = 10, to = n_years_c, by = 10)) {
    
    # First year of pruning (reduce honey yield by 1/3)
    if (i <= n_years_c) {
      Tot_hedge_honey_yield[i] <- Hedge_honey_yield[i] * hedge_row_area_c * (2/3)
    }    
    # Second year of pruning (reduce honey yield by 2/3)
    if (i + 1 <= n_years_c) {
      Tot_hedge_honey_yield[i + 1] <- Hedge_honey_yield[i + 1] * hedge_row_area_c * (1/3)
    }    
    # Third year of pruning (reduce honey yield by 2/3 again)
    if (i + 2 <= n_years_c) {
      Tot_hedge_honey_yield[i + 2] <- Hedge_honey_yield[i + 2] * hedge_row_area_c * (1/3)
    }    
    # Fourth year after pruning (reduce honey yield by 1/3, recovering from pruning)
    if (i + 3 <= n_years_c) {
      Tot_hedge_honey_yield[i + 3] <- Hedge_honey_yield[i + 3] * hedge_row_area_c * (2/3)
    }
    
    # Fifth year after pruning (honey yield returns to full capacity)
    if (i + 4 <= n_years_c) {
      Tot_hedge_honey_yield[i + 4] <- Hedge_honey_yield[i + 4] * hedge_row_area_c
    }
  }
  
  # # For the remaining years that are not part of the pruning cycle (after year 10 but not a pruning year)
  # for (i in setdiff(10:n_years_c, Hedge_pruning_indices)) {
  #   if (Tot_hedge_honey_yield[i] == 0 && i <= n_years_c) {
  #     Tot_hedge_honey_yield[i] <- Hedge_honey_yield[i] * hedge_row_area_c
  #   }
  # }
  # 
  # 
  
  # For the remaining years that are not part of the pruning cycle (after year 10 but not a pruning year)
  for (i in setdiff(10:n_years_c, Hedge_pruning_indices)) {
    # Check if the value is NA before comparison and handle appropriately
    if (is.na(Tot_hedge_honey_yield[i]) || Tot_hedge_honey_yield[i] == 0) {
      if (i <= n_years_c) {
        Tot_hedge_honey_yield[i] <- Hedge_honey_yield[i] * hedge_row_area_c
      }
    }
  }
  
  
  #Honey yield [kg] is assumed to follow same gompertz curve as fruit yield
  Rowan_honey_yield <- gompertz_yield(max_harvest = honey_potential_rowan_p,
                                      time_to_first_yield_estimate = time_to_first_rowan_c,
                                      time_to_second_yield_estimate = time_to_second_rowan_p,
                                      first_yield_estimate_percent = rowan_yield_first_p,
                                      second_yield_estimate_percent = rowan_yield_second_p,
                                      n_years=n_years_c,
                                      var_CV = cv_rowan_yield_p,
                                      no_yield_before_first_estimate = TRUE)
  
  #Yield from all rowan fruit trees [kg] considering risks
  Tot_rowan_honey_yield <- Rowan_honey_yield * num_rowan_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  #Honey yield is assumed to follow same progression as fruit yield
  Raspberry_honey_yield[time_to_first_raspberry_c] <- honey_potential_raspberry_p * raspberry_area_c * percentage_raspberry_first_yield_p
  Raspberry_honey_yield[time_to_second_raspberry_c:n_years_c] <- vv(honey_potential_raspberry_p, cv_raspberry_yield_max_p, time_to_second_raspberry_c:n_years_c) * raspberry_area_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  Plum_honey_yield <- gompertz_yield(max_harvest = honey_potential_plum_p,
                                  time_to_first_yield_estimate = time_to_first_plum_c,
                                  time_to_second_yield_estimate = time_to_second_plum_p,
                                  first_yield_estimate_percent = plum_yield_first_p,
                                  second_yield_estimate_percent = plum_yield_second_p,
                                  n_years=n_years_c,
                                  var_CV = cv_plum_yield_p,
                                  no_yield_before_first_estimate = TRUE)
  
  Tot_plum_honey_yield <- Plum_honey_yield * num_plum_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  Apple_honey_yield <- gompertz_yield(max_harvest = honey_potential_apple_p,
                                   time_to_first_yield_estimate = time_to_first_apple_p,
                                   time_to_second_yield_estimate = time_to_second_apple_p,
                                   first_yield_estimate_percent = apple_yield_first_p,
                                   second_yield_estimate_percent = apple_yield_second_p,
                                   n_years=n_years_c,
                                   var_CV = cv_apple_yield_p,
                                   no_yield_before_first_estimate = TRUE)
  
  
  Tot_apple_honey_yield <- Apple_honey_yield * num_apple_trees_c #* AF_Chance_perc_weather_fail #* AF_chance_perc_crop_fail
  
  AF_tot_honey_potential <- Org_soy_honey_yield + Flax_honey_yield + Buckwheat_honey_yield + Streuobstwiese_honey_yield +
    Tot_rowan_honey_yield + Raspberry_honey_yield + Tot_plum_honey_yield + Tot_apple_honey_yield + Tot_hedge_honey_yield
  
  AF_num_beehives <- AF_tot_honey_potential/honey_yield_per_hive_p
  AF_num_beehives <- ifelse((AF_num_beehives %% 1) > 0.5, #%%->modulo, calculates rest
                            ceiling(AF_num_beehives),
                            floor(AF_num_beehives)) #if total number of suggested beehives is decimal number with rest of > 0.5,
                                                    #then next higher number of beehives is suggested and chosen by farmer

  AF_tot_honey_yield <- AF_num_beehives * honey_yield_per_hive_p
  
  AF_honey_benefit <- vv(honey_value_organic_p, cv_honey_value_p, n_years_c) * AF_tot_honey_yield
  
  AF_tree_benefit <- AF_rowan_benefit + AF_raspberry_benefit + AF_plum_benefit + AF_apple_benefit + AF_honey_benefit 
  
  # consider sale of pruning bits??
  
  # Intangible benefits from ESS like soil erosion control, improved soil quality and biodiversity, change in microclimate, reduced impact of extreme events
  #carbon sequestration in T C/ha/yr
  # Timber yield function from Marcos 
  # AF_timber_yield <- ontogenic_growth_gompertz(max_harvest = volume_target_rotation,
  #                                              time_to_first_yield_estimate = time_first_volume_est,
  #                                              time_to_second_yield_estimate = time_sec_volume_est,
  #                                              first_yield_estimate_percent = first_vol_rel_to_sec_est_perc,
  #                                              second_yield_estimate_percent = sec_volume_est_per,
  #                                              n_years_c = n_years_c,
  #                                              no_yield_before_first_estimate = FALSE)
  # 
  # AF_C_sequestration <- gompertz_yield(
  #   max_harvest = AF_timber_yield,
  #   time_to_first_yield_estimate = 10, #time_to_first_C_sequester,
  #   time_to_second_yield_estimate = 15, #time_to_second_C_sequester,
  #   first_yield_estimate_percent = C_sequester_first,
  #   second_yield_estimate_percent = C_sequester_second,
  #   n_years_c = n_years_c,
  #   var_CV = var_cv_p,
  #   no_yield_before_first_estimate = TRUE
  # )
  # 
  # C_benefit <- vv(pc_carbon_storage, var_cv_p, n_years_c) * 
  #   AF_C_sequestration * arable_area_treeless_c
  
  GW_benefit <- rep(0, n_years_c)
  Erosion_control_benefit <- rep(0, n_years_c)
  NMES_indices <- seq(from = 5, to = n_years_c)
  
  GW_benefit[NMES_indices] <- vv(pc_ground_water_recharge_p, var_cv_p, length(NMES_indices)) * arable_area_treeless_c
  Erosion_control_benefit[NMES_indices] <- vv(soil_loss_p, var_cv_p, length(NMES_indices)) * 
    vv(pc_soil_loss_p, var_cv_p, length(NMES_indices)) * arable_area_treeless_c  
  #pollinator_benefit yet to be added
  
  Nonmarket_ES_benefit <- GW_benefit + Erosion_control_benefit #+ C_benefit

  #Subsidy in AF system
  ES3_subsidy <- rep(0, n_years_c)
  ES3_subsidy[1:n_years_c] <- es3_subsidy_c * tree_row_area_c
  
  #Agroforestry total benefit
  AF_total_benefit <- AF_tree_benefit + AF_arable_benefit + ES3_subsidy + Nonmarket_ES_benefit
  
  #AF costs ####
  # Source: FE- farmer/practitioner estimate; EE- expert estimate
  #Calculating costs - tree component ###
  
  #Implementation cost : define variables
  AF_planning_cost <- rep(0, n_years_c) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  AF_dig_plant_holes <- rep(0, n_years_c) # FE; Second step of implementation: digging/drilling holes for the trees [€]
  AF_tree_cost <- rep(0, n_years_c) #FE; Cost per tree [€]
  AF_plant_tree_cost <- rep(0, n_years_c) #FE; Labour cost for planting one tree [€] 
  AF_material_cost <- rep(0, n_years_c) #FE; Material cost of tree protection mesh [€]
  AF_weed_protect_cost <- rep(0, n_years_c) #Material cost of weed suppressing fleece [€]
  AF_compost_cost <- rep(0, n_years_c) #FE; Cost of compost used during planting [€]
  #AF_irrigation_system_cost <- rep(0, n_years_c) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF_irrigation_planting_cost <- rep(0, n_years_c) #FE; Cost for watering in newly planted trees [€]
  AF_total_planting_cost <- rep(0, n_years_c)
  
  #Running cost : define variables
  AF_apple_pruning <- rep(0, n_years_c) #FE/EE; Labour cost of pruning fruit trees [€]
  AF_raspberry_pruning <- rep(0, n_years_c)
  AF_rowan_pruning <- rep(0, n_years_c)
  AF_plum_pruning <- rep(0, n_years_c)
  AF_hedge_row_pruning <- rep(0, n_years_c)
  AF_root_pruning <- rep(0, n_years_c) #FE/EE; Labour cost of pruning roots of trees next to tree rows [€]
  #AF_thinning <- rep(0, n_years_c) #FE/EE; Labour cost of thinning fruit trees [€]
  AF_apple_harvest <- rep(0, n_years_c) #FE; Labour cost of harvesting apples manually [€]
  AF_raspberry_harvest <- rep(0, n_years_c) #Labour cost of harvesting raspberries manually
  AF_rowan_harvest <- rep(0, n_years_c)
  AF_plum_harvest <- rep(0, n_years_c)
  #AF_self_harvest <-  rep(0, n_years_c) #FE; cost of materials provided for self harvest of alley fruits [€]
  ES3_application <- rep(0, n_years_c) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  AF_alley_annual_irrigation <- rep(0, n_years_c) #FE; Cost of annual irrigation of tree rows [€]
  AF_mowing_treerow <- rep(0, n_years_c) #FE; Labour hours of mowing the tree rows manually [h/ha]
  AF_irrigation_establishment_phase <- rep(0, n_years_c) #Water cost for intensive irrigation during establishment years [€]
  AF_regular_annual_irrigation_cost <- rep(0, n_years_c) #Water cost for regular annual irrigation after trees are established [€]
  
  # AF SYSTEM ESTABLISHMENT COST  
  
  #Planning and consulting
  AF_planning_cost[1] <- planning_consulting_p + farmer_planning_time_p * Labour_costs[1]
  
  #Field prep
  Num_trees <- num_apple_trees_c + num_rowan_trees_c + num_plum_trees_c
  
  AF_tree_cost[1] <- (apple_tree_cost_p * num_apple_trees_c) + (rowan_tree_cost_p * num_rowan_trees_c)+ 
    (plum_tree_cost_p * num_plum_trees_c) +(raspberry_plant_cost_p * num_raspberry_shrubs_c)+ (hedge_shrub_cost_p * num_hedge_shrubs_c)
  
  AF_plant_tree_cost[1] <- ((planting_trees_alley_p*(num_rowan_trees_c+num_plum_trees_c)) + (planting_raspberry_p*num_raspberry_shrubs_c) +
    (planting_trees_streuobstwiese_p*num_apple_trees_c) + (planting_trees_hedge_p*num_hedge_shrubs_c)) * Labour_costs[1]
  
  AF_material_cost[1] <-  material_rowan_p*num_rowan_trees_c + material_plum_p*num_plum_trees_c + material_raspberry_p*num_raspberry_shrubs_c +
    material_hedgerow_p*num_hedge_shrubs_c + deer_fence_p # apple material cost is part of apple_tree_cost
  
  AF_weed_protect_cost[1] <- weed_protection_p * Num_trees
  
  AF_compost_cost[1] <- compost_planting_p * compost_price_p * Num_trees
  
  AF_irrigation_planting_cost[1] <- irrigation_tree_rows_establishment_p * Num_trees * water_price_p
 
 # Alley cropping system cost
  AF_total_planting_cost <-  AF_tree_cost + AF_plant_tree_cost + AF_material_cost +
    AF_weed_protect_cost + AF_compost_cost  + AF_irrigation_planting_cost #+ AF_irrigation_system_cost? Is irrigation system being installed?

  AF_total_investment_cost <- AF_planning_cost + AF_total_planting_cost #Investment cost of AF system implementation
  
  #Running costs
  ES3_application[1] <- es3_application_p * Labour_costs[1] #application for Eco Scheme subsidy has to be repeated annually
  ES3_application[2:n_years_c] <- vv(es3_re_application_p, var_cv_p, length(2:n_years_c)) * Labour_costs[2:n_years_c] #re-applying for ES3 requires less time 
  
  # Irrigation
  #AF_irrigation_system_cost[1] <- 
  AF_irrigation_establishment_phase[2:4] <- irrigation_apple_123_p * Num_trees * water_price_p #"irrigation_apple_123" is used for all fruit trees, since they all depend on heavy irrigation in establishment phase
  AF_regular_annual_irrigation_cost [5:n_years_c] <- vv(irrigation_tree_rows_regular_p, var_CV = n_years_c, length(5:n_years_c)) *
    tree_row_area_c * water_price_p 
  # manually irrigate
  AF_apple_irrigation_cost <- rep(0, n_years_c)
  AF_apple_irrigation_cost[5:n_years_c] <- chance_event(chance = chance_apple_irrigation_need_p,
                                      value_if = irrigation_apple_123_p,
                                      value_if_not = 0) * num_apple_trees_c * water_price_p #5-10% chance that apples will need extra irrigation
  # total irrigation cost
  AF_running_irrigation_cost <- AF_regular_annual_irrigation_cost + AF_apple_irrigation_cost
  
  # Pruning
  AF_apple_pruning[1:5] <- pruning_apple_juv1_p * num_apple_trees_c * Labour_costs[1:5]
  AF_apple_pruning[6:10] <- pruning_apple_juv2_p * num_apple_trees_c * Labour_costs[6:10] 
  AF_apple_pruning[11:15] <- pruning_apple_adult1_p * num_apple_trees_c * Labour_costs[11:15] 
  AF_apple_pruning[16:n_years_c] <- pruning_apple_adult2_p * num_apple_trees_c * Labour_costs[16:n_years_c]
  AF_raspberry_pruning[time_to_second_raspberry_c:n_years_c] <- vv(pruning_raspberry_p, var_CV = var_cv_p, length(time_to_second_raspberry_c:n_years_c)) * Raspberry_row_legth * Labour_costs[time_to_second_raspberry_c:n_years_c]
  AF_rowan_pruning[1:3] <- vv(pruning_rowan1_p, var_CV = var_cv_p, length(1:3)) * num_rowan_trees_c * Labour_costs[1:3]
  AF_rowan_pruning[4:7] <- vv(pruning_rowan2_p, var_CV = var_cv_p, length(4:7)) * num_rowan_trees_c * Labour_costs[4:7] #Rowan is assumed to not need regular pruning after establishment phase of ~7a (F.Ganswind (2024))
  AF_plum_pruning[1:5] <- vv(pruning_plum_juv1_p, var_CV = var_cv_p, length(1:5)) * num_plum_trees_c * Labour_costs[1:5]
  AF_plum_pruning[6:10] <- vv(pruning_plum_juv2_p, var_CV = var_cv_p, length(6:10)) * num_plum_trees_c * Labour_costs[6:10]
  AF_plum_pruning[11:15] <- vv(pruning_plum_adult1_p, var_CV = var_cv_p, length(11:15)) * num_plum_trees_c * Labour_costs[11:15]
  AF_plum_pruning[16:n_years_c] <- vv(pruning_plum_adult2_p, var_CV = var_cv_p, length(16:n_years_c)) * num_plum_trees_c * Labour_costs[16:n_years_c]
  
  
  AF_hedge_row_pruning[Hedge_pruning_indices] <- hedge_row_area_c/3 * pruning_hedgerow_p #pruning_hedgerow includes cost of labour, machinery etc. in €/ha
  
  AF_pruning <- AF_apple_pruning + AF_raspberry_pruning + AF_rowan_pruning + AF_plum_pruning + AF_hedge_row_pruning
  AF_root_pruning <- vv(root_pruning_p, var_CV = var_cv_p, n_years_c) * Labour_costs
  
  #AF_thinning <- vv(pruning_annual, var_CV = var_cv_p, n_years_c) * Labour_costs * thinned_trees
  AF_mowing_treerow <-
    vv(mowing_treerow_p, var_CV = var_cv_p, n_years_c) * tree_row_area_c * Labour_costs
  AF_apple_harvest[time_to_first_apple_p:n_years_c] <-
    vv(apple_harvest_cost_p, var_CV = var_cv_p, length(time_to_first_apple_p:n_years_c))*AF_tot_apple_yield[time_to_first_apple_p:n_years_c]*Labour_costs[time_to_first_apple_p:n_years_c]
  AF_raspberry_harvest [time_to_first_raspberry_c:n_years_c] <-
    vv(raspberry_harvest_cost_p, var_CV = var_cv_p, length(time_to_first_raspberry_c:n_years_c))*AF_raspberry_yield[time_to_first_raspberry_c:n_years_c]*Labour_costs[time_to_first_raspberry_c:n_years_c]
  AF_rowan_harvest[time_to_first_rowan_c:n_years_c] <-
    vv(rowan_harvest_cost_p, var_CV = var_cv_p, length(time_to_first_rowan_c:n_years_c))*AF_rowan_yield[time_to_first_rowan_c:n_years_c]*Labour_costs[time_to_first_rowan_c:n_years_c]
  AF_plum_harvest[time_to_first_plum_c:n_years_c] <-
    vv(plum_harvest_cost_p, var_CV = var_cv_p, length(time_to_first_plum_c:n_years_c))*AF_plum_yield[time_to_first_plum_c:n_years_c]*Labour_costs[time_to_first_plum_c:n_years_c]
  # AF_self_harvest  [2:n_years_c] <-
  #   vv(self_harvest_cost, var_CV = var_cv_p, length(2:n_years_c))
  # AF_apple_packaging [time_to_first_apple_p:n_years_c] <-
  #   vv(apple_packaging_cost, var_CV = var_cv_p, length(time_to_first_apple_p:n_years_c))
  # AF_raspberry_packaging [2:n_years_c] <-
  #   vv(raspberry_packaging_cost, var_CV = var_cv_p, length(2:n_years_c))
  # AF_rowan_packaging [time_to_first_rowan_c:n_years_c] <-
  #   vv(rowan_packaging_cost, var_CV = var_cv_p, length(time_to_first_rowan_c:n_years_c))
  # AF_plum_packaging [time_to_first_plum_c:n_years_c] <-
  #   vv(plum_packaging_cost, var_CV = var_cv_p, length(time_to_first_plum_c:n_years_c))
  
  Time_to_first_fruit <- min(time_to_first_apple_p, time_to_first_rowan_c, time_to_first_plum_c, time_to_first_raspberry_c)
  
  AF_fruit_packaging <- rep(0, n_years_c)
  AF_fruit_packaging[Time_to_first_fruit:n_years_c] <-
    vv(fruit_packaging_p, var_CV = var_cv_p, length(Time_to_first_fruit:n_years_c)) * 
    (Table_apple_yield+B_qual_table_apple_yield + AF_tot_plum_yield + AF_tot_rowan_yield + AF_raspberry_yield)[Time_to_first_fruit:n_years_c] # labour cost of packaging [€/kg]
    #Total cost of tree component in AF system
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + AF_root_pruning + 
    AF_running_irrigation_cost + AF_mowing_treerow + AF_apple_harvest + AF_raspberry_harvest + 
    AF_rowan_harvest + AF_plum_harvest + AF_fruit_packaging #+ AF_self_harvest
  
  #Beekeeping related costs
  AF_beehive_cost <- rep(0, n_years_c)
  AF_honey_packaging_cost <- rep(0, n_years_c)
  
  AF_beehive_cost <- AF_num_beehives * beehives_cost_p

  AF_honey_packaging_cost <- AF_tot_honey_yield*honey_packaging_cost_p
  
  AF_total_beekeeping_cost <- AF_beehive_cost + AF_honey_packaging_cost
  
  #Cost of arable component ###
  # Arable crops in AF 
  
  #AF_XXX_management_cost includes: seed, insurance, fixed+variable machine cost
  AF_clover_management_cost <- rep(0, n_years_c)
  AF_org_wheat_management_cost <- rep(0, n_years_c)
  AF_buckwheat_management_cost <- rep(0, n_years_c)
  AF_rye_management_cost <- rep(0, n_years_c)
  AF_flax_management_cost <- rep(0, n_years_c)
  AF_org_soy_management_cost <- rep(0, n_years_c)
  AF_spelt_management_cost <- rep(0, n_years_c)
  
  
  AF_clover_management_cost[Clover_indices] <- vv(clover_ley_management_p, var_CV = var_cv_p, length(Clover_indices))*Arable_area_AF
  AF_org_wheat_management_cost[Org_wheat_indices] <- vv(org_wheat_management_p, var_CV = var_cv_p, length(Org_wheat_indices))*Arable_area_AF
  AF_buckwheat_management_cost[Buckwheat_indices] <- vv(buckwheat_management_p, var_CV = var_cv_p, length(Buckwheat_indices))*Arable_area_AF 
  AF_rye_management_cost[Rye_indices] <- vv(rye_management_p, var_CV = var_cv_p, length(Rye_indices))*Arable_area_AF
  AF_flax_management_cost[Flax_indices] <- vv(flax_management_p, var_CV = var_cv_p, length(Flax_indices))*Arable_area_AF
  AF_org_soy_management_cost[Org_soy_indices] <- vv(org_soy_management_p, var_CV = var_cv_p, length(Org_soy_indices))*Arable_area_AF
  AF_spelt_management_cost[Spelt_indices] <- vv(spelt_management_p, var_CV = var_cv_p, length(Spelt_indices))*Arable_area_AF
  
  AF_clover_labour_cost <- rep(0, n_years_c)
  AF_org_wheat_labour_cost <- rep(0, n_years_c)
  AF_buckwheat_labour_cost <- rep(0, n_years_c)
  AF_rye_labour_cost <- rep(0, n_years_c)
  AF_flax_labour_cost <- rep(0, n_years_c)
  AF_org_soy_labour_cost <- rep(0, n_years_c)
  AF_spelt_labour_cost <- rep(0, n_years_c)
  
  AF_clover_labour_cost[Clover_indices] <- vv(clover_ley_labour_p, var_CV = var_cv_p, length(Clover_indices))*Arable_area_AF*(Labour_costs[Clover_indices]*af_labour_increase/100)
  AF_org_wheat_labour_cost[Org_wheat_indices] <- vv(org_wheat_labour_p, var_CV = var_cv_p, length(Org_wheat_indices))*Arable_area_AF*(Labour_costs[Org_wheat_indices]*af_labour_increase/100)
  AF_buckwheat_labour_cost[Buckwheat_indices] <- vv(buckwheat_labour_p, var_CV = var_cv_p, length(Buckwheat_indices))*Arable_area_AF*(Labour_costs[Buckwheat_indices]*af_labour_increase/100)
  AF_flax_labour_cost[Flax_indices] <- vv(rye_labour_p, var_CV = var_cv_p, length(Rye_indices))*Arable_area_AF*(Labour_costs[Rye_indices]*af_labour_increase/100)
  AF_rye_labour_cost[Rye_indices] <- vv(flax_labour_p, var_CV = var_cv_p, length(Flax_indices))*Arable_area_AF*(Labour_costs[Flax_indices]*af_labour_increase/100)
  AF_org_soy_labour_cost[Org_soy_indices] <- vv(org_soy_labour_p, var_CV = var_cv_p, length(Org_soy_indices))*Arable_area_AF*(Labour_costs[Org_soy_indices]*af_labour_increase/100)
  AF_spelt_labour_cost[Spelt_indices] <- vv(spelt_labour_p, var_CV = var_cv_p, length(Spelt_indices))*Arable_area_AF*(Labour_costs[Spelt_indices]*af_labour_increase/100)
  
  
  #Processing and packaging costs [€/t]
  AF_clover_PPcost <- rep(0, n_years_c)
  AF_org_wheat_PPcost <- rep(0, n_years_c)
  AF_buckwheat_PPcost <- rep(0, n_years_c)
  AF_rye_PPcost <- rep(0, n_years_c)
  AF_flax_PPcost <- rep(0, n_years_c)
  AF_org_soy_PPcost <- rep(0, n_years_c)
  AF_spelt_PPcost <- rep(0, n_years_c)
  
  #clover
  # AF_clover_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_clover_yield #processing and packaging costs per ton -> no values yet -> without processing and packaging gross value is determined
  
  AF_total_clover_cost <- AF_clover_management_cost + AF_clover_labour_cost #+ AF_clover_PPcost
  
  #org_wheat
  # AF_org_wheat_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_org_wheat_yield
  
  AF_total_org_wheat_cost <- AF_org_wheat_management_cost + AF_org_wheat_labour_cost #+ AF_org_wheat_PPcost
  
  #buckwheat
  # AF_buckwheat_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_buckwheat_yield
  
  AF_total_buckwheat_cost <- AF_buckwheat_management_cost + AF_buckwheat_labour_cost #+ AF_buckwheat_PPcost
  
  #rye
  # AF_rye_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_rye_yield
  
  AF_total_rye_cost <- AF_rye_management_cost + AF_rye_labour_cost #+ AF_rye_PPcost
  
  #flax
  # AF_flax_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_flax_yield
  
  AF_total_flax_cost <- AF_flax_management_cost + AF_flax_labour_cost #+ AF_flax_PPcost
  
  #org_soy
  # AF_org_soy_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_org_soy_yield
  
  AF_total_org_soy_cost <- AF_org_soy_management_cost + AF_org_soy_labour_cost #+ AF_org_soy_PPcost
  
  #spelt
  # AF_spelt_PPcost <-
  #   vv(crop_pp_cost, var_CV = var_cv_p, n_years_c) * AF_spelt_yield
  
  AF_total_spelt_cost <- AF_spelt_management_cost + AF_spelt_labour_cost #+ AF_spelt_PPcost
  
  #Total cost of arable component in AF system
  AF_total_arable_management_cost <-
    AF_total_clover_cost + AF_total_org_wheat_cost + 
    AF_total_buckwheat_cost + AF_total_rye_cost + AF_total_flax_cost +
    AF_total_org_soy_cost + AF_total_spelt_cost
  
  #Total running cost of AF system
  AF_total_running_cost <- AF_total_treerow_management_cost + AF_total_arable_management_cost + AF_total_beekeeping_cost
  
  # Hail insurance per 1000€ market value - add insurance cost now as it depends on the turnover of the farm 
  #AF_insurance <- (AF_total_benefit * hail_insurance)/1000 #hashed out for now, since "management"-variables include cost for insurance
  
  
  #Total cost of AF system
  AF_total_cost <- AF_total_investment_cost + AF_total_running_cost #+ AF_insurance
  
  #Agroforestry output: system bottomline####
  AF_bottom_line_benefit <- (AF_total_benefit - AF_total_cost) #* AF_Chance_market_fluc
  #AF_bottom_line_benefit_farm <- (AF_farm_benefit - AF_total_cost) * AF_Chance_market_fluc
  
  #Calculating NPVs and Cash Flows####
  #AF System
  AF_NPV <- discount(AF_bottom_line_benefit, discount_rate=discount_rate_p,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow <- discount(AF_bottom_line_benefit,discount_rate=discount_rate_p,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  #Treeless system
  #Treeless system bottomline####
  Treeless_bottom_line_benefit <- (Treeless_total_benefit - Treeless_total_cost) #* AF_Chance_market_fluc
  
  NPV_treeless_system <- discount(Treeless_total_benefit, discount_rate = discount_rate_p,
                                  calculate_NPV = TRUE) #NVP of monoculture arable system 
  Treeless_cash_flow <- discount(Treeless_total_benefit, discount_rate = discount_rate_p,
                                 calculate_NPV = FALSE) #Cash flow of monoculture system
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of monoculture system
  
  # #AF only farm-level without intangibles
  # AF_NPV_farm <- discount(AF_bottom_line_benefit_farm, discount_rate=discount_rate_p,
  #                         calculate_NPV = TRUE)#NVP of AF system
  # AF_cash_flow_farm <- discount(AF_bottom_line_benefit_farm,discount_rate=discount_rate_p,
  #                               calculate_NPV = FALSE)#Cash flow of AF system
  # AF_cum_cash_flow_farm <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  #Tradeoff (difference between AF system and treeless system)
  Tradeoff_benefit <- AF_bottom_line_benefit - Treeless_bottom_line_benefit
  #Tradeoff_benefit_farm <- AF_bottom_line_benefit_farm - Treeless_bottom_line_benefit
  
  NPV_tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate_p,
                           calculate_NPV = TRUE )
  # NPV_tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate_p,
  #                               calculate_NPV = TRUE )
  CF_Tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate_p,
                          calculate_NPV = FALSE )
  # CF_Tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate_p,
  #                              calculate_NPV = FALSE )
  Cum_CF_Tradeoff <- cumsum(CF_Tradeoff)
  # Cum_CF_Tradeoff_farm <- cumsum(CF_Tradeoff_farm)
  
  #Defining what output variables the following Monte Carlo Simulation should create #####
  return(list(NPV_Agroforestry_System = AF_NPV,
              NPV_Treeless_System = NPV_treeless_system,
              #NPV_AF_Farm_level = AF_NPV_farm,
              NPVtrade_off = NPV_tradeoff,
              #NPVtradeoff_farm_level = NPV_tradeoff_farm,
              AFcashflow = AF_cash_flow,
              #AFcashflow_farm_level = AF_cash_flow_farm,
              AFcumcashflow = AF_cum_cash_flow,
              #AFcumcashflow_farm_level = AF_cum_cash_flow_farm,
              Treelesscashflow = Treeless_cash_flow,
              Treelesscumcashflow = Treeless_cum_cash_flow
              #treeless costs#treeless benefits #AF costs #AF benefits
  ))
}

#Run the Monte Carlo analysis of the model
# mcSimulation_results <- mcSimulation(
#   estimate = estimate_read_csv(fileName = "Hof_luetke_input_table.csv"),
#   model_function = AF_benefit,
#   numberOfModelRuns = 10000,
#   functionSyntax = "plainNames")

# Print the output using make_variables function
#print(generated_variables)

# write.csv(mcSimulation_results, file = "hof_luetke_variables_output.csv", row.names = FALSE)

# PLOTS####
# plot NPV distributions
# NPV Tradeoff
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPVtrade_off", "NPV_Treeless_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome as NPV in € for 7.87 ha",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system over 60 years with intangibles"),
                   legend.position="bottom")
ggsave(
  filename = "images/NPV_tardeoff_Treeless_vs_NPV_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

# plot_distributions(mcSimulation_object = mcSimulation_results, 
#                    vars = c("NPVtradeoff_farm_level", "NPV_Treeless_System"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 7,
#                    x_axis_name = "Outcome as NPV in € for 10 ha mixed fruits system",
#                    scale_x_continuous(labels = function(x) x / 100000),
#                    ggtitle("Net Present Value of the system over 60 years with intangibles"),
#                    legend.position="bottom")
# ggsave(
#   filename = "images/NPV_tradeoff_Treeless_vs_NPV_AF_farm.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )
# AF with intangibles and Treeless
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome as NPV in 10 ha mixed fruits system",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system over 60 years with intangibles"),
                   legend.position="bottom")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# AF without intangibles and Treeless
# plot_distributions(mcSimulation_object = mcSimulation_results, 
#                    vars = c("NPV_Treeless_System", "NPV_AF_Farm_level"),
#                    method = 'smooth_simple_overlay', 
#                    base_size = 7,
#                    x_axis_name = "Outcome as NPV in € for 10 ha mixed fruits system",
#                    scale_x_continuous(labels = function(x) x / 100000),
#                    ggtitle("Net Present Value of the system over 60 years without intangibles"),
#                    legend.position="right"
#                    )
# ggsave(
#   filename = "images/NPV_Treeless_vs_NPV_AF_farm.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )

# boxplots of outcome distributions
# AF with intangibles and Treeless
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_Treeless_System","NPV_Agroforestry_System"),
                                    method = 'boxplot',
                                    x_axis_name = "Outcome as NPV in € for 10 ha mixed fruits system",
                                    ggtitle("Net Present Value of the system over 60 years with intangibles"),
                                    legend.position="right")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_boxplot.png",
  plot = last_plot()
)
# AF without intangibles and Treeless
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
#                                     vars = c("NPV_Treeless_System","NPV_AF_Farm_level"),
#                                     method = 'boxplot',
#                                     x_axis_name = "Outcome as NPV in € for 10 ha mixed fruits system",
#                                     ggtitle ("Net Present Value of the system over 60 years without intangibles"),
#                                     legend.position="right")
# 
# ggsave(
#   filename = "images/NPV_Treeless_vs_NPV_AF_farm_boxplot.png",
#   plot = last_plot()
# )

# value of the decision (difference in NPV between AF and treeless
# tradeoff between treeless and AF with intangibles
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPVtrade_off",
                                    old_names = "NPVtrade_off",
                                    new_names = "NPV (over 40 years) of the decision",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "Net decision outcome (NPV in Euro)")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_Tradeoff.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# tradeoff between treeless and AF without intangibles
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
#                                     vars = "NPVtradeoff_farm_level",
#                                     old_names = "NPVtradeoff_farm_level",
#                                     new_names = "NPV (over 60 years) of the decision: do mixed fruits agroforestry intervention on 10 hectares without intangible benefits",
#                                     method = 'boxplot_density',
#                                     y_axis_name = "Probability",
#                                     x_axis_name = "Net decision outcome (NPV in Euro)")
# ggsave(
#   filename = "images/NPV_Treeless_vs_NPV_AF_farm_Tradeoff.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )

# Cashflow analysis
# AF with intangiles
decisionSupport::plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Annual Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue"
)
ggsave(
  filename = "images/AnnualCashflow_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#Treeless
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "Treelesscashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Annual Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/AnnualCashflow_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# AF without intangiles
# plot_cashflow(
#   mcSimulation_object = mcSimulation_results,
#   cashflow_var_name = "AFcashflow_farm_level",
#   x_axis_name = "Timeline of intervention [a]",
#   y_axis_name = "Annual Cashflow [€]",
#   color_25_75 = "navajowhite",
#   color_5_95 = "green4",
#   color_median = "darkblue",
# )
# ggsave(
#   filename = "images/AnnualCashflow_AF_farmlevel.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )
#Cumulative cashflow of AF with intangiles
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcumcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Cumulative Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/CumulativeCashflow_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)


# Cumulative cashflow of treeless
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "Treelesscumcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Cumulative Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/AnnualCashflow_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#Cumulative cashflow of AF without intangiles
# plot_cashflow(
#   mcSimulation_object = mcSimulation_results,
#   cashflow_var_name = "AFcumcashflow_farm_level",
#   x_axis_name = "Timeline of intervention [a]",
#   y_axis_name = "Cumulative  Cashflow [€]",
#   color_25_75 = "navajowhite",
#   color_5_95 = "green4",
#   color_median = "darkblue",
# )
# ggsave(
#   filename = "images/CumulativeCashflow_AF_farmlevel.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )

#Projection to Latent Structures (PLS) analysis
# AF with intangibles
pls_result_AF <- plsr.mcSimulation(object = mcSimulation_results,
                                   resultName = names(mcSimulation_results$y)[1], ncomp = 1)
plot_pls(pls_result_AF, input_table = input_file, cut_off_line = 1, threshold = 0.5)
ggsave(
  filename = "images/PLS_VIP_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#treeless
pls_result_treeless <- plsr.mcSimulation(object = mcSimulation_results,
                                         resultName = names(mcSimulation_results$y)[2], ncomp = 1)
plot_pls(pls_result_treeless, input_table = input_file, cut_off_line = 1, threshold = 0.5)
ggsave(
  filename = "images/PLS_VIP_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#AF without intangibles
# pls_result_AF_farm <- plsr.mcSimulation(object = mcSimulation_results,
#                                         resultName = names(mcSimulation_results$y)[3], ncomp = 1)
# plot_pls(pls_result_AF_farm, input_table = input_file, cut_off_line = 1, threshold = 0.5)
# ggsave(
#   filename = "images/PLS_VIP_AF_farmlevel.png",
#   plot = last_plot(),
#   width = 5, 
#   height = 3
# )


#Value of Information Analysis using decisionSupport package
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[4:5])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPVtrade_off")
plot_evpi(evpi, decision_vars = "NPVtrade_off")


#END!!!!##