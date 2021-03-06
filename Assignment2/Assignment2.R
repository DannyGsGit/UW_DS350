#### Assignment 2 ####
# Prepared By: Danny Godbout

# Goal: 
# Write an R-script to compute the Monty Hall Probabilities with
# simulations (get probabilities AND variances for switching and
# not switching).



#### Monty Hall Simulation Function ####
# Setup: 3 doors; 2 = Goat, 1 = Car
# 1- Pick a door
# 2- Host opens one of the remaining 2 doors, revealing a goat
# 3- Either keep current choice, or switch doors


f_monty_hall_single_run <- function() {
  # This function runs a single cycle of the monty hall problem, returning
  # results for scenarios where we keep the first choice door and where
  # we switch.
  #
  # Input Args: N/A
  #
  # Output Args: 
  # 1) results: Single row data frame with 2 columns for original and switched
  #             choices. 1=win, 0=loss.
  
  # Define all doors
  all_doors <- 1:3
  
  # Place car behind a random door
  car_door <- sample(all_doors, 1)
  
  # Choose a door
  first_choice_door <- sample(all_doors, 1)
  # first_choice_door <- car_door
  
  # Host chooses a remaining goat door
  remaining_goats <- all_doors[all_doors != car_door & all_doors != first_choice_door]
  
  if(length(remaining_goats) == 1){
    host_door <- remaining_goats
  }else{
    host_door <- sample(remaining_goats, 1)
  }
  
  # Switch
  switched_door <- all_doors[all_doors != first_choice_door & all_doors != host_door]
  
  # Which choice won a car?
  switch_win <- ifelse(switched_door == car_door, "car", "goat")  # The new, switch choice won
  original_win <- ifelse(first_choice_door == car_door, "car", "goat")  # The original door won
  
  # Merge
  results <- data.frame(switch = switch_win, original = original_win)
  
  # Return
  return(results)
}





#### Run multiple time ####

# Set number of runs
n <- 1000

# Run simulation
simulation_result<- data.frame(t(replicate(n, f_monty_hall_single_run(), simplify = "matrix")))

# Unlist column formats for analysis
simulation_result <- data.frame(apply(simulation_result, 2, unlist))
simulation_result$run <- 1:n





#### Plot results ####

# Melt simulation results for plotting
library(reshape2)
plot.data <- melt(simulation_result, id = "run")

# Build ggplot histogram with facet
library(ggplot2)
p.dist <- ggplot(plot.data, aes(value, fill = factor(value))) +
  geom_bar() +
  facet_grid(variable ~ .)
print(p.dist)





#### Calculate summary stats ####

## Probability function
f_probability <- function(data, n) {
  # p = win / n
  probability <- length(which(data == "car")) / n
  return(probability)
}

## Variance function
f_variance <- function(p, n) {
  # Var = np(1-p)
  variance <- n * p * (1 - p)
  return(variance)
}


# Summary Statistics for Switch Strategy
prob.switch <- f_probability(data = simulation_result$switch, n = n)
var.switch <- f_variance(p = prob.switch, n = 1)

# Summary Statistics for Not-Switching Strategy:
prob.original <- f_probability(data = simulation_result$original, n = n)
var.original <- f_variance(p = prob.original, n = 1)

# Combine stats to display:
stats.df <- data.frame(Switch = c(prob.switch, var.switch),
                       Original = c(prob.original, var.original))
row.names(stats.df) <- c("Probability", "Variance")
print(stats.df)
