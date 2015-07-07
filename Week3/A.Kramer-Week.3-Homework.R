#----------------------------------------------
#
# Data Science 350
# Aleksey Kramer
# Homework - Week 3
# 
#----------------------------------------------

# Create doors.  0 means goat, 1 means prise
doors <- c(0,0,0)

# Set the number of simulations (n)
n = 100000

# Simulation for no switching
before_no_switches <- Sys.time()
no_switches = sapply(1:n, function(x) {
    # Set location of the prise behind one of the doors (randomly)
    prise <- sample(1:3, 1)
    doors[prise] <- 1
    
    # Select a random answer to stay with (no switching doors)
    attempt <- sample(1:3, 1)
    stay <- doors[attempt] == 1
    
    # Reset the doors to initial conditions
    doors[prise] <- 0
    
    # Return TRUE/FALSE converted to numbers
    return(stay + 0)
})
after_no_switches <- Sys.time()

# Accumulating results from running no_switches simulation
probability_of_wins_if_not_switched <- length(no_switches[no_switches == 1]) / n
variance_if_not_switched <- var(no_switches)
time_to_run_not_switched <- after_no_switches - before_no_switches

# Simulation for switching
before_switches <- Sys.time()
switches = sapply(1:n, function(x) {
    # Set location of the prise behind one of the doors (randomly)
    prise <- sample(1:3, 1)
    doors[prise] <- 1
    
    # Select a random answer to switch from later
    initial_attempt <- sample(1:3, 1)
    
    # Host opens a door with no prise
    host_door <- sample(1:3, 1)
    if(initial_attempt == host_door) {
        repeat {
            host_door <- sample(1:3, 1)
            if(initial_attempt != host_door) {
                if(doors[host_door] == 0)
                break
            }
        }
    } else if(doors[host_door] != 0) {
        repeat {
            host_door <- sample(1:3, 1)
            if(initial_attempt != host_door) {
                if(doors[host_door] == 0)
                    break
            }
        }
    }
    
    # Find the switch door
    repeat {
        switch_guess <- sample(1:3, 1)
        if(switch_guess != initial_attempt) {
            if(switch_guess != host_door) {
                break
            }
        }
    }
    
    # Test if the guess is correct and the prise is won
    result <- doors[switch_guess] == 1
    
    # Reset the doors to initial conditions
    doors[prise] <- 0
    
    # Return TRUE/FALSE converted to numbers
    return(result + 0)
})
after_switches <- Sys.time()

# Accumulating results for running switches simulation
probability_of_wins_if_switched <- length(switches[switches == 1]) / n
variance_if_switched <- var(switches)
time_to_run_switches <- after_switches - before_switches

# Printing out results
print(paste("Probability of winning if not switched is:", probability_of_wins_if_not_switched))
print(paste("Variance of winning if not switched is:", variance_if_not_switched))
print(paste("Time to run not switched:", time_to_run_not_switched))
print("")
print(paste("Probability of winning if switched is:", probability_of_wins_if_switched))
print(paste("Variance of winning if switched is:", variance_if_switched))
print(paste("Time to run switches:", time_to_run_switches))
