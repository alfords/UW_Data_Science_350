#----------------------------------------------
#
# Data Science 350
# Aleksey Kramer
# Homework - Week 3
# 
#----------------------------------------------

# Create doors.  0 means goat, 1 means prise
doors = c(0,0,0)

# Set the number of simulations (n)
n = 100000
no_switches = sapply(1:n, function(x){
    # Set location of the prise behind one of the doors (randomly)
    prise <- sample(1:3,1)
    doors[prise] <- 1
    
    # Select a random answer to stay with (no switching doors)
    attempt <- sample(1:3, 1)
    stay <- doors[attempt] == 1
    
    # Reset the doors to initial conditions
    doors[prise] <- 0
    
    # Return TRUE/FALSE converted to numbers
    return(stay + 0)
})

no_switch_wins <- length(no_switches[no_switches == 1])
no_switch_loss <- length(no_switches[no_switches == 0])

probability_of_wins_if_switches <- no_switch_wins / n
probability_of_loss_if_switches <- no_switch_loss / n
variance <- var(no_switches)