#demonstrate then have students play with 
#duration and timesteps to see when and how much 
#rk is better than euler

#numerical methods: approximate (simulation, iterative calculations)
#analytical methods: exact (solve a math problem)

#dT/dt = -aT
#The change in temperature per chunk of time
# equals our proportionality constant times 
# the starting temperature (a is in mins)

#dT/(1) = -0.5 * 100
# dT = -50

#so T = 50C after one minute

#dT/(1) = -0.5 * (50)
#dT = -25

#so T = 25C after two minutes

#Analytical solution:
# T(t) = T_init * exp(-a * t)
# T(t) = T_init * e ^ (-a * t)
# The temperature at any time (t) equals
# the initial tempurature * exp(-constant * t)

# so: The temperature at 2 minutes is found by
# T(2) = 100 * exp(-0.5 * 2)
# T at 2 minutes = 36.79

#Numerical = 25
#Analytical = 36.79

#Why are these so far apart? How could we make them agree more?

library(tidyverse)
theme_set(theme_classic())

T_init <- 100 #degrees celsius
a <- 0.5      #constant of proportionality that reflects 
              #the temperature difference between the coffee and the ambient air
duration <- 5 #in minutes (do this for longer to discuss prediction beyond limits)
timestep <- 1 #minutes

#create empty vectors to fill in model
coffee_euler <- rep(NA, duration/timestep)
coffee_euler[1] <- T_init
coffee_rk <- coffee_euler

for(x in 2:(length(coffee_euler)+1)){
  #euler's method
  cooling <- -(a * timestep) * coffee_euler[x - 1]
  
  coffee_euler[x] <- coffee_euler[x - 1] + cooling
  
  #runge_kutta... first one overestimates, so the next step 
  #will underestimate. Taking the avg gives better approximation
  cooling <- -(a * timestep) * coffee_rk[x - 1]
  
  coffee_rk[x] <- coffee_rk[x - 1] + cooling
  
  cooling2 <- -(a * timestep) * coffee_rk[x]
  
  avgcooling <- (cooling + cooling2) / 2
  
  coffee_rk[x] <- coffee_rk[x - 1] + avgcooling
}

#create times for plotting and for analytical solution
time <- seq(0, duration, timestep)

#analytical solution
coffee_analytical <- T_init * exp(-a * time)

#prep output
output <- as_tibble(cbind(time, coffee_euler, coffee_analytical, coffee_rk))
output_long <- pivot_longer(output, -time, values_to = "temp")

#plot
ggplot(data = output_long, aes(x = time, y = temp, color = name))+
  geom_line()+
  geom_point()
