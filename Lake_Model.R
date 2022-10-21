#system modeling
#stocks, flows

library(tidyverse)
theme_set(theme_classic())

lake_size <- 10
lake_initial <- 0
inflow <- 2
inflow <- c(3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0)
outflow <- 2
outflow_vol <- 8
timesteps <- 20

lake_data <- 0
outflow_data <- 0

if(length(inflow == 1))
  inflow <- rep(inflow, timesteps)

for(i in 1:timesteps){
  if(i == 1) lake <- lake_initial
  
  lake <- lake + inflow[i]
  lake_data[i] <- lake
  
  if(lake > outflow_vol){
    lake <- lake - outflow
    outflow_data[i] <- outflow
  }else{
    outflow_data[i] <- 0
  }
  
}

time <- seq(1:timesteps)
data <- as_tibble(cbind(time, inflow, outflow_data, lake_data))
data_long <- pivot_longer(data, -time, names_to = "model_out")

ggplot(data = data_long, aes(x = time, y = value, color = model_out))+
  geom_line()
