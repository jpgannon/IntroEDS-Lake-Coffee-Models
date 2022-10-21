#Adapted from: https://scied.ucar.edu/activity/very-simple-climate-model-activity

#Equation
#T = To + S * log2(C / Co)

#T is new/current temp
#To is known temp and a ref time 
#S is the "climate sensitivity" factor 
# this is the temp rise per doubling of CO2
#C is new/current CO2 concentration
#Co is the known atmospheric CO2 concentration for 
# the same ref time as To

library(tidyverse)
theme_set(theme_classic())

S <- 3 #degrees C for every doubling of CO2

#Carbon emissions, gigatons of carbon
Emissions <- c(4.14, 4.68, 5.59, 5.86, 6.53, 
               6.68, 7.34, 7.79, 7.79, 8.93,
               9.84, 10.34)

#CO2 concentration #ppm
CO2 <- rep(NA, length(Emissions))
CO2[1] <- 316.91 #starting CO2 concentration

#Temperature degrees C
Temp <- rep(NA, length(Emissions))
Temp[1] <- 13.8 #starting temperature
#Actual global temps, for comparison
Temp_actual <- c(13.98, 13.89, 13.92, 13.92, 14.05, 
                 14.08, 14.19, 14.24, 14.40, 14.47,
                 14.45, 14.65)

#years
year <- seq(from = 1960, by = 5, length.out = length(Emissions))

#Calculate CO2 concentration from emissions
# 55% of emissions are absorbed by ocean and biosphere
# Every 2.3 GtC of emissions that get to the atmosphere 
# raises CO2 1ppm
# 0.1% CO2 leaves atmosphere naturally 
CO2added <- ((Emissions * 0.45) / 2.3) * 5

for(x in 2:length(Emissions)){
  #multiply by 0.999 because 1% CO2 leaves naturally
  CO2[x] <- (CO2[x-1] + CO2added[x]) * 0.999
  
  #T = To + S * log2(C / Co)
  Temp[x] <- Temp[x-1] + (S * log2(CO2[x]/CO2[x-1]))
}

#Format data for plotting
Data <- as_tibble(cbind(year, CO2, Temp, Emissions))
Temps <- as_tibble(cbind(year, Temp, Temp_actual))
if(length(Temps$Temp_actual) > 12)
  is.na(Temps$Temp_actual[13:length(Emissions)]) = TRUE

Data_long <- pivot_longer(Data, cols = CO2:Emissions)

#plot 3 panel plot of CO2, emissions, and temp
ggplot(Data_long, aes(x = year, y = value))+
  geom_point()+
  geom_line()+
  facet_wrap(facets = "name", ncol = 1, scales = "free_y")

#plot just temperature, compare to measured temp
#and show line at 3 deg C warming
ggplot(Temps, aes(x = year, y = Temp, color = "modeled"))+
  geom_line()+ geom_point()+
  geom_line(aes(y = Temp_actual, color = "measured"))+
  geom_point(aes(y = Temp_actual, color = "measured"))+
  geom_hline(yintercept = 15.8, color = "red")
  