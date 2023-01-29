

pop <- read.csv("TotalPopulation.csv", header = FALSE)

gdp <- read.csv("gdp.csv")

View(pop)

library(tidyverse)
library(dplyr)
library(ggplot2)



colnames(pop) <- c("State", as.character(pop[3, 2:7]))

pop <- pop[4:nrow(pop),]

pop %>% 
  gather(2:7, key = "Year", value = "Population") -> pop1

View(pop1)

View(gdp)

colnames(gdp) <- as.character(gdp[2,])

gdp <- gdp[3:nrow(gdp), ]

gdp %>%
  gather(2:7, key = "Year", value = "GDP") -> gdp2

View(gdp2)

write.csv(pop1, "population.csv")
write.csv(gdp2, "gdp.csv")
