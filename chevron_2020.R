
df <- read.csv("data.csv")

df2020 <- read.csv("2020data.csv")

pop <- read.csv("population.csv")

gdp <- read.csv("gdp.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)


pop %>%
  arrange(State) -> pop1

gdp %>% 
  arrange(State) -> gdp1


df1 <- bind_rows(df, df2020)


df1 %>% 
  arrange(State) -> df2




## remove NA rows
invest <- na.omit(df2)

## variables: spreading MSN
invest %>% 
  select(c(State, Year, MSN, Amount)) %>% 
  spread(key = MSN, value = Amount) -> invest2



## collapsing rows

invest %>%
  select(c(State, Year, CO2.Emissions..Mmt., TotalNumberofInvestments, TotalAmountofAssistance)) %>%
  distinct() %>%
  select(c(CO2.Emissions..Mmt., TotalNumberofInvestments, TotalAmountofAssistance)) -> invest3
# View(invest3)

cbind(invest2, invest3) -> invest4

## remove repated variables

invest4 %>% 
  select(-c("BDFDB", "BFPRP", "CLPRP", "COPRK", 
            "NGMPK", "NGMPP", "PAPRP", "TEPRB", "TETCB", "WDEXB", "WDPRB",
            "WDTCB", "WSTCB")) -> invest4

# write.csv(invest4, "invest2020.csv")


invest4 <- bind_cols(invest4, gdp1$GDP, pop1$Population)

invest4 %>%
  rename(GDP = ...22, Population = ...23) -> invest4


## PLS

labels <- read.csv("state_labels.csv", header = FALSE)


## get group number for each state
seq1 <- labels[seq(1, 250, 5),]
## create group number for each row
V1 <- rep(seq1, each = 6)

cbind(V1, invest4) -> invest5



library(pls)


#make this example reproducible
set.seed(1)


table(labels)







predict <- rep(NA, 5)




k <- 1




for (i in 0:4) {
  
  
 
  cluster <- i
  
  ## subset, get group x rows
  invest5 %>%
    filter(V1 == cluster) -> invest6

  
  
  
  n <- nrow(invest6)
  
  #fit PLR model
  # model <- plsr(TotalAmountofAssistance ~ ., data=invest7, scale=TRUE, validation="CV")
  

  
  
  
  
  #define training and testing sets
  invest6 %>% 
    filter(Year != 2020) -> train
  
  invest6 %>%
    filter(Year == 2020) -> test
  y_test <- test$TotalAmountofAssistance
  
  
  if (i == 1 | i == 2) {
    predict[k] <- sum(train$TotalAmountofAssistance) / n
    k <- k + 1
  }
  else {
  
                  
                  
  #use model to make predictions on a test set
  model1 <- plsr(TotalAmountofAssistance ~ ., data=select(train, -c(V1, State, Year, GDP, Population)), scale=TRUE, validation="CV")
  pcr_pred <- predict(model1, test, ncomp=2)
  
  # validationplot(model)
  
  #calculate RMSE
  RMSE <- sqrt(mean((pcr_pred - y_test)^2))
  
  predict[k] <- list(pcr_pred)
  
  k <- k + 1
  
  print(RMSE)
  
  print(summary(model1))
  }
  
}

prediction <- unlist(predict)
# prediction[26] <- 0
# prediction[27] <- 0


## get y
invest5 %>%
  filter(Year == 2020) %>%
  select(TotalAmountofAssistance) -> y_val

y <- as.vector(y_val$TotalAmountofAssistance)
yHat <- as.vector(prediction)

# plot(y, yHat)

final <- data.frame(y, yHat)




ggplot(data = final, aes(y, yHat)) +
  geom_point(size = 3, color = "darkblue") +
  xlab("Investment") + ylab("Prediction") +
  labs(title = "Prediction of Investments using Partial Least Squares") +
  theme_light()


RMSE <- sqrt(mean((y - yHat)^2))

RMSE



