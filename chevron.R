
invest <- read.csv("data.csv")


colnames(invest)


library(tidyverse)
library(dplyr)
library(ggplot2)



## check NAs
# invest2 <- invest[!complete.cases(invest),]

## remove NA rows
invest <- na.omit(invest) 

unique(as.factor(invest$Year))

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



## assistance vs year
ggplot(invest4, aes(Year, log(TotalAmountofAssistance), group = State)) +
  geom_line(aes(col = State), size = 1) +
  geom_point(size = 3, aes(col = State)) +
  ylab("log(Assistance)")

ggplot(invest4, aes(Year, TotalAmountofAssistance, group = State)) +
  geom_line(aes(col = State), size = 1) +
  geom_point(size = 3, aes(col = State)) +
  ylab("Assistance")


ggplot(invest4, aes(Year, CO2.Emissions..Mmt.), group = State) +
  geom_line(aes(col = State)) +
  geom_point()

# View(invest4)


## PLS

labels <- read.csv("state_labels.csv", header = FALSE)

cbind(labels, invest4) -> invest5


library(pls)


#make this example reproducible
set.seed(1)


table(labels)

## subset, get group x rows
invest5 %>%
  filter(V1 == 2) %>%
  select(-c(V1, State, Year)) -> invest6

n <- nrow(invest6)

#fit PCR model
model <- plsr(TotalAmountofAssistance ~ ., data=invest6, scale=TRUE, validation="CV")

# summary(model)


validationplot(model)


seq1 <- seq(1, n)

seq2 <- seq(5, n, 5) # test

seq3 <- seq1[-seq2] # train

#define training and testing sets
train <- invest6[seq3, ]
y_test <- invest6$TotalAmountofAssistance
test <- invest6[seq1, ]



#use model to make predictions on a test set
model1 <- plsr(TotalAmountofAssistance ~ ., data=train, scale=TRUE, validation="CV")
pcr_pred <- predict(model1, test, ncomp=2)

#calculate RMSE
sqrt(mean((pcr_pred - y_test)^2))








