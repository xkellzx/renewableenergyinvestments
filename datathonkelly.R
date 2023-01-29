setwd("C:/Users/Kelly/Downloads/2023Spring/Datathon2023")
invest = read.csv("investment.csv")
invest2020 = read.csv("invest2020.csv")
statelabels = read.csv("state_labels.csv", header = FALSE)
colnames(statelabels) = "StateLabels"
finaldata = read.csv("training_and_testing_data.csv") # full model
# cross validation (manual)
#finaldata = finaldata[,!(names(finaldata) %in% c("GDP", "Population"))]
#finaldata = finaldata[,!(names(finaldata) %in% c("GDP", "Population", "Net.generation", "Net.summer.capacity"))]
finaldata = finaldata[,!(names(finaldata) %in% c("GDP", "Population", "Net.generation", "Net.summer.capacity", "Average.retail.price", "Total.retail.sales"))] # best model
#finaldata = finaldata[,!(names(finaldata) %in% c("GDP", "Population", "Net.generation", "Net.summer.capacity", "Average.retail.price", "Total.retail.sales", "Disbursement"))]
#finaldata = finaldata[,!(names(finaldata) %in% c("GDP", "Population", "Net.generation", "Net.summer.capacity", "Disbursement"))]
#finaldata = finaldata[,!(names(finaldata) %in% c("Net.generation", "Net.summer.capacity", "Average.retail.price", "Total.retail.sales"))]
View(finaldata)

# clean disbursement data
disburse = read.csv("fiscal_year_disbursements.csv")
disburse <- disburse[disburse$Fiscal.Year %in% c(2015, 2016, 2017, 2018, 2019),]
disburse <- disburse[disburse$Fiscal.Year %in% c(2020),]

library(dplyr)

# Group by sum using dplyr
dis_tbl <- disburse %>% group_by(State, Fiscal.Year) %>% 
  summarise(sum_dis = sum(Disbursement),
            .groups = 'drop')
View(dis_tbl)

# Convert tibble to df
df <- dis_tbl %>% as.data.frame()
df <- df[-(1:5),]
View(df)

df1 <- merge(x = invest, y = df, 
             by.x=c("State", "Year"), 
             by.y=c("State", "Fiscal.Year"), all.x = TRUE)
df1 = df1[,-3]
colnames(df1)[35] = "Disbursement"
View(df1)
unique(df1$State) 

# backup data
invest_backup <- finaldata# good idea in general
invest <- invest_backup

# clean clustered new df
invest <- finaldata[finaldata$Year %in% c(2015, 2016, 2017, 2018, 2019),]
invest <- cbind(statelabels, invest)
invest_test <- finaldata[finaldata$Year == 2020,]
index <- seq(1, 250, 5)
statelabels_test <- statelabels[index,]
invest_test <- cbind(statelabels_test, invest_test)

View(invest)
View(invest_test)

statecount <- table(invest$StateLabels)
table(invest_test$statelabels_test)


dat1 = invest[, c(-2, -3, -4)]
dat1_test = invest_test[, c(-2, -3, -4)]

dat1 = na.omit(dat1)
dat <- dat1[,!(names(dat1) %in% "TotalAmountofAssistance")]

dat1_test = na.omit(dat1_test)
dat_test <- dat1_test[,!(names(dat1_test) %in% "TotalAmountofAssistance")]

View(dat1)
View(dat1_test)

View(dat)
View(dat_test)

# create clusters
index <- seq(1, statecount[1], 5)
cluster1_states <- na.omit(invest[invest$StateLabels == 0,]$State[index])
index <- seq(1, statecount[2], 5)
cluster2_states <- na.omit(invest[invest$StateLabels == 1,]$State[index])
index <- seq(1, statecount[3], 5)
cluster3_states <- na.omit(invest[invest$StateLabels == 2,]$State[index])
index <- seq(1, statecount[4], 5)
cluster4_states <- na.omit(invest[invest$StateLabels == 3,]$State[index])
index <- seq(1, statecount[5], 5)
cluster5_states <- na.omit(invest[invest$StateLabels == 4,]$State[index])

clust1 <- dat[dat$StateLabels == 0,]
clust2 <- dat[dat$StateLabels == 1,]
clust3 <- dat[dat$StateLabels == 2,]
clust4 <- dat[dat$StateLabels == 3,]
clust5 <- dat[dat$StateLabels == 4,]

clust1 = clust1[,-1]
clust2 = clust2[,-1]
clust3 = clust3[,-1]
clust4 = clust4[,-1]
clust5 = clust5[,-1]

clust1_test <- dat_test[dat_test$statelabels_test == 0,]
clust2_test <- dat_test[dat_test$statelabels_test == 1,]
clust3_test <- dat_test[dat_test$statelabels_test == 2,]
clust4_test <- dat_test[dat_test$statelabels_test == 3,]
clust5_test <- dat_test[dat_test$statelabels_test == 4,]

clust1_test = clust1_test[,-1]
clust2_test = clust2_test[,-1]
clust3_test = clust3_test[,-1]
clust4_test = clust4_test[,-1]
clust5_test = clust5_test[,-1]

View(clust1)
dim(clust1)
View(clust1_test)
dim(clust1_test)

library(Metrics)

# pca, regression, and predictions

do_pca <- function(i, j, k){
  
  clust <- t(i)
  X <- data.matrix(scale(clust))
  pcob <- prcomp(X)
  pc1 <- pcob$rotation[,1]
  temp <- dat1[dat1$StateLabels == k,][,-1]
  assist <- temp[,"TotalAmountofAssistance"]
  train = as.data.frame(cbind(pc1, assist))
  
  clust_test <- t(j)
  X_test <- data.matrix(scale(clust_test))
  pcob_test <- prcomp(X_test)
  pc1_test <- pcob_test$rotation[,1]
  temp_test <- dat1_test[dat1_test$statelabels_test == k,][,-1]
  assist_test <- temp_test[,"TotalAmountofAssistance"]
  test = as.data.frame(cbind(pc1_test, assist_test))
  
  fit = lm(assist ~ pc1, data = train)
  print(summary(fit))
  
  beta0 <- summary(fit)$coef[1]
  beta1 <- summary(fit)$coef[2]
  pred <- beta0 + beta1*(test$pc1_test)
  
  cat("Adj R squared", summary(fit)$adj.r.squared, " ")
  
  norm <- (max(test$assist_test) - min(test$assist_test))
  cat("RMSE", rmse(test$assist_test, pred)/norm)
  
  plot(test$pc1_test, test$assist_test)
  lines(test$pc1_test, pred, col = "red")
}

####### manually bc function isnt working ig ######
# CLUSTER 1
clust <- t(clust1)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 0,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust1_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 0,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))

beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred <- beta0 + beta1*(test$pc1_test)

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test, main = "Cluster 1", xlab = "Principal Component 1", ylab = "Amount of Assistance")
lines(test$pc1_test, pred, col = "purple")

par(mfrow=c(2,3))
c1 <- cbind(cluster1_states, pred)

# accounted variance plot
par(mfrow=c(1,1))
prvar <- pcob$sdev^2
prvar

pve <- prvar / sum(prvar)
pve

par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", main = "Cluster 1", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# CLUSTER 2
clust <- t(clust2)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 1,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust2_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 1,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))

beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred <- beta0 + beta1*(test$pc1_test)

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test, main = "Cluster 2", xlab = "Principal Component 1", ylab = "Amount of Assistance")
lines(test$pc1_test, pred, col = "purple")

c2 <- cbind(cluster2_states, pred)

# accounted variance plot
par(mfrow=c(1,1))
prvar <- pcob$sdev^2
prvar

pve <- prvar / sum(prvar)
pve

par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", main = "Cluster 2", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# CLUSTER 3
clust <- t(clust3)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 2,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust3_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 2,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))

beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred <- beta0 + beta1*(test$pc1_test)

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test, main = "Cluster 3", xlab = "Principal Component 1", ylab = "Amount of Assistance")
lines(test$pc1_test, pred, col = "purple")

c3 <- cbind(cluster3_states, pred)

# CLUSTER 4
clust <- t(clust4)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 3,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust4_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 3,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))

beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred <- beta0 + beta1*(test$pc1_test)

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test, main = "Cluster 4", xlab = "Principal Component 1", ylab = "Amount of Assistance")
lines(test$pc1_test, pred, col = "purple")

c4 <- cbind(cluster4_states, pred)

# CLUSTER 5
clust <- t(clust5)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 4,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust5_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 4,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))

beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred <- beta0 + beta1*(test$pc1_test)

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test, main = "Cluster 5", xlab = "Principal Component 1", ylab = "Amount of Assistance")
lines(test$pc1_test, pred, col = "purple")

c5 <- cbind(cluster5_states, pred)

# save to new df
predicted_assist_per_state <- rbind(c1, c2, c3, c4, c5)
colnames(predicted_assist_per_state)[1] = "State"
colnames(predicted_assist_per_state)[2] = "Assistance"
View(predicted_assist_per_state)
#write.csv(df, "predicted_assist_per_state_2020.csv")

#############################################################################

clust <- t(clust5)
X <- data.matrix(scale(clust))
pcob <- prcomp(X)
pc1 <- pcob$rotation[,1]
temp <- dat1[dat1$StateLabels == 4,][,-1]
assist <- temp[,"TotalAmountofAssistance"]
#assist <- scale(assist)
train = as.data.frame(cbind(pc1, assist))

clust_test <- t(clust5_test)
X_test <- data.matrix(scale(clust_test))
pcob_test <- prcomp(X_test)
pc1_test <- pcob_test$rotation[,1]
temp_test <- dat1_test[dat1_test$statelabels_test == 4,][,-1]
assist_test <- temp_test[,"TotalAmountofAssistance"]
#assist_test <- scale(assist_test)
test = as.data.frame(cbind(pc1_test, assist_test))

fit = lm(assist ~ pc1, data = train)
print(summary(fit))
beta0 <- summary(fit)$coef[1]
beta1 <- summary(fit)$coef[2]
pred_val <- beta0 + beta1*test$pc1_test

pred = predict(fit, newdata = data.frame(test))    
pred

cat("Adj R squared", summary(fit)$adj.r.squared, " ")

norm <- (max(test$assist_test) - min(test$assist_test))
cat("RMSE", rmse(test$assist_test, pred)/norm)

plot(test$pc1_test, test$assist_test)
lines(test$pc1_test, pred_val, col = "purple")
length(test$pc1_test)
length(pred)
length(fitted(fit))

########################################################################
# PCA
states <- invest$State
apply(invest[, c(-1, -2, -3)], 2, mean)
apply(invest[, c(-1, -2, -3)], 2, var)
dat1 <-  dat1[,!(names(dat1) %in% c("GETCB", "TotalAmountofAssistance"))]
View(dat1)

t.dat <- t(dat1)
View(t.dat)

pr.out <- prcomp(t.dat, scale = TRUE)
pr.out$center
pr.out$scale
pr.out$rotation
biplot(pr.out , scale = 0)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out , scale = 0)

pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var

pve <- pr.var / sum(pr.var)
pve

par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained", ylim = c(0, 1),
        type = "b")
plot(cumsum(pve), xlab = "Principal Component",
         ylab = "Cumulative Proportion of Variance Explained",
         ylim = c(0, 1), type = "b")

X <- data.matrix(scale(t.dat))
pcob <- prcomp(X)
summary(pcob)
pcob$rotation
pc1 <- pcob$rotation[,1]
assist <- dat$TotalAmountofAssistance
df = as.data.frame(cbind(pc1, assist))

cor(df$pc1, df$assist)
plot(df$pc1, df$assist)

library(splines)
fit = lm(assist ~ pc1, data = df)
summary(fit)

plot(df$pc1, df$assist)
lines(df$pc1, predict(fit), col = "red")

values <- data.frame(actual=df$assist, predicted=predict(fit))
values

library(ggplot2)
ggplot(df, aes(x=predict(fit), y=assist)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

dev.off(dev.list()["RStudioGD"]) 

# testing data
set.seed(100)  
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices 
trainingData <- cars[trainingRowIndex, ]  
testData  <- cars[-trainingRowIndex, ] 

mMod <- lm(dist ~ speed, data=trainingData)  
distPred <- predict(lmMod, testData)
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred)) 
########################################################################

sX <- svd(X)
names(sX)
round(sX$v, 3)

x = pcob$rotation
nomit <- 10
set.seed(15)
inb <- sample(seq(31), nomit)
ina <- sample(1:3, nomit, replace = TRUE)
Xna <- X
index.na <- cbind(ina , inb)
Xna[index.na] <- NA

fit.svd <- function(X, M = 1){
  svdob <- svd(X)
  with(svdob, u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[, 1:M, drop = FALSE])))
}

Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]

thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna)
mssold <- mean (( scale (Xna , xbar , FALSE)[!ismiss])^2)
mss0 <- mean (Xna[!ismiss]^2)

while (rel_err > thresh) {
  iter <- iter + 1
  Xapp <- fit.svd(Xhat, M = 1)
  Xhat[ismiss] <- Xapp[ismiss]
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss) / mss0
  mssold <- mss
  cat ("Iter:", iter, "MSS:", mss ,
  "Rel.Err:", rel_err , "\n")
}

cor(Xapp[ismiss], X[ismiss])

# correlation heat map
dat = invest[, c(-1, -2, -3)]
corr<- round(cor(dat),2) 
head(corr)

library(reshape2)
melted_corr <- melt(corr)

library(ggplot2)
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

# KMEANS
km.out <- kmeans(dat1, centers = 4, nstart = 20)
View(dat1[1:])
km.out
km.out$clusters

# VIF
library(car)
fit_full = lm(TotalAmountofAssistance ~ BFPRP + HYTCB + CO2.Emissions..Mmt. + TotalNumberofInvestments, data = dat1[-c(1, 2, 11)])
vif(fit_full)

# linear regression
fit_lin = lm(TotalAmountofAssistance ~ ., data = dat1[-c(1, 2, 11)])
summary(fit_lin)

# splines
library(splines)
table(cut(dat$TotalAmountofAssistance , 3))


fit <- lm(TotalAmountofAssistance ~ bs(year, df = 4), data = invest[state == "Texas",])
summary(fit)

par(mfrow = c(2, 3))
rss <- rep(NA, 6)
for (i in 3:6) {
  fit <- lm(TotalAmountofAssistance ~ bs(Year, df = i + 3, knots = i), data = invest[state == "Texas",])
  rss[i] <- sum(fit$residuals^2)
  plot(3:6, rss[-c(1, 2)], xlab = "Degrees of Freedom", ylab = "RSS", type = "l")
}

fit <- lm(TotalAmountofAssistance ~ poly(Year, 3), data = invest[state == "Texas",])
summary(fit)

library(splines)
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

index <- seq(1, 250, 5)
states <- invest$State[index]
for (s in states){
  fit <- lm(TotalAmountofAssistance ~ bs(Year, df = 6, knots = 3), data = invest[state == s,])
  plot(invest$Year, invest$TotalAmountofAssistance, ylab = "Total Assistance", xlab = "Year")
  
  years<-range(invest$Years)
  years.grid<-seq(from=years[1], to = years[2])
  plot(age,wage,col="grey",xlab="Age",ylab="Wages")
  points(year.grid, predict(fit, newdata = list(Year=yeare.grid)),col="darkgreen",lwd=2,type="l")
}

# random forest
set.seed(123)

train <- sample(1:nrow(dat), nrow(dat)/2)
dat.train <- dat[train, ]
dat.test <- dat[-train, ]

library(randomForest)
bag.dat <- randomForest(TotalAmountofAssistance ~ ., data = dat.train, mtry = 10,
                        importance = TRUE)
bag.dat

predict.bag <- predict(bag.dat, newdata = dat.test)
mean((predict.bag - dat.test$Sales)^2)

importance(bag.dat)
