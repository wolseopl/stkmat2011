rm(list=ls())

# import csv files
f = file.choose() # hourly data
df.hourly = read.csv(f, sep=",",header = TRUE, stringsAsFactors = FALSE)
f = file.choose() # number of APs data
df.aps = read.csv(f, sep=",", header = TRUE, stringsAsFactors = FALSE)

# df inner join df.ops on RouterID and Band
df <- merge(df.hourly, df.aps, by=c("RouterID","Band"))
rm(df.aps, df.hourly, f)

## cleaning the data
df <- df[complete.cases(df$AirtimeCoeff), ] # remove NaNs in AirtimeCoeffs

# convert Interface as factor
df$Interface <- as.factor(df$Interface)
class(df$Interface)

# convert Channel as factor
df$Channel <- as.factor(df$Channel)
class(df$Channel)

# convert TimeStamp to datetime
df$TimeStamp <- strptime(df$TimeStamp, format = ("%m/%d/%Y %H:%M"))
class(df$TimeStamp)
head(df$TimeStamp)

# round to the nearest hour
df$TimeStamp <- round(df$TimeStamp, "hours")
head(df$TimeStamp)

# Which attributes are stored in the POSIXlt time variable?
attributes(head(df$TimeStamp))

# Extract the value of the hour attribute as a number and add it to the data set
df$Hour <- df$TimeStamp[["hour"]]
head(df$Hour)
class(df$Hour)

# The integer is converted to factor
df$Hour <- as.factor(df$Hour)
head(df$Hour)

# divide into band tyeps
df.2p4ghz <- subset(df, Band == "2.4GHz")
df.5ghz <- subset(df, Band == "5GHz")

# remove "Band" and "TimeStamp" column
df.2p4ghz[ ,c('Band', 'TimeStamp')] <- list(NULL)
df.5ghz[ ,c('Band', 'TimeStamp')] <- list(NULL)

str(df.2p4ghz)
str(df.5ghz)

table(df.2p4ghz$Channel)
table(df.5ghz$Channel)

# drop unused levels
df.2p4ghz$Channel <- droplevels(df.2p4ghz$Channel)
table(df.2p4ghz$Channel)

df.5ghz$Channel <- droplevels(df.5ghz$Channel)
table(df.5ghz$Channel)


############### 2.4 GHz ###################

# use vapply to go through and check how many unique values there are in each column:
# return TRUE when unique values are more than 1, FALSE otherwise
vapply(df.2p4ghz, function(x) length(unique(x)) > 1, logical(1L))

# drop columns with a single value
df.2p4ghz <- df.2p4ghz[vapply(df.2p4ghz, function(x) length(unique(x)) > 1, logical(1L))]

str(df.2p4ghz)

# AirtimeCoeff > 1 is not valid for 2.4 GHz
# Remove them
head(sort(df.2p4ghz$AirtimeCoeff, decreasing = F))
head(sort(df.2p4ghz$AirtimeCoeff, decreasing = T))
df.2p4ghz <- subset(df.2p4ghz, AirtimeCoeff <= 1.00)
length(df.2p4ghz$AirtimeCoeff[df.2p4ghz$AirtimeCoeff>1])

# drop RouterID
df.2p4ghz[ ,'RouterID'] <- list(NULL)

# basic multicollinearity/singularity check using scatterplot
index <- sample(1:nrow(df.2p4ghz), size = 0.9*nrow(df.2p4ghz))
test <- df.2p4ghz[-index,]

pairs(test[1:11], cex=0.1)
pairs(test[11:21], cex=0.1)
pairs(test[21:31], cex=0.1)
pairs(test[31:41], cex=0.1)
pairs(test[41:49], cex=0.1)
pairs(test[36:45], cex=0.1)
pairs(test[c(1:5,21:26)], cex=0.1)

# variables found to be significantly collinear with other variables:

# HourInWeek, ChaminStatsAverage.goodtx, ChaminStatsAverage.glitch
# NoiseMean, NoiseStdAvg, NoiseMeanStd, ChaminStatsAverage.idle
# TxFrames, TxFail, RxFrames, RxError, idle_SumAllDevices, TxReTrans
# tx_ucast_pkts_SumAllDevices, tx_ucast_bytes_SumAllDevices
# tx_mcast_bytes_SumAllDevices, rx_ucast_bytes_SumAllDevices
# rx_mcast_bytes_SumAllDevices, rx_decrypt_succeeds_SumAllDevices

df.2p4ghz[ ,c('HourInWeek','ChaminStatsAverage.goodtx',
              'ChaminStatsAverage.glitch', 'NoiseMean',
              'NoiseStdAvg', 'NoiseMeanStd', 'ChaminStatsAverage.idle',
              'TxFrames', 'TxFail', 'RxFrames', 'RxError',
              'idle_SumAllDevices', 'TxReTrans',
              'tx_ucast_pkts_SumAllDevices', 'tx_ucast_bytes_SumAllDevices',
              'tx_mcast_bytes_SumAllDevices', 'rx_ucast_bytes_SumAllDevices',
              'rx_mcast_bytes_SumAllDevices', 'rx_decrypt_succeeds_SumAllDevices')] <- list(NULL)

set.seed(123)
# divide tha data into train and test set
# 90 % train, 10 % test
index <- sample(1:nrow(df.2p4ghz), size = 0.9*nrow(df.2p4ghz))
train <- df.2p4ghz[index,]
test <- df.2p4ghz[-index,] # update test set


## Plot airtime vs all parameters
library(reshape2)
library(ggplot2)
library(gridExtra)

split1 <- test[,c(1:9)]
split1$AirtimeCoeff <- test$AirtimeCoeff

split1.1 <- melt(split1, "AirtimeCoeff")
p1 <- ggplot(split1.1, aes(value, AirtimeCoeff)) + geom_point(size=0.1) + facet_wrap(~variable, scales="free")
grid.arrange(p1, ncol=1)

#### log-transform skewed variables
atc <- df.2p4ghz$AirtimeCoeff
df.2p4ghz.t <- df.2p4ghz # transformed data to be updated

# log transformation of "ChanimStatsAverage.tx"
a <- df.2p4ghz$ChanimStatsAverage.tx
head(table(a))
plot(a, atc, cex=0.1)
plot(log1p(a), atc, cex=0.1)

df.2p4ghz.t$ChanimStatsAverage.tx <- log1p(df.2p4ghz.t$ChanimStatsAverage.tx)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChanimStatsAverage.tx"] <- "lnCSA.tx"


# log transformation of ChaminStatsAverage.inbss
a <- df.2p4ghz.t$ChaminStatsAverage.inbss
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.inbss <- log1p(df.2p4ghz.t$ChaminStatsAverage.inbss)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.inbss"] <- "lnCSA.inbss"


# log transformation of ChaminStatsAverage.obss
a <- df.2p4ghz.t$ChaminStatsAverage.obss
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.obss <- log1p(df.2p4ghz.t$ChaminStatsAverage.obss)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.obss"] <- "lnCSA.obss"


# log transformation of ChaminStatsAverage.nocat
a <- df.2p4ghz.t$ChaminStatsAverage.nocat
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.nocat <- log1p(df.2p4ghz.t$ChaminStatsAverage.nocat)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.nocat"] <- "lnCSA.nocat"



split2 <- test[,c(10:11,13,16:21)]
split2$AirtimeCoeff <- test$AirtimeCoeff

split2.1 <- melt(split2, "AirtimeCoeff")
p2 <- ggplot(split2.1, aes(value, AirtimeCoeff)) + geom_point(size=0.1) + facet_wrap(~variable, scales="free")
grid.arrange(p2, ncol=1)


# log transformation of ChaminStatsAverage.nopkt
a <- df.2p4ghz.t$ChaminStatsAverage.nopkt
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.nopkt <- log(df.2p4ghz.t$ChaminStatsAverage.nopkt)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.nopkt"] <- "lnCSA.nopkt"


# log transformation of ChaminStatsAverage.txop
a <- df.2p4ghz.t$ChaminStatsAverage.txop
head(table(a))
plot(a, atc) # negatively skewed distribution
head(sort(a, decreasing = T))
plot(log(98+1-a), atc)

df.2p4ghz.t$ChaminStatsAverage.txop <- log(98+1-df.2p4ghz.t$ChaminStatsAverage.txop)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.txop"] <- "refl_lnCSA.txop"

# log transformation of ChaminStatsAverage.badtx
a <- df.2p4ghz.t$ChaminStatsAverage.badtx
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.badtx <- log1p(df.2p4ghz.t$ChaminStatsAverage.badtx)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.badtx"] <- "lnCSA.badtx"

# log transformation of ChaminStatsAverage.badplcp
a <- df.2p4ghz.t$ChaminStatsAverage.badplcp
head(table(a))
plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$ChaminStatsAverage.badplcp <- log1p(df.2p4ghz.t$ChaminStatsAverage.badplcp)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="ChaminStatsAverage.badplcp"] <- "lnCSA.badplcp"

# log transformation of TxBytes
a <- df.2p4ghz.t$TxBytes
head(table(a))
c <- signif(0.5*sort(unique(a))[2],4) # finds second-lowest value then divide by 2
c

plot(a, atc)
plot(log1p(a), atc)
plot(log(a+c), atc) # much better distribution

df.2p4ghz.t$TxBytes <- log(df.2p4ghz.t$TxBytes+c)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="TxBytes"] <- "lnTxBytes"


# log transformation of RxBytes
a <- df.2p4ghz.t$RxBytes
head(table(a))
c <- signif(0.5*sort(unique(a))[2],4) # finds second-lowest value then divide by 2
c

plot(a, atc)
plot(log1p(a), atc)
plot(log(a+c), atc) # much better distribution

df.2p4ghz.t$RxBytes <- log(df.2p4ghz.t$RxBytes+c)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="RxBytes"] <- "lnRxBytes"



# log transformation of RxRetries_SumAllDevices
a <- df.2p4ghz.t$RxRetries_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$RxRetries_SumAllDevices <- log1p(df.2p4ghz.t$RxRetries_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="RxRetries_SumAllDevices"] <- "lnRxRe_sad"


# log transformation of TxRetries_SumAllDevices
a <- df.2p4ghz.t$TxRetries_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$TxRetries_SumAllDevices <- log1p(df.2p4ghz.t$TxRetries_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="TxRetries_SumAllDevices"] <- "lnTxRe_sad"


## distribution of residuals of TxRetries_SumAllDevices
## before and after log-transformation
require(MASS)
par(mfrow=c(2,2))
a <- df.2p4ghz$TxRetries_SumAllDevices
model1 <- lm(AirtimeCoeff ~ a, data = df.2p4ghz)
esr <- studres(model1)
plot(esr ~ fitted(model1), cex=0.1, xlab="fitted(model_nt)")
hist(esr, breaks=30, main="Histogram of esr_nt")

model2 <- lm(AirtimeCoeff ~ log1p(a), data = df.2p4ghz)
esr <- studres(model2)
plot(esr ~ fitted(model2), cex=0.1, xlab="fitted(model_t)")
hist(esr, breaks=30, main="Histogram of esr_t")


split3 <- test[,c(22:29)]
split3$AirtimeCoeff <- test$AirtimeCoeff

split3.1 <- melt(split3, "AirtimeCoeff")
p3 <- ggplot(split3.1, aes(value, AirtimeCoeff)) + geom_point(size=0.1) + facet_wrap(~variable, scales="free")
grid.arrange(p3, ncol=1)



# log transformation of in_network_SumAllDevices
a <- df.2p4ghz.t$in_network_SumAllDevices
head(table(a))
plot(a, atc)
plot(log1p(a), atc) # not particularly good distribution

sort(a, decreasing = T)[1:30]

# cutoff
addmargins(table(a[a>=4e4]))
a[a>=4e4]=4e4
plot(a, atc)  # better distribution

df.2p4ghz.t$in_network_SumAllDevices <- a


# log transformation of tx_mcast_pkts_SumAllDevices
a <- df.2p4ghz.t$tx_mcast_pkts_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$tx_mcast_pkts_SumAllDevices <- log1p(df.2p4ghz.t$tx_mcast_pkts_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="tx_mcast_pkts_SumAllDevices"] <- "lnTxMcastpkts_sad"


# log transformation of rx_ucast_pkts_SumAllDevices
a <- df.2p4ghz.t$rx_ucast_pkts_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$rx_ucast_pkts_SumAllDevices <- log1p(df.2p4ghz.t$rx_ucast_pkts_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="rx_ucast_pkts_SumAllDevices"] <- "lnRxUcastpkts_sad"


# log transformation of rx_mcast_pkts_SumAllDevices
a <- df.2p4ghz.t$rx_mcast_pkts_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$rx_mcast_pkts_SumAllDevices <- log1p(df.2p4ghz.t$rx_mcast_pkts_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="rx_mcast_pkts_SumAllDevices"] <- "lnRxMcastpkts_sad"


# log transformation of rx_decrypt_failures_SumAllDevices
a <- df.2p4ghz.t$rx_decrypt_failures_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$rx_decrypt_failures_SumAllDevices <- log1p(df.2p4ghz.t$rx_decrypt_failures_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="rx_decrypt_failures_SumAllDevices"] <- "lnRxDecryptFail_sad"


# log transformation of tx_pkts_retry_exhausted_SumAllDevices
a <- df.2p4ghz.t$tx_pkts_retry_exhausted_SumAllDevices
head(table(a))

plot(a, atc)
plot(log1p(a), atc)

df.2p4ghz.t$tx_pkts_retry_exhausted_SumAllDevices <- log1p(df.2p4ghz.t$tx_pkts_retry_exhausted_SumAllDevices)
colnames(df.2p4ghz.t)[names(df.2p4ghz.t)=="tx_pkts_retry_exhausted_SumAllDevices"] <- "lnTxPktsReExhaust_sad"



split4 <- test[,c(12,15,30)]
split4$AirtimeCoeff <- test$AirtimeCoeff

split4.1 <- melt(split4, "AirtimeCoeff")
p4 <- ggplot(split4.1, aes(value, AirtimeCoeff)) + geom_point(size=0.1) + facet_wrap(~variable, scales="free")
grid.arrange(p4, ncol=1)


### devide transformed data set into train and test set
train.t <- df.2p4ghz.t[index,]
test.t <- df.2p4ghz.t[-index,]


###### Model comparison #######

#### NAIVE MODEL ####
best.guess <- mean(train$AirtimeCoeff) 

# Evaluate RMSE and MAE on the testing data
RMSE.naive <- sqrt(mean((best.guess-test$AirtimeCoeff)^2))
RMSE.naive
[1] 0.1792058

MAE.naive <- mean(abs(best.guess-test$AirtimeCoeff))
MAE.naive
[1] 0.1429198


#### LASSO ####

require(glmnet)
# construct design matrices using non-transformed data
model.mat.train = model.matrix(AirtimeCoeff~., data = train)
model.mat.test = model.matrix(AirtimeCoeff~., data = test)
grid = 10^seq(-3, 2, length=1000) # search area from 0.001 to 100..

# The glmnet function standardizes your data by default and then returns the coefficients on the original scale.
lasso.mod = glmnet(model.mat.train, train[, "AirtimeCoeff"], alpha = 1, lambda = grid, thresh = 1e-12)

dim(coef(lasso.mod))
# [1]   57 1000

# performing lasso regression with lambda = 0 is the least squares.
lasso.pred = predict(lasso.mod, newx = model.mat.test, s = 0)
mean((lasso.pred-test[, "AirtimeCoeff"])^2)
# [1] 0.03073095

sqrt(mean((lasso.pred-test[, "AirtimeCoeff"])^2))
# [1] 0.1753024

# fitting a lasso model on the training set, with lambda chosen by CV. 
set.seed(1)
lasso.fit = cv.glmnet(x = model.mat.train, y = train[, "AirtimeCoeff"], alpha = 1)
plot(lasso.fit)
lambda.best = lasso.fit$lambda.min
lambda.best #[1] 0.003737994

y.hat = predict(lasso.mod, newx = model.mat.test, s = lambda.best)
mse.lasso.test = mean((y.hat - test[, "AirtimeCoeff"])^2)
RMSE.lasso <- sqrt(mse.lasso.test)
RMSE.lasso
# [1] 0.1751647

MAE.lasso <- mean(abs(y.hat - test[, "AirtimeCoeff"]))
MAE.lasso
# [1] 0.1390245

lasso.coef = predict(lasso.mod, type="coefficients", s=lambda.best)[1:dim(coef(lasso.mod))[1],]
lasso.coef
lasso.coef[lasso.coef==0]

## Lasso on transformed data
# construct design matrices from train.t and test.t
model.mat.train.t = model.matrix(sqrt(AirtimeCoeff)~., data = train.t)
model.mat.test.t = model.matrix(sqrt(AirtimeCoeff)~., data = test.t)
grid = 10^seq(-3, 2, length=1000) # search area from 0.001 to 100..
lasso.mod.t = glmnet(model.mat.train.t, sqrt(train.t[, "AirtimeCoeff"]), alpha = 1, lambda = grid, thresh = 1e-12)

# fitting a lasso model on the training set, with lambda chosen by CV. 
set.seed(1)
lasso.fit.t = cv.glmnet(x = model.mat.train.t, y = sqrt(train.t[, "AirtimeCoeff"]), alpha = 1)
plot(lasso.fit.t)
lambda.best.t = lasso.fit.t$lambda.min
lambda.best.t #[1] 0.0008031284
# when lambda is bigger, coefs smaller and vice versa. sqrt(sum(coef(lasso.fit[-1,2500^2])))

y.hat.t = (predict(lasso.mod.t, newx = model.mat.test.t, s = lambda.best.t))^2
mse.lasso.test.t = mean((y.hat.t - test.t[, "AirtimeCoeff"])^2)
RMSE.lasso.t <- sqrt(mse.lasso.test.t)
RMSE.lasso.t
# [1] 0.1628181

MAE.lasso.t <- mean(abs(y.hat.t - test.t[, "AirtimeCoeff"]))
MAE.lasso.t
# [1] 0.1194672

lasso.coef.t = predict(lasso.mod.t, type="coefficients", s=lambda.best.t)[1:dim(coef(lasso.mod.t))[1],]
lasso.coef.t # compare this to the one from GAM!
lasso.coef.t[lasso.coef.t!=0]
lasso.coef.t[lasso.coef.t==0]


#### GAM ####

# function for checking the number of unique values in each IV.
check.k <- function(x, ...){
  
  for (i in 1:ncol(x)) {
    if ( (length(unique(x[,i])) < 15) & (class(x[,i]) != "factor") ) {
      print(c(colnames(x[i]), class(x[,i])))
      print(length(unique(x[,i]))) 
    } else {
      NULL
    }
  }
}

check.k(train)

# non-transformed data with gaussian
gam.nt=gam(AirtimeCoeff~s(ChanimStatsAverage.tx)
           +s(ClassificationCounters.CongestedNonWifi)
           +s(ChaminStatsAverage.inbss)
           +s(ClassificationCounters.CongestedWifi)
           +s(ClassificationCounters.PacketLoss, k=10)
           +s(ChaminStatsAverage.obss)+s(ChaminStatsAverage.nocat, k=10)
           +s(Samples)
           +s(activeSamples, k=10)+s(ChaminStatsAverage.nopkt, k=8)
           +s(ChaminStatsAverage.txop)
           +s(ChaminStatsAverage.badtx, k=8)
           +s(ChaminStatsAverage.badplcp, k=10)
           +s(ChaminStatsAverage.knoise, k=10)
           +s(TxBytes, k=10)
           +s(RxBytes, k=8)
           +s(RxRetries_SumAllDevices, k=10)+s(TxRetries_SumAllDevices, k=10)
           +s(in_network_SumAllDevices, k=10)+s(tx_mcast_pkts_SumAllDevices)
           +s(rx_ucast_pkts_SumAllDevices)+s(rx_mcast_pkts_SumAllDevices, k=8)
           +s(rx_decrypt_failures_SumAllDevices)
           +s(tx_pkts_retry_exhausted_SumAllDevices, k=8)
           +s(NumberOfCompetingAPsOnSameChannel)
           +s(NumberOfNonCompetingAPsOnOthrrChannels, k=10)
           +Interface+Channel+Hour, 
           data=train, family = gaussian(), select = TRUE)


par(mfrow=c(3,3))
plot(gam.nt, se=T, col="blue", scale=0)
summary(gam.nt)
gam.check(gam.nt, cex=0.1)


y.hat.test.gam.nt <- predict.gam(gam.nt, newdata = test)

RMSE.gam.nt <- sqrt(mean((y.hat.test.gam.nt-test$AirtimeCoeff)^2))
RMSE.gam.nt
# [1] 0.168398

MAE.gam.nt <- mean(abs(y.hat.test.gam.nt-test$AirtimeCoeff))
MAE.gam.nt
# [1] 0.1279063



check.k(train.t)
colnames(train.t)[names(train.t)=="ClassificationCounters.CongestedWifi"] <- "ClaCount.CngestdWifi"
colnames(train.t)[names(train.t)=="NumberOfNonCompetingAPsOnOthrrChannels"] <- "NumNonCompAPsOthrChnls"
colnames(train.t)[names(train.t)=="ClassificationCounters.CongestedNonWifi"] <- "ClaCount.CngestdNonWifi"
colnames(train.t)[names(train.t)=="NumberOfCompetingAPsOnSameChannel"] <- "NumCompAPsSameChnl"
colnames(train.t)[names(train.t)=="ClassificationCounters.PacketLoss"] <- "ClaCount.PktLoss"

colnames(test.t)[names(test.t)=="ClassificationCounters.CongestedWifi"] <- "ClaCount.CngestdWifi"
colnames(test.t)[names(test.t)=="NumberOfNonCompetingAPsOnOthrrChannels"] <- "NumNonCompAPsOthrChnls"
colnames(test.t)[names(test.t)=="ClassificationCounters.CongestedNonWifi"] <- "ClaCount.CngestdNonWifi"
colnames(test.t)[names(test.t)=="NumberOfCompetingAPsOnSameChannel"] <- "NumCompAPsSameChnl"
colnames(test.t)[names(test.t)=="ClassificationCounters.PacketLoss"] <- "ClaCount.PktLoss"


gam.t=gam(sqrt(AirtimeCoeff)~
            # gamt_1 low impact.. from lasso
            s(ClaCount.CngestdWifi)
          +s(ClaCount.CngestdNonWifi)
          +s(Samples, k=10)
          +s(activeSamples, k=15)
          +s(lnCSA.badtx, k=15)
          +s(lnRxMcastpkts_sad, k=10)
          # gamt_2 bigger impact
          +s(lnRxBytes, k=15)+s(lnTxBytes)+s(lnRxRe_sad)+s(lnTxRe_sad, k=15)
          +s(lnCSA.tx)+s(lnCSA.inbss, k=10)
          # gamt_3 related to interference
          +s(NumCompAPsSameChnl, k=10)
          +s(NumNonCompAPsOthrChnls, k=13)
          +s(ChaminStatsAverage.knoise, k=15)
          +s(refl_lnCSA.txop, k=10)
          +s(in_network_SumAllDevices, k=10)
          +s(lnCSA.obss, k=10)
          # 
          +s(lnCSA.nopkt, k=15)
          +s(ClaCount.PktLoss, k=15)
          +s(lnCSA.nocat, k=10)
          +s(lnCSA.badplcp, k=10)
          +s(lnTxMcastpkts_sad, k=10)
          +s(lnRxUcastpkts_sad, k=10)
          +s(lnRxDecryptFail_sad)
          +s(lnTxPktsReExhaust_sad, k=10)
          +Interface+Channel+Hour, data=train.t, family = gaussian(), select = TRUE)

par(mfrow=c(2,3))
plot.gam(gam.t, se=T, scale=0, col="blue")
summary(gam.t)
summary.aov(gam.t)
gam.check(gam.t, cex=0.1)

# RMSE and MAE
y.hat.test.gam.t <- (predict.gam(gam.t, newdata = test.t))^2

RMSE.gam.t <- sqrt(mean((y.hat.test.gam.t-test.t$AirtimeCoeff)^2))
RMSE.gam.t
[1] 0.1604702

MAE.gam.t <- mean(abs(y.hat.test.gam.t-test.t$AirtimeCoeff))
MAE.gam.t
[1] 0.1133721