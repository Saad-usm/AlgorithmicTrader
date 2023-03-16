library(quantmod)


start <- as.Date("2021-04-01")
end <- as.Date("2022-05-01")
#
AAPL <- getSymbols("^GSPC",auto.assign = FALSE, from = "2021-06-01")
chart_Series(AAPL)

#head(AAPL)
currentPrice = 0
cash = 1000
shares = 0
for (x in 2:272) {
  currentPrice = AAPL[[x]]
  previousPrice = AAPL[[x-1]]
  diff  = currentPrice - previousPrice
  scale = diff/10
  scale2 = scale
  scale3 = scale*100
  if ((cash > ((scale)*currentPrice)) && (diff > 0)) {
    shares = shares + scale
    cash = cash - ((scale)*currentPrice)
  }

  if ((diff < 0) && (shares > -scale2)) {
    shares = shares + scale2
    cash = cash - (scale2*currentPrice)
  }

  if ((x>20) && (AAPL[[x-20]] > AAPL[[x-10]]) && (AAPL[[x]] > AAPL[x-10])
      && (cash > ((scale3)*currentPrice)) && (diff > 0)) {
    shares = shares + scale3
    cash = cash - ((scale3)*currentPrice)
  }

  if ((x>20) && (AAPL[[x-20]] < AAPL[[x-10]]) && (AAPL[[x]] < AAPL[x-10])
      && (shares > -scale3) && (diff < 0)) {
    shares = shares + scale3
    cash = cash - ((scale3)*currentPrice)
  }

  print(cash+(shares*currentPrice))

}

print(cash+(shares*currentPrice))


install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

library(caret)

trainList1 <- c()
trainList2 <- c()
trainList3 <- c()
trainList4 <- c()
trainList5 <- c()

for (x in 1:88) {
  for (y in 1:5) {
    if (y == 1) {
      trainList1 <- c(trainList1, AAPL[[y+x]])
    } else if (y == 2) {
      trainList2 <- c(trainList2, AAPL[[y+x]])
    } else if (y == 3) {
      trainList3 <- c(trainList3, AAPL[[y+x]])
    } else if (y == 4) {
      trainList4 <- c(trainList4, AAPL[[y+x]])
    } else if (y == 5) {
      trainList5 <- c(trainList5, AAPL[[y+x]])
    }
  }
}

traininglistResult <- c()
for (z in 6:93) {
  traininglistResult <- c(traininglistResult, AAPL[[z]])
}

df1 <- data.frame(traininglistResult, trainList1, trainList2, trainList3, trainList4, trainList5)

testList1 <- c()
testList2 <- c()
testList3 <- c()
testList4 <- c()
testList5 <- c()

for (x in 150:237) {
  for (y in 1:5) {
    if (y == 1) {
      testList1 <- c(testList1, AAPL[[y+x]])
    } else if (y == 2) {
      testList2 <- c(testList2, AAPL[[y+x]])
    } else if (y == 3) {
      testList3 <- c(testList3, AAPL[[y+x]])
    } else if (y == 4) {
      testList4 <- c(testList4, AAPL[[y+x]])
    } else if (y == 5) {
      testList5 <- c(testList5, AAPL[[y+x]])
    }
  }
}

df2 <- data.frame(testList1, testList2, testList3, testList4, testList5)

testlistResult <- c()
for (z in 155:242) {
  print(AAPL[[z]])
  testlistResult <- c(testlistResult, AAPL[[z]])
}

print(df2)

print(nrow(df1))
print(length(traininglistResult))


model = train(traininglistResult ~ trainList1 + trainList2 + trainList3 + trainList4 + trainList5, df1, method = 'brnn')

nrow(df2)
nrow(df1)
predicted <- predict(model, df2)
print(predicted)
print(testlistResult)


library(ggplot2)


data <- read.csv("coin_Cardano.csv")
price <- data["Close"][1:400,]
plot(price)


currentPrice = 0
cash = 1000
shares = 0
for (x in 2:400) {
  currentPrice = price[x]
  previousPrice = price[x-1]
  diff  = currentPrice - previousPrice
  scale = diff*100
  scale2 = scale/10
  scale3 = scale*100
  if ((cash > ((scale)*currentPrice)) && (diff > 0)) {
    shares = shares + scale
    cash = cash - ((scale)*currentPrice)
  }

  if ((diff < 0) && (shares > -scale2)) {
    shares = shares + scale2
    cash = cash - (scale2*currentPrice)
  }

  if ((x>20) && (SHOP[[x-20]] > SHOP[[x-10]]) && (SHOP[[x]] > SHOP[x-10])
      && (cash > ((scale3)*currentPrice)) && (diff > 0)) {
    shares = shares + scale3
    cash = cash - ((scale3)*currentPrice)
  }

  if ((x>20) && (SHOP[[x-20]] < SHOP[[x-10]]) && (SHOP[[x]] < SHOP[x-10])
      && (shares > -scale3) && (diff < 0)) {
    shares = shares + scale3
    cash = cash - ((scale3)*currentPrice)
  }

  print(cash+(shares*currentPrice))

}

print(cash+(shares*currentPrice))

if (!require("coinmarketcapr")) {
  install.packages("coinmarketcapr")
}
library(coinmarketcapr)
install.packages("crypto")

key <- "64fb762b-0917-48d8-8099-750db1a828e6"

coinmarketcapr::setup(key)

get_crypto_ohlcv("EUR", latest = F, time_period = "hourly",
                 time_start=Sys.Date()-180, count=5, interval="monthly")


