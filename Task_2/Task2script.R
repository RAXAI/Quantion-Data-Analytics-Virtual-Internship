#loading required libraries

library(data.table)
library(ggplot2)
library(tidyr)
library(lsa)
data <-data.table(read.csv("QVI_data.csv"))

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# create a new column yearmonth in the format yyyymm from the date column

data$DATE <- as.Date(data$DATE)
data$YEARMONTH <-  format(data$DATE, "%Y%m")


# number of customer by store and yearMonth
numberOfCustomers <- aggregate(.~STORE_NBR+YEARMONTH, data = data[,c("LYLTY_CARD_NBR","STORE_NBR","YEARMONTH")],uniqueN)
setnames(numberOfCustomers,c("STORE_NBR","YEARMONTH","NBR_CUSTOMERS"))


# number of transaction per customer by store and yearMonth
numberOfTransactions <- aggregate(.~STORE_NBR+YEARMONTH, data = data[,c("STORE_NBR","TXN_ID","YEARMONTH")],uniqueN)
setnames(numberOfTransactions,c("STORE_NBR","YEARMONTH","NBR_TRANSACTIONS"))
numberOfTransactionsPerCustomer <- numberOfTransactions
numberOfTransactionsPerCustomer$NBR_TRANSACTIONS<- numberOfTransactions$NBR_TRANSACTIONS/numberOfCustomers$NBR_CUSTOMERS





#number of chips per transaction by store and yearMonth

numberOfChips <- aggregate(.~STORE_NBR+YEARMONTH, data = data[,c("STORE_NBR","PROD_QTY","YEARMONTH")],sum)
setnames(numberOfChips,c("STORE_NBR","YEARMONTH","NBR_Chips"))
numberOfChips$NBR_Chips <- numberOfChips$NBR_Chips/numberOfTransactions$NBR_TRANSACTIONS

#number units sold by store and yearMonth

numberOfUnits <- aggregate(.~STORE_NBR+YEARMONTH, data = data[,c("PROD_QTY","STORE_NBR","YEARMONTH")],sum)
setnames(numberOfUnits,c("STORE_NBR","YEARMONTH","NBR_Units"))

#total_sales  by store and yearMonth 

totalSales <- aggregate(.~STORE_NBR+YEARMONTH, data = data[,c("TOT_SALES","STORE_NBR","YEARMONTH")],sum)
setnames(totalSales,c("STORE_NBR","YEARMONTH","TOTAL_SALES"))

#avgPricePerUnit by store and yearMonth

avgPricePerUnit <- totalSales
setnames(avgPricePerUnit,c("STORE_NBR","YEARMONTH","AVG_PRICE"))
avgPricePerUnit$AVG_PRICE <- avgPricePerUnit$AVG_PRICE/numberOfUnits$NBR_Units

# create dataframe of our measures overtime 
setnames(totalSales,c("STORE_NBR","YEARMONTH","TOTAL_SALES"))
measureOverTime <- totalSales

measureOverTime$nCustomers <- numberOfCustomers$NBR_CUSTOMERS
measureOverTime$nTxnPerCust <- numberOfTransactionsPerCustomer$NBR_TRANSACTIONS
measureOverTime$nChipsPerTxn <- numberOfChips$NBR_Chips
measureOverTime$avgPricePerUnit <- avgPricePerUnit$AVG_PRICE

measureOverTime <- data.table(measureOverTime)


#filtering out the data to the pretrial period and only including stores with ful' observation periods 


storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in%
                                      storesWithFullObs,]
  

# function which caluclates the covarience between two stores for a specific metric
calculateCorrelation <- function(inputTable,metricCol,storeComparison){

  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
    
  for (i in storeNumbers) {
    
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2"= i,
                                   "corr_measure"=cov(inputTable[STORE_NBR==i,eval(metricCol)],inputTable[STORE_NBR==storeComparison,eval(metricCol)])
                                   )
    
    calcCorrTable <- rbind(calcCorrTable,calculatedMeasure)
  }
  return(calcCorrTable)
  
  
}

#function which calculates the standardized magnitude distance between two stores for a specific metric.

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison){
  calcDistTable = data.table(Store1 = numeric(),Store2 = numeric(),YEARMONTH = numeric(),measure=numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  
  for (i in storeNumbers) {
    
    
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison,eval(metricCol)]-inputTable[STORE_NBR == i,eval(metricCol)]))
    
    calcDistTable <- rbind(calcDistTable,calculatedMeasure)
    

  }
  
  #standardize the distance 
  
  minMaxDist <- calcDistTable[,.(minDist=min(measure),maxDist = max(measure)),by = c("Store1","YEARMONTH")]
  distTable <- merge(calcDistTable,minMaxDist, by = c("Store1","YEARMONTH"))
  distTable[,magnitudeMeasure:= 1-(measure - minDist)/(maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                                .(Store1, Store2)]
  return(finalDistTable)
  
}

#calculate correlations for trial store
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures,quote(TOTAL_SALES),trial_store)
corr_nCustomers<- calculateCorrelation(preTrialMeasures,quote(nCustomers),trial_store)

# calculate magnitude for selected trial store

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(TOTAL_SALES),
                                               trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,
                                                   quote(nCustomers), trial_store)

# weighted averages for nsales and ncustomers between correlation and distance then merging the two tables togther


corr_weight <-0.5
score_nsales <- merge(corr_nSales,magnitude_nSales,by=c("Store1","Store2"))[,scoreNSales := 0.5*corr_nSales$corr_measure + 0.5*magnitude_nSales$mag_measure ]
score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers,by=c("Store1","Store2"))[,scoreNCust := 0.5*corr_nCustomers$corr_measure + 0.5*magnitude_nCustomers$mag_measure ]




#calculated the final score using the weighted average between two scores calculated between the total sales and number of customers 
score_Control<-merge(score_nsales,score_nCustomers)
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]




#determining the store with the highest score mean it is most similar to the trial store for the drivers we selected
#control_store <- score_Control[score_Control$Store2!=trial_store,]
control_store <- score_Control

control_store <- control_store[which.max(score_Control$finalControlScore),Store2]





#visually representing drivers (TOTAL_SALES and nCustomers) for trial store and potential control store during the period before the trial

# aggregating the average total sales each month during the period before the trial

measureOverTime$YEARMONTH <- as.numeric(measureOverTime$YEARMONTH)
str(measureOverTime)
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(TOTAL_SALES), by = c("YEARMONTH",
                                       "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]


ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

# aggregating the average number of customer each month during the period before the trial


pastnCustomers <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,"Trial",ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(nCustomers), by = c("YEARMONTH","Store_type")][
  , TransactionMonth := as.Date(paste(YEARMONTH %/%100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]


ggplot(pastnCustomers, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of Customers", title = " Number of Customers by month")




#now focusing on the trial period, to extract insights  whether there was an uplift in overall chip sales. 

# Scaling the control stores sales to a similar level to the trial store

scalingFactorControlSales<- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,sum(TOTAL_SALES)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902,sum(TOTAL_SALES)]

#apply scaling factor to the control store 

measureOverTimeSales <-measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store,][,
controlSales := TOTAL_SALES * scalingFactorControlSales]

# calculate the percentage difference between calculate the difference between the scaled control sales and trial store sales
trialSales <-  measureOverTime[STORE_NBR == trial_store,]

percentageDiff <- merge(trialSales,scaledControlSales,by = YEARMONTH)[,percentageDiff:= abs()]
