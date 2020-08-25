
# loading the libraries 

library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

#loading data

transactionData <- read.csv("QVI_transaction_data.csv", header= TRUE)
customerData <- read.csv("QVI_purchase_behaviour.csv",header = TRUE)


head(transactionData)
head(customerData)

#Exploring and cleaning transaction data

# converting the date into the right format 

transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

head(transactionData)


# summarizing the product name column in transaction data
summary(transactionData$PROD_NAME)

#created a set of words 
# this is going to help us remove products which are not chips, as analysis is focused only on chip products 
productWords <- data.table(unlist(strsplit(unique(transactionData[,"PROD_NAME"])," ")))

setnames(productWords,'words')

# removing special characters from our set of words

#removed punctuation 
productWords[,SPECIAL := grepl("[[:punct:]]",words)]
productWords <- productWords[SPECIAL == FALSE,][,SPECIAL := NULL]

#removed digits 
productWords[,SPECIAL := grepl("[[:digit:]]",words)]
productWords <- productWords[SPECIAL == FALSE,][,SPECIAL := NULL]

# changing empty string to NA
productWords[words == ""] <- NA

#removing all empty cells
productWords <- productWords[complete.cases(productWords),]


# creating a frequency table for out set of words, sorted.
productWords <- data.frame(sort(table(productWords),decreasing = TRUE))


#Noticed some salsa product, these have to be removed

transactionData<- data.table(transactionData)

transactionData[,SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][,SALSA := NULL]

#checking for any potential outliers and nulls
summary(transactionData)

sum(is.na(transactionData))

# no nulls recorded but potential outlier in product quantity as max is 200 
# whereas mean is 1.908


# 2 outlier orders from the same customer
outlier <- transactionData[PROD_QTY == 200,]

#checking the other transaction the outlier customer made

outlierTransactions <- transactionData[LYLTY_CARD_NBR == 226000,]
#customer has only made 2 orders

# created frequency table with the number of transaction each day 
numberOfTransactionsByDate <- data.frame(sort(table(transactionData$DATE),decreasing = TRUE ))

setnames(numberOfTransactionsByDate,c('date','freq'))


numberOfTransactionsByDate<-numberOfTransactionsByDate[order(as.Date(numberOfTransactionsByDate$date)),]


 # noticed there are only 364 observations meant to be 365 so we are going to 
# correct this 
  
#created a seqeunce of dates between the dates the transactions have been recorded from
seqOfDates <- data.table(seq(as.Date('2018-07-01'),as.Date('2019-06-30'),by = 1))

setnames(seqOfDates,"date")


# now joining our transaction frequency table to our sequence of dates table
# to find the missing data and fill it in 

seqOfDates$date <- as.factor(seqOfDates$date)
class(seqOfDates$date)
class(numberOfTransactionsByDate$date)



completeTransactionFreq <- merge (x = seqOfDates, y = numberOfTransactionsByDate, by="date", all.x = TRUE)

# replacing NA values with 0 

completeTransactionFreq[is.na(completeTransactionFreq)] <- 0
completeTransactionFreq$date <- as.Date(completeTransactionFreq$date)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust=0.5))

ggplot(completeTransactionFreq,aes(x=date,y= freq)) +
  geom_line() + 
  labs(x = "Day", y ="Number of transactions",title="Transactions over time")+
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#subset of transaction data focusing on december 
december <- completeTransactionFreq[completeTransactionFreq$date >= as.Date("2018-12-01") & completeTransactionFreq$date <= as.Date("2018-12-31"),]

# plotting transactions over december 
ggplot(december,aes(x=date,y= freq)) +
  geom_line() + 
  labs(x = "Day", y ="Number of transactions",title="Transactions over time (December)")+
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#creating other features for transactionData: brand of chips and pack_size
transactionData[,PACK_SIZE := parse_number(PROD_NAME)]


#HISTOGRAM of pack sizes



ggplot(transactionData,aes(x=PACK_SIZE) )+
  geom_histogram(binwidth = 10,color="black",fill="lightblue") +scale_x_discrete() +
  labs(x = "Pack Sizes", y ="Frequency",title="Histogram of Pack Sizes")+scale_color_brewer(palette="Dark2")+geom_density(alpha=.2, fill="#FF6666") 

# mean and standard deviation of pack sizes 
mean(transactionData$PACK_SIZE)
sd(transactionData$PACK_SIZE)

#creating brand name feature from prod name

transactionData$BRAND_NAME <- sub('(^\\w+)\\s.+','\\1',transactionData$PROD_NAME)


# cleaning brand names with similar names

transactionData[tolower(BRAND_NAME) == "red", BRAND_NAME := "RRD"]

transactionData[tolower(BRAND_NAME) == "grain", BRAND_NAME := "GrnWves"]

transactionData[tolower(BRAND_NAME) == "infzns", BRAND_NAME := "Infuzions"]

transactionData[tolower(BRAND_NAME) == "ww", BRAND_NAME := "Woolworths"]

transactionData[tolower(BRAND_NAME) == "snbts", BRAND_NAME := "Sunbites"]

# bar chart of number of chips sold for different brand names 
brands <- data.frame(sort(table(transactionData$BRAND_NAME),decreasing = TRUE ))

setnames(brands,c("BRAND","freq"))

ggplot(brands,aes(x=BRAND,y= freq,fill=BRAND)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Brands", y ="Frequency",title="Distribution Of Brand Purchases")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



# exploring and cleaning customer data


summary(customerData)

#checking for any nulls

sum(is.na(customerData))

# no nulls in data 

# frequency table for the different lifestages of customers
lifestageCategory <- data.frame(sort(table(customerData$LIFESTAGE),decreasing = TRUE ))

setnames(lifestageCategory,c("lifestage","freq"))

ggplot(lifestageCategory,aes(x=lifestage,y= freq,fill=lifestage)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "lifestage", y ="frequency",title="Distribution Of Customers Over Lifestages")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_fill_brewer(palette="Dark2")



premiumCustomerType <- data.frame(sort(table(customerData$PREMIUM_CUSTOMER),decreasing = TRUE ))

setnames(premiumCustomerType,c("premium_customer_type","freq"))

ggplot(premiumCustomerType,aes(x=premium_customer_type,y= freq,fill=premium_customer_type)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "lifestage", y ="frequency",title="Distribution Of Customers Over Premium Types")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_fill_brewer(palette="Dark2")


#merging our two datasets using a left join 

data <- merge(transactionData,customerData, all.x = TRUE)


#checking for missing data within merged data


sum(is.na(data))

#no missing data found in our merged dataset


#generating new insights

#total sales by life stages 

totalSalesByLifestage <- aggregate(data$TOT_SALES, by=list(LIFESTAGE=data$LIFESTAGE),FUN=sum)

setnames(totalSalesByLifestage,c("Lifestage","Total_Sales"))

totalSalesByLifestage<-totalSalesByLifestage[order(totalSalesByLifestage$Total_Sales,decreasing = FALSE),]

ggplot(totalSalesByLifestage,aes(x=reorder(Lifestage,-Total_Sales),y= Total_Sales,fill=Lifestage)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "lifestage", y ="Total Sales",title="Total Sales By Lifestage")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_fill_brewer(palette="Dark2")


# total sales by customer premium 

totalSalesByPremium <- aggregate(data$TOT_SALES, by=list(LIFESTAGE=data$PREMIUM_CUSTOMER),FUN=sum)

setnames(totalSalesByPremium,c("Premium_Customer","Total_Sales"))

totalSalesByPremium<-totalSalesByPremium[order(totalSalesByPremium$Total_Sales,decreasing = FALSE),]

ggplot(totalSalesByPremium,aes(x=reorder(Premium_Customer,-Total_Sales),y= Total_Sales,fill=Premium_Customer)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Premium Customer", y ="Total Sales",title="Total Sales By Premium Customer")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_fill_brewer(palette="Dark2")

#total sales by customer premium and lifestage
totalSalesByPremiumAndLifestage <- aggregate(.~LIFESTAGE+PREMIUM_CUSTOMER, data = data[,c("LIFESTAGE","PREMIUM_CUSTOMER","TOT_SALES")] , sum)


totalSalesByPremiumAndLifestage$Lifestage_Premium <- paste(totalSalesByPremiumAndLifestage$LIFESTAGE,totalSalesByPremiumAndLifestage$PREMIUM_CUSTOMER)
totalSalesByPremiumAndLifestage <- totalSalesByPremiumAndLifestage[,c("Lifestage_Premium","TOT_SALES")]

ggplot(totalSalesByPremiumAndLifestage,aes(x=reorder(Lifestage_Premium,-TOT_SALES),y= TOT_SALES,fill=Lifestage_Premium)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Lifestage and Premium", y ="Total Sales",title="Total Sales By Lifestage By Premium")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Calculating the number of customers by Life-stage and Premium

numberOfCustomersByLifestageByPremium <- data.frame(paste(customerData$LIFESTAGE,customerData$PREMIUM_CUSTOMER))


numberOfCustomersByLifestageByPremium <- data.frame(sort(table(numberOfCustomersByLifestageByPremium),decreasing = TRUE ))

setnames(numberOfCustomersByLifestageByPremium,c("Lifestage_Premium","freq"))



ggplot(numberOfCustomersByLifestageByPremium,aes(x=Lifestage_Premium,y = freq,fill=Lifestage_Premium)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Lifestage and Premium", y ="Number of Customers",title="Number of Customers By Lifestage By Premium")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Calculating The Average Number of Units Bought by Lifestage and Premium

averageNumberOfUnits <- data.table(data[,c("LIFESTAGE","PREMIUM_CUSTOMER","PROD_QTY")])

averageNumberOfUnits$Lifestage_Premium <-  data.table(paste(data$LIFESTAGE,data$PREMIUM_CUSTOMER))

setnames(averageNumberOfUnits,c("Lifestage","premium","prod_qty","Lifestage_Premium"))


averageNumberOfUnits<- averageNumberOfUnits[,c("Lifestage_Premium","prod_qty")]


setnames(averageNumberOfUnits,c("Lifestage_Premium","PROD_QTY"))

averageNumberOfUnits <- aggregate(.~Lifestage_Premium, data = averageNumberOfUnits[,c("Lifestage_Premium","PROD_QTY")] , mean)

ggplot(averageNumberOfUnits,aes(x=reorder(Lifestage_Premium,-PROD_QTY),y= PROD_QTY,fill=Lifestage_Premium)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Lifestage and Premium", y ="Average Units Bought",title="Average Units Per Customer Segment ")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Calcuating average price per unit by Lifestage and Premium segments 

averagePrice <- data.table(data[,c("LIFESTAGE","PREMIUM_CUSTOMER","PROD_QTY","TOT_SALES")])

averagePrice$Lifestage_Premium <-  data.table(paste(data$LIFESTAGE,data$PREMIUM_CUSTOMER))

setnames(averagePrice,c("Lifestage","premium","prod_qty","TOT_SALES","Lifestage_Premium"))


averagePrice<- averagePrice[,c("Lifestage_Premium","prod_qty","TOT_SALES")]




averagePrice <- aggregate(.~Lifestage_Premium, data = averagePrice , FUN= sum )

averagePrice$averagePricePerUnit <- averagePrice$TOT_SALES / averagePrice$prod_qty


ggplot(averagePrice,aes(x=reorder(Lifestage_Premium,-averagePricePerUnit),y= averagePricePerUnit,fill=Lifestage_Premium)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Lifestage and Premium", y ="Average Price Per Unit Bought",title="Average Price Per Unit Per Customer Segment ")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# so we have determined on average the customer segment mainstream - young single/couples spend more on average per chip product
# so I am going to focus on this segment for further insights.


#Find out which brand is purchased the most by this segment  

mainstreamYoungSingleCouples <- data.table(data)

mainstreamYoungSingleCouples$Lifestage_Premium <-  data.table(paste(data$LIFESTAGE,data$PREMIUM_CUSTOMER))

mainstreamYoungSingleCouples <- mainstreamYoungSingleCouples[Lifestage_Premium =='YOUNG SINGLES/COUPLES Mainstream']


mainstreamYoungSingleCouplesBrandFreq <- data.frame(sort(table(mainstreamYoungSingleCouples$BRAND_NAME),decreasing = TRUE ))

setnames(mainstreamYoungSingleCouplesBrandFreq,c('BRAND_NAME','freq'))

ggplot(mainstreamYoungSingleCouplesBrandFreq,aes(x=BRAND_NAME ,y=freq ,fill=BRAND_NAME)) +
  geom_bar(stat="identity",width = 0.5) + 
  labs(x = "Brands", y ="Count",title="Mainstream - Young Single/Couples Brand Purchases")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Now producing an histogram of PACK_SIZE to determine whether this segment buys larger chips. 

is.na(mainstreamYoungSingleCouples)

ggplot(mainstreamYoungSingleCouples,aes(x=PACK_SIZE) )+
  geom_histogram(binwidth = 10,color="black",fill="lightblue") +
  labs(x = "Pack Sizes", y ="Frequency",title="Histogram of Pack Sizes For Young Single/Couples- Mainstream")+scale_color_brewer(palette="Dark2")+geom_density(alpha=.2, fill="#FF6666")+
  scale_x_continuous(breaks = seq(0, 400, 10), limits = c(0,400))

# calculating mean and sd for pack size for this segment
mean(mainstreamYoungSingleCouples$PACK_SIZE)
sd(mainstreamYoungSingleCouples$PACK_SIZE)

