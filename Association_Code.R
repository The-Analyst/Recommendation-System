library(arules)
#trans <- as(split(df[,"BRAND"], df[,"EMAIL"]), "transactions")


data_click = read.csv("yoochoose-clicks1.csv")
data_buy = read.csv("yoochoose-buys1.csv")
data_test = read.csv("yoochoose-test.csv")

colnames(data_buy) = c("Session ID", "Timestamp","Item ID","Price","Quantity")
colnames(data_click) = c("Session ID", "Timestamp","Item ID","Category")
colnames(data_test) = c("Session ID", "Timestamp","Item ID","Category")

#data_buy$Timestamp = as.Date(data_buy$Timestamp)


#Finding Unique sessions in Clicks, buys and test

length(unique(data_buy$`Session ID`)) #509696
length(unique(data_click$`Session ID`)) #9249729
length(unique(data_test$`Session ID`)) #2312432

#To find the number of uique items with categories
length(unique(data_click$`Item ID`)) #52739
length(unique(data_buy$`Item ID`))  #19949

# making another data frame for the item info
item_info = data_buy[,c(3,4,5)]

# For sequence classification we need to give class labels for the training data
data_buy$buy = 1
data_click$buy = ifelse(data_buy$`Session ID` == data_click$`Session ID`,1,0) #need to correct it while doing sql computations
data_buy$buy = as.factor(data_buy$buy)
data_click$buy = as.factor(data_click$buy)

data_click$Timestamp =as.Date(as.character(data_click$Timestamp))

#Combining the datastes
#install.packages("plyr")
#library(plyr)
#data_total = rbind.fill(data_buy,data_click)

#Sampling the data for the months of April only [data avaibale for- April,May,June.July,August,September]
buy = subset(data_buy, data_buy$Timestamp < as.Date("2014-05-01")) 
click = subset(data_click, data_click$Timestamp < as.Date("2014-05-01"))
#click$buy = NULL

total = rbind.fill(buy,click)

#Sorting the data so that all sessions with same id come together
total = arrange(total,total$`Session ID`)
temp = total

#Removing the price and quantity as it is not required for developing sequences
total$Quantity=NULL
total$Price = NULL
total$Category = NULL
total$Timestamp=NULL
total$buy=as.integer(total$buy)

head(total,10)
total[is.na(total)] = 0

#Need to tag all the session clicks as 1 even if one of the clicks is 1 in that session
buy_sessions = total$`Session ID`[total$buy==1] #list of all buy sessions
for(i in buy_sessions){
  total$buy[total$`Session ID` == i] = 1
}

#The dataframe can be in either a normalized (single) form or a flat file (basket) form.
#When the file is in basket form it means that each record represents a transaction where the items in the basket are represented by columns.
#When the dataset is in single form it means that each record represents one single item and each item contains a transaction id


#Trying to build a transaction data frame
trans <- as(split(total[,"Item ID"], total[,"Session ID"]), "transactions")

#Breaking the dataset into buy and no buy
buy = total[total$buy == 1,]
buy$buy = NULL
no_buy = total[total$buy == 0,]
no_buy$buy = NULL

buy_train = buy[1:floor(0.8*length(buy$`Session ID`)),]
buy_test = buy [floor(0.8*length(buy$`Session ID`)): length(buy$`Session ID`),]
colnames(buy_test) = c("Session_ID","Item_ID")
colnames(buy_train) = c("Session_ID","Item_ID")

nobuy_train = no_buy[1:floor(0.8*length(no_buy$`Session ID`)),]
nobuy_test = no_buy [floor(0.8*length(no_buy$`Session ID`)): length(no_buy$`Session ID`),]
colnames(nobuy_test) = c("Session_ID","Item_ID")
colnames(nobuy_train) = c("Session_ID","Item_ID")

# COnverting data frame into transactions for buy and no buy
trans_buy = as(split(buy_train[,"Item_ID"], buy_train[,"Session_ID"]), "transactions")
trans_nobuy = as(split(nobuy_train[,"Item_ID"], nobuy_train[,"Session_ID"]), "transactions")


#Forming association rules for the buy and no buy transactions
rules_buy <- apriori(trans_buy, parameter = list(supp=0.001, conf=0.5, minlen=2))
rules_nobuy <- apriori(trans_nobuy, parameter = list(supp=0.0001, conf=0.6, minlen=2))

###################### BUY DATA #################################################

#execute rules against test data for buy
rulesDF_buy = as(rules_buy,"data.frame")
buy_test$preds = apply(buy_test,1,function(X) makepreds(X["Item_ID"], rulesDF_buy))

# extract unique predictions for each test user for buy
userpreds = as.data.frame(aggregate(preds ~ Session_ID, data = buy_test, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(Item_ID ~ Session_ID, data = buy_test, paste, collapse=","))
baskets$Item_ID = apply(baskets,1,function(X) uniqueitems(X["Item_ID"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds_buy = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["Session_ID"])))

#count total number of unique predictions made
totalpreds_buy = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision_buy = correctpreds_buy*100/totalpreds_buy

cat("Precission=", precision_buy, "Correct Prediction =",correctpreds_buy,"Total Predictions=",totalpreds_buy)
#Precission= 72.58065 Correct Prediction = 7965 Total Predictions= 10974
####################### BUY DATA END ###############################################


############### NOBUY DATA ###############################################

#execute rules against test data for nobuy
rulesDF_nobuy = as(rules_nobuy,"data.frame")
nobuy_test$preds = apply(nobuy_test,1,function(X) makepreds(X["Item_ID"], rulesDF_nobuy))

# extract unique predictions for each test user for buy
userpreds = as.data.frame(aggregate(preds ~ Session_ID, data = nohbuy_test, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(Item_ID ~ Session_ID, data = buy_test, paste, collapse=","))
baskets$Item_ID = apply(baskets,1,function(X) uniqueitems(X["Item_ID"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds_buy = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["Session_ID"])))

#count total number of unique predictions made
totalpreds_buy = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision_buy = correctpreds_buy*100/totalpreds_buy

cat("Precission=", precision_buy, "Correct Prediction =",correctpreds_buy,"Total Predictions=",totalpreds_buy)


############### NOBUY DATA END #########################################


#######################################################################
# the supporting functions
#######################################################################

#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  antecedent = paste("{",item,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

# count how many predictions are in the basket of items already seen by that user 
# Caution : refers to "baskets" as a global
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$Session_ID == baskID,"Item_ID"][[1]]
  cnt = 0 
  for (p in plist) {
    if (p %in% blist) cnt = cnt+1
  }
  cnt
}

# count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len > 0 && (predlist[[1]] == "")) 0 # avoid counting an empty list
  else len
}


################ Vizualise the rules ##########################
library(arulesViz)
plot(rules_buy)
plot(rules_buy, method="graph")
plot(rules_buy, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1)
plot(rules_buy, method="matrix")
plot(rules_buy, method="paracoord", control=list(reorder=TRUE))


