library(arulesSequences)
library(caTools)
library(data.table)


#Read the data
MyData <- total

#Need to tag all the session clicks as 1 even if one of the clicks is 1 in that session

buy_sessions = MyData$SessionID[MyData$buy==1] 
for(i in buy_sessions){
  MyData$buy[MyData$SessionID == i] = 1
}

#Breaking the dataset into buy and no buy
buy = MyData[MyData$buy == 1,]
no_buy = MyData[MyData$buy == 0,]

#converting it into data table
require(data.table)
setDT(buy)

#create the sequence according to the Session ID
buy[, Seq:=seq(.N), by=list(cumsum(c(0, abs(diff(SessionID)))))]

#Delete the columns not required further
buy$Timestamp =NULL
buy$buy = NULL
buy$X = NULL

#convert data table to data frame
setDF(buy)
buy[, c(1,3,2)]
buy1 <- buy[, c(1,3,2)]
buy1

#convert the data frame to text file
write.table(buy1,"buy.txt",sep="\t",row.names=FALSE,col.names = FALSE)


#####Reading text file for sequencing#########

data = read_baskets(con = "buy.txt", info = c("sequenceID","eventID"))
as(head(data), "data.frame")

#creating sequences
seqs <- cspade(data, parameter = list(support = 0.0001), control = list(verbose = TRUE))
as(seqs,"data.frame")

#creating rules
rule <- ruleInduction( seqs, confidence = 0.001, control = list( verbose = TRUE ))
rules_seq_buy <- as( rule, "data.frame" )

#PLotting rules
library(arulesViz)
plot(rules_seq_buy)
plot(rules_seq_buy, method="graph")
plot(rules_seq_buy, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1)
plot(rules_seq_buy, method="matrix")
plot(rules_seq_buy, method="paracoord", control=list(reorder=TRUE))

#Arranging rules according to Support, Confidence and Lift
support <- rules_seq_buy[order(rules_seq_buy$support),][10:1,]
confidence <- rules_seq_buy[order(rules_seq_buy$confidence),][10:1,]
lift <- rules_seq_buy[order(rules_seq_buy$lift),][10:1,]

row.names(support) <- NULL
row.names(confidence) <- NULL
row.names(lift) <- NULL

##### Splitting buy data into 80-20 test-train ratio #############
buy1_train = buy1[1:floor(0.8*length(buy1$`SessionID`)),]
buy1_test = buy1[floor(0.8*length(buy1$`SessionID`)): length(buy1$`SessionID`),]


###################### TESTING ON BUY DATA TO BE DONE #################################################
###################### BUY DATA #################################################

#execute rules against test data for buy
ruleDF_buy = as(rule,"data.frame")
buy1_test$preds = apply(buy1_test,1,function(X) makepreds(X["ItemID"], ruleDF_buy))


# extract unique predictions for each test user for buy
userpreds = as.data.frame(aggregate(preds ~ SessionID, data = buy1_test, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(ItemID ~ SessionID, data = buy1_test, paste, collapse=","))
baskets$Item_ID = apply(baskets,1,function(X) uniqueitems(X["ItemID"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds_buy = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["SessionID"])))

#count total number of unique predictions made
totalpreds_buy = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision_buy = correctpreds_buy*100/totalpreds_buy

cat("Precission=", precision_buy, "Correct Prediction =",correctpreds_buy,"Total Predictions=",totalpreds_buy)
#Precission=       Correct Prediction =      Total Predictions=    
####################### BUY DATA END ###############################################


#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, ruleDF) {
  antecedent = paste("<{",item,"}> =>",sep="") 
  firingrules = ruleDF[grep(antecedent, ruleDF$rule,fixed=TRUE),1]
  gsub(" "," ",toString(sub("\\} ","",sub(".*=> \\{","",firingrules))))
}

# count how many predictions are in the basket of items already seen by that user 
# Caution : refers to "baskets" as a global
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$SessionID == baskID,"ItemID"][[1]]
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

totalpreds_buy = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

countpreds(userpreds["preds"][[1]])



