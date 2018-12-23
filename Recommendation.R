buy<-read.csv("C:/Users/issuser/Desktop/zhangxiaoman/yoochoose-buys.csv")
colnames(buy) = c("Session ID", "Timestamp","Item ID","Price","Quantity")
buy$Timestamp <- ymd_hms(buy$Timestamp)
enddate = as.POSIXct("2014-05-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="UTC")
buy1<-buy[which(buy$Timestamp < enddate),]

buy1<-buy1[with(buy1,Quantity>0&Price>0),]
summary(buy1$Price)

df<-function(x){
  if(x<500)
  {result<-1}
  else if(x>=500 & x<=1000)
  {result<-2}
  else if(x>1000 & x<=1500)
  {result<-3}
  else if(x>1500 & x<=2000)
  {result<-4}
  else if(x>2000 & x<=2500)
  {result<-5}
  else if(x>2500 & x<=3000)
  {result<-6}
  else if(x>3000 & x<=3500)
  {result<-7}
  else if(x>3500 & x<=4000)
  {result<-8}
  else if(x>4000)
  {result<-9}
  return(result)
}

buy1$level<-level<-sapply(buy1$Price, df)
buy2<-buy1[,c(1,6)]

############################## CONVERT Matrix ############################
data=buy2
colnames(data)=c("Session_ID","Item_ID")
data2 <- data %>% group_by(Session_ID) %>% summarise(val=paste(Item_ID, collapse=" "))

data3<-data2[c(1:10000),]
df=dfm(data3$val)

test=convert(df, to = "data.frame")
test1<-provideDimnames(test, sep='',list(
  user=paste('u', 1:10000),
  item=paste('i', 1:10)))

test1<as.data.frame(test1)
write.csv(test2,"C:/Users/issuser/Desktop/zhangxiaoman/level.csv")

###################### Recommendation ############################

library("recommenderlab")	 	 
# Loading to pre-computed affinity data	 
affinity.data<-read.csv("C:/Users/issuser/Desktop/zhangxiaoman/level.csv")
affinity.data<-affinity.data[,-1]
rownames(affinity.data)=affinity.data$document
affinity.data<-affinity.data[,-1]
affinity.matrix<- as(affinity.data, "realRatingMatrix")

# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
Rec.model<-Recommender(affinity.matrix, method = "UBCF")

#crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))

# recommended top 5 items for user u1
recommended.items.u1 <- predict(Rec.model, affinity.matrix['1',], n=3)
# to display them
as(recommended.items.u1, "list")
# to obtain the top 3
recommended.items.u1.top3 <- bestN(recommended.items.u1, n = 1)
# to display them
as(recommended.items.u1.top3, "list")

# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
e <- evaluationScheme(affinity.matrix[1:10], method="split", train=0.5, given=-1)
# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")
# creation of recommender model based on ibcf for comparison
Rec.ibcf <- Recommender(getData(e, "train"), "IBCF")
# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), type="ratings")
# making predictions on the test data set
p.ibcf <- predict(Rec.ibcf, getData(e, "known"), type="ratings")
# obtaining the error metrics for both approaches and comparing them
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))
error.ibcf<-calcPredictionAccuracy(p.ibcf, getData(e, "unknown"))
error <- rbind(error.ubcf,error.ibcf)
rownames(error) <- c("UBCF","IBCF")
error



