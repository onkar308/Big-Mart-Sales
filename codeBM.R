train = read.csv("TrainBM.csv")
test = read.csv("TestBM.csv")
test$Item_Outlet_Sales <- NA
combi <- rbind(train,test)
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
sum(is.na(combi$Item_Weight))
library(mice)
library(VIM)
str(combi)
misssing_index = which(is.na(combi$Item_Weight))
for(i in misssing_index) {
    item = combi$Item_Identifier[i]
    combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item],na.rm = T)}
view(train)
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
   item = combi$Item_Identifier[i]
   combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)}
sum(is.nan(combi$Outlet_Size))
outlet_size = which(is.na(combi$Outlet_Size))
combi <- within(combi,{Item_MRP_Clusters <- ifelse(Item_MRP < 69, "1st", 
                                            ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                            ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))})
combi <- within(combi,{Outlet_Location_Type_num <- ifelse(Outlet_Location_Type == "Tier3",0,
                                                  ifelse(Outlet_Location_Type == "Tier2",1,2))})
str(combi)
combi <- within(combi,{c("combi$Outlet_Location_Type","combi$Outlet_Size") == NULL})
#removing coloumns from combi
temp1 <- combi[-1]
temp1 <- temp1[-7]
temp1 <- temp1[-4]
combi <- within(combi,{Item_Visibility <- log(Item_Visibility +1)})
combi <- within(combi,{price_per_unit_wt <- log(price_per_unit_wt +1)})
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
train = combi[1:8523,]
test = combi[8524:14204,]
View(test)
test <- test[-12]
train <- train[-9]
View(train)
train <- train[-9]
View(train)
dataTrain = train[-1]
dataTest = test[-1]
View(dataTrain)
xTrain = dataTrain[-9]
yTrain = dataTrain[,9]
yTrain = dataTrain[,9:9]
library(randomForest)
set.seed(123)
bmRF = randomForest(yTrain~.,data = xTrain,)
rf_mod = train(x = xTrain, 
               +                y = yTrain,
               +                method='ranger', 
               +                num.trees = 100,
               +                importance = "permutation")

plot(rf_mod)
plot(varImp(rf_mod)) predictRF = predict(rf_mod,newdata = test)
submission$Item_Outlet_Sales = predictRF
write.csv(submission, "BMRF_submit.csv", row.names = F)

