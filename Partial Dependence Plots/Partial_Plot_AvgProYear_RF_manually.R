library(randomForest)
#library(caret)

train_orig = rbind(set1,set2,set3,set4,set5)
train_over = rbind(set1234over,set1235over,set1245over,set1345over,set2345over)
#train_under = rbind(set1234under,set1235under,set1245under,set1345under,set2345under)

ntree = 100
mtry_over = 7;
nodesize_over = 40;
set.seed(1)
RF_over = randomForest(Severity~., data = train_over, mtry = mtry_over, ntree = ntree, replace = FALSE, nodesize = nodesize_over);

n_row = nrow(train_orig)
result = data.frame()

for (avgproyear in seq(1995,1996)){
  sum = 0
  for (rownum in seq(1,n_row)){
    test = train_orig[rownum,]
    test$AvgProYear = avgproyear
    RFpre = predict(RF_over, newdata = test, type = "prob")[1]
    sum = sum + RFpre
  }
  avg = sum / n_row
  result = rbind(result,avg)
}
