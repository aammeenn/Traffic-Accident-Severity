library(e1071)
#library(caret)

train_orig = rbind(set1,set2,set3,set4,set5)
#train_over = rbind(set1234over,set1235over,set1245over,set1345over,set2345over)
#train_under = rbind(set1234under,set1235under,set1245under,set1345under,set2345under)

nu = 0.008
kernel = "linear"
train_neg = train_orig[!(train_orig$Severity=="Non-Slight"),]
svm_neg_orig = svm(Severity~., data = train_neg, type = "one-classification", kernel = kernel, nu = nu)

#svmpre = predict(svm_pos_orig, newdata = train_orig)
#svmpreresult = as.data.frame(svmpre)
#svmpreresult[svmpreresult==TRUE] = "Non-Slight"
#svmpreresult[svmpreresult==FALSE] = "Slight"

n_row = nrow(train_orig)
result = data.frame()

for (avgproyear in seq(1966,1970)){
  sum = 0
  for (rownum in seq(1,n_row)){
    test = train_orig[rownum,]
    test$AvgProYear = avgproyear
    svmpre = predict(svm_neg_orig, newdata = test)
    if (svmpre==FALSE){
      sum = sum + 1 
    }
  }
  avg = sum / n_row
  result = rbind(result,avg)
}
