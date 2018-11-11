library(e1071);
library(caret);

# SVM CV function
svmCV = function(train, val, gammain, nuin) {
  # build model
  kernal = "sigmoid";
  gamma = gammain;
  nu = nuin;
  trainmajor = train[!(train$Severity=="Non-Slight"),];
  svmorig = svm(Severity~., data = trainmajor, type = "one-classification", kernal = kernal, gamma = gamma, nu = nu);
  svmpre = predict(svmorig, newdata = val);
  svmpreresult = as.data.frame(svmpre);
  svmpreresult[svmpreresult==TRUE] = "Slight";
  svmpreresult[svmpreresult==FALSE] = "Non-Slight";
  svmpreresult = lapply(svmpreresult, as.factor);
  svmpreresult = svmpreresult[[1]];
  svmtruth = val$Severity;
  
  # calculate recall, precision, f-measure
  recallpos = recall(data = svmpreresult, reference = svmtruth, relevant = "Non-Slight");
  recallneg = recall(data = svmpreresult, reference = svmtruth, relevant = "Slight");
  precisionpos = precision(data = svmpreresult, reference = svmtruth, relevant = "Non-Slight");
  precisionneg = precision(data = svmpreresult, reference = svmtruth, relevant = "Slight");
  fmeaspos = F_meas(data = svmpreresult, reference = svmtruth, relevant = "Non-Slight");
  
  #return results
  results = c(recallpos,recallneg,precisionpos,precisionneg,fmeaspos);
  return(t(as.data.frame(results)));
}

# calculate CV function
CalculateCV = function(train1,train2,train3,train4,train5,val1,val2,val3,val4,val5,gammain,nuin) {
  train = train1;
  val = val1;
  CV1 = svmCV(train, val, gammain, nuin);
  train = train2;
  val = val2;
  CV2 = svmCV(train, val, gammain, nuin);
  train = train3;
  val = val3;
  CV3 = svmCV(train, val, gammain, nuin);
  train = train4;
  val = val4;
  CV4 = svmCV(train, val, gammain, nuin);
  train = train5;
  val = val5;
  CV5 = svmCV(train, val, gammain, nuin);
  
  CVall = ((CV1+CV2+CV3+CV4)*3544 + CV5*3545) / 17721;
  return(CVall);
}

allresults = data.frame();
for (gamma in c(0.000007)) {
  for (nu in c(0.45)) {
    svmresults = CalculateCV(set2345,set1345,set1245,set1235,set1234,set1,set2,set3,set4,set5,gamma,nu);
    svmresults = cbind(gamma,nu,svmresults);
    allresults = rbind(allresults,svmresults);
  }
}