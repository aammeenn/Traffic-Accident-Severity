library(e1071);
library(caret);

# SVM CV function
svmCV = function(train, val, gammain, nuin) {
  # build model
  kernel = "radial";
  gamma = gammain;
  nu = nuin;
  trainminor = train[!(train$Severity=="Slight"),];
  svmorig = svm(Severity~., data = trainminor, type = "one-classification", kernel = kernel, gamma = gamma, nu = nu);
  svmpre = predict(svmorig, newdata = val);
  svmpreresult = as.data.frame(svmpre);
  svmpreresult[svmpreresult==TRUE] = "Non-Slight";
  svmpreresult[svmpreresult==FALSE] = "Slight";
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
for (gamma in c(0.7)) {
  for (nu in c(0.3)) {
    svmresults = CalculateCV(t_set2345,t_set1345,t_set1245,t_set1235,t_set1234,t_set1,t_set2,t_set3,t_set4,t_set5,gamma,nu);
    svmresults = cbind(gamma,nu,svmresults);
    allresults = rbind(allresults,svmresults);
  }
}