library(e1071);
library(caret);

# bind datsets
TotalOriginal = rbind(set1,set2,set3,set4,set5);
TotalOriginal = TotalOriginal[,c(1:7,9:23,8)];
TotalTrain = TotalOriginal;

# fit model & get f-meas
GetFMeas = function(train) {
  kernel = "linear";
  nu = 0.45;
  trainmajor = train[!(train$Severity=="Non-Slight"),];
  val = train;
  svmorig = svm(Severity~., data = trainmajor, type = "one-classification", kernel = kernel, nu = nu);
  svmpre = predict(svmorig, newdata = val);
  svmpreresult = as.data.frame(svmpre);
  svmpreresult[svmpreresult==TRUE] = "Slight";
  svmpreresult[svmpreresult==FALSE] = "Non-Slight";
  svmpreresult = lapply(svmpreresult, as.factor);
  svmpreresult = svmpreresult[[1]];
  svmtruth = val$Severity;
  
  fmeaspos = F_meas(data = svmpreresult, reference = svmtruth, relevant = "Non-Slight");
  show(fmeaspos);
  return(fmeaspos);
}

ImpOrder = data.frame();
varnumleft = 22;

while (varnumleft>0) {
  VarList = colnames(TotalTrain[,c(1:(varnumleft))]);
  minfmeas = 1;
  mostimpvar = 0;
  for (varnum in c(1:varnumleft)) {
    Train = TotalTrain[,-varnum];
    fmeas = GetFMeas(Train);
    if (fmeas<minfmeas) {
      minfmeas = fmeas;
      mostimpvar = varnum;
    }
  }
  InputVar = t(as.data.frame(c(VarList[mostimpvar],minfmeas)));
  rownames(InputVar) = 1;
  colnames(InputVar) = c("Var","f-meas");
  ImpOrder = rbind(ImpOrder,InputVar);
  TotalTrain = TotalTrain[,-mostimpvar];
  varnumleft = length(TotalTrain)-1;
  show(InputVar);
}

