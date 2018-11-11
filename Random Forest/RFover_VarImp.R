library(randomForest);
library(caret);

# bind datsets
TotalOver = rbind(set1234over,set1235over,set1245over,set1345over,set2345over);
TotalOver = TotalOver[,c(1:7,9:23,8)];
TotalTrain = TotalOver;

# fit model & get f-meas
GetFMeas = function(train, mtry) {
  val = train;
  ntree = 50;
  nodesize = 40;
  set.seed(1)
  RF = randomForest(Severity~., data = train, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize);
  RFpre = predict(RF, newdata = val, type = "response");
  RFtruth = val$Severity;
  
  # calculate f-measure
  fmeaspos = F_meas(data = RFpre, reference = RFtruth, relevant = "Non-Slight");
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
    fmeas = GetFMeas(Train, ceiling(varnumleft/3));
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
