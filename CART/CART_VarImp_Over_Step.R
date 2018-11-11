library(rpart);
library(caret);

# bind datsets
TotalOver = rbind(set1234over,set1235over,set1245over,set1345over,set2345over);
TotalOver = TotalOver[,c(1:7,9:23,8)];
TotalTrain = TotalOver;

# fit model & get f-meas
GetFmeas = function(train) {
  minsplit = 1000;
  cp = 0.0001;
  control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
  CART = rpart(Severity~., data = train, control = control);
  CARTpre = predict(CART, newdata = train, type = "class");
  CARTtruth = train$Severity;
  fmeaspos = F_meas(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  return(fmeaspos);
}

ImpOrder = data.frame();
varnumleft = 22;

for(i in 1:8) {
  VarList = colnames(TotalTrain[,c(1:(varnumleft))]);
  minFmeas = 1;
  mostimpvar = 0;
  for (varnum in c(1:varnumleft)) {
    Train = TotalTrain[,-varnum];
    fmeas = GetFmeas(Train);
    show(fmeas);
    if (fmeas<minFmeas) {
      minFmeas = fmeas;
      mostimpvar = varnum;
    }
  }
  InputVar = t(as.data.frame(c(VarList[mostimpvar],mostimpvar)));
  rownames(InputVar) = 1;
  colnames(InputVar) = c("Var","VarNum");
  ImpOrder = rbind(ImpOrder,InputVar);
  TotalTrain = TotalTrain[,-mostimpvar];
  varnumleft = length(TotalTrain)-1;
}
