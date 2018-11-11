library(RWeka);
library(caret);

# bind datsets
TotalOriginal = rbind(set1,set2,set3,set4,set5);
TotalOriginal = TotalOriginal[,c(1:7,9:23,8)];
TotalTrain = TotalOriginal;

# fit model & get f-meas
GetFmeas = function(train) {
  logic = Logistic(Severity~., data = train);
  logicpre = predict(logic, newdata = train, type = "class");
  logictruth = train$Severity;
  fmeaspos = F_meas(data = logicpre, reference = logictruth, relevant = "Non-Slight");
  return(fmeaspos);
}

ImpOrder = data.frame();
varnumleft = 22;

for (i in 1:5) {
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
