
# bind datsets
TotalOriginal = rbind(set1,set2,set3,set4,set5);
TotalOriginal = TotalOriginal[,c(1:7,9:23,8)];
TotalTrain = TotalOriginal;

# fit model & get f-meas
GetAIC = function(train) {
  logic = glm(Severity~., family = binomial(link = "logit"), data = train);
  aic = extractAIC(logic)[2];
  return(aic);
}

ImpOrder = data.frame();
varnumleft = 22;

while (varnumleft>0) {
  VarList = colnames(TotalTrain[,c(1:(varnumleft))]);
  maxaic = 1;
  mostimpvar = 0;
  for (varnum in c(1:varnumleft)) {
    Train = TotalTrain[,-varnum];
    aic = GetAIC(Train);
    show(aic);
    if (aic>maxaic) {
      maxaic = aic;
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
