library(RWeka);
library(caret);
library(AUC);
library(precrec);

LogicGetPR = function(train, val) {
  # build model
  logic = Logistic(Severity~., data = train);
  logicpre = predict(logic, newdata = val, type = "class");
  logictruth = val$Severity;
  
  #calculate auc
  logicpreprob = as.data.frame(predict(logic, newdata = val, type = "prob"));
  logictruthtable = as.data.frame(logictruth);
  logictruthtable[] = lapply(logictruthtable, as.character);
  logictruthtablepos = logictruthtable;
  logictruthtableneg = logictruthtable;
  logictruthtablepos[logictruthtablepos=="Non-Slight"] = 1;
  logictruthtablepos[logictruthtablepos=="Slight"] = 0;
  logictruthtableneg[logictruthtableneg=="Non-Slight"] = 0;
  logictruthtableneg[logictruthtableneg=="Slight"] = 1;
  
  # return pr
  return(cbind(logicpreprob$`Non-Slight`,logictruthtablepos));
}

# calculate PR curve
CalculatePR = function(train1,train2,train3,train4,train5,val1,val2,val3,val4,val5) {
  train = train1;
  val = val1;
  PR1 = LogicGetPR(train, val);
  train = train2;
  val = val2;
  PR2 = LogicGetPR(train, val);
  train = train3;
  val = val3;
  PR3 = LogicGetPR(train, val);
  train = train4;
  val = val4;
  PR4 = LogicGetPR(train, val);
  train = train5;
  val = val5;
  PR5 = LogicGetPR(train, val);
  
  PRall = rbind(PR1,PR2,PR3,PR4,PR5);
  PRdata = mmdata(scores = PRall[,1], labels = PRall[,2]);
  PREval = evalmod(PRdata);
  return(PREval);
}

LogicPROriginal = CalculatePR(set2345,set1345,set1245,set1235,set1234,set1,set2,set3,set4,set5);
plot(LogicPROriginal, curvetype = "PRC");

LogicPROver = CalculatePR(set2345over,set1345over,set1245over,set1235over,set1234over,set1,set2,set3,set4,set5);
plot(LogicPROver, curvetype = "PRC");
