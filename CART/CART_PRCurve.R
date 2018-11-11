library(rpart);
library(caret);
library(AUC);
library(precrec);

# CART PR function
CARTGetPR = function(train, val) {
  # build model
  minsplit = 55;
  cp = 0.0001;
  control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
  CART = rpart(Severity~., data = train, control = control);
  CARTpre = predict(CART, newdata = val, type = "class");
  CARTtruth = val$Severity;
  
  # calculate recall, precision, f-measure
  recallpos = recall(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  recallneg = recall(data = CARTpre, reference = CARTtruth, relevant = "Slight");
  precisionpos = precision(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  precisionneg = precision(data = CARTpre, reference = CARTtruth, relevant = "Slight");
  fmeaspos = F_meas(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  
  #calculate auc
  CARTpreprob = as.data.frame(predict(CART, newdata = val, type = "prob"));
  CARTtruthtable = as.data.frame(CARTtruth);
  CARTtruthtable[] = lapply(CARTtruthtable, as.character);
  CARTtruthtablepos = CARTtruthtable;
  CARTtruthtableneg = CARTtruthtable;
  CARTtruthtablepos[CARTtruthtablepos=="Non-Slight"] = 1;
  CARTtruthtablepos[CARTtruthtablepos=="Slight"] = 0;
  CARTtruthtableneg[CARTtruthtableneg=="Non-Slight"] = 0;
  CARTtruthtableneg[CARTtruthtableneg=="Slight"] = 1;
  
  # return all results
  return(cbind(CARTpreprob$`Non-Slight`,CARTtruthtablepos));
}

# calculate PR function
CalculatePR = function(train1,train2,train3,train4,train5,val1,val2,val3,val4,val5) {
  train = train1;
  val = val1;
  PR1 = CARTGetPR(train, val);
  train = train2;
  val = val2;
  PR2 = CARTGetPR(train, val);
  train = train3;
  val = val3;
  PR3 = CARTGetPR(train, val);
  train = train4;
  val = val4;
  PR4 = CARTGetPR(train, val);
  train = train5;
  val = val5;
  PR5 = CARTGetPR(train, val);
  
  PRall = rbind(PR1,PR2,PR3,PR4,PR5);
  PRdata = mmdata(scores = PRall[,1], labels = PRall[,2]);
  PREval = evalmod(PRdata);
  return(PREval);
}

CARTPROriginal = CalculatePR(set2345,set1345,set1245,set1235,set1234,set1,set2,set3,set4,set5);
plot(CARTPROriginal, curvetype = "PRC");

CARTPROver = CalculatePR(set2345over,set1345over,set1245over,set1235over,set1234over,set1,set2,set3,set4,set5);
plot(CARTPROver, curvetype = "PRC");

CARTPRUnder = CalculatePR(set2345under,set1345under,set1245under,set1235under,set1234under,set1,set2,set3,set4,set5);
plot(CARTPRUnder, curvetype = "PRC");

CARTPRSMOTE = CalculatePR(set2345SMOTE,set1345SMOTE,set1245SMOTE,set1235SMOTE,set1234SMOTE,set1,set2,set3,set4,set5);
plot(CARTPRSMOTE, curvetype = "PRC");