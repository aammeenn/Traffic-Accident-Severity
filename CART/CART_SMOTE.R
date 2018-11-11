library(rpart);
library(caret);
library(AUC);
library(precrec);

# CART CV function
CARTCV = function(train, val) {
  # build model
  minsplit = 300;
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
  
  prdatapos = mmdata(scores = CARTpreprob$`Non-Slight`, labels = CARTtruthtablepos);
  prevalpos = evalmod(prdatapos);
  praucpostable = auc(prevalpos);
  prdataneg = mmdata(scores = CARTpreprob$Slight, labels = CARTtruthtableneg);
  prevalneg = evalmod(prdataneg);
  praucnegtable = auc(prevalneg);
  rocauc = praucpostable[1,4];
  praucpos = praucpostable[2,4];
  praucneg = praucnegtable[2,4];
  
  # return all results
  results = c(recallpos,recallneg,precisionpos,precisionneg,fmeaspos,rocauc,praucpos,praucneg);
  return(t(as.data.frame(results)));
}

# calculate CV function
CalculateCV = function(train1,train2,train3,train4,train5,val1,val2,val3,val4,val5) {
  train = train1;
  val = val1;
  CV1 = CARTCV(train, val);
  train = train2;
  val = val2;
  CV2 = CARTCV(train, val);
  train = train3;
  val = val3;
  CV3 = CARTCV(train, val);
  train = train4;
  val = val4;
  CV4 = CARTCV(train, val);
  train = train5;
  val = val5;
  CV5 = CARTCV(train, val);
  
  CVall = ((CV1+CV2+CV3+CV4)*3544 + CV5*3545) / 17721;
  return(CVall);
}

# CART original
CARTSMOTE = CalculateCV(set2345SMOTE,set1345SMOTE,set1245SMOTE,set1235SMOTE,set1234SMOTE,set1,set2,set3,set4,set5);