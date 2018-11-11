library(randomForest);
library(caret);
library(AUC);
library(precrec);

# RF CV function
RFCV = function(train, val) {
  # build model
  mtry = 5;
  ntree = 100;
  nodesize = 20;
  set.seed(1)
  RF = randomForest(Severity~., data = train, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize);
  RFpre = predict(RF, newdata = val, type = "response");
  RFtruth = val$Severity;
  
  # calculate recall, precision, f-measure
  recallpos = recall(data = RFpre, reference = RFtruth, relevant = "Non-Slight");
  recallneg = recall(data = RFpre, reference = RFtruth, relevant = "Slight");
  precisionpos = precision(data = RFpre, reference = RFtruth, relevant = "Non-Slight");
  precisionneg = precision(data = RFpre, reference = RFtruth, relevant = "Slight");
  fmeaspos = F_meas(data = RFpre, reference = RFtruth, relevant = "Non-Slight");
  
  #calculate auc
  RFpreprob = as.data.frame(predict(RF, newdata = val, type = "prob"));
  RFtruthtable = as.data.frame(RFtruth);
  RFtruthtable[] = lapply(RFtruthtable, as.character);
  RFtruthtablepos = RFtruthtable;
  RFtruthtableneg = RFtruthtable;
  RFtruthtablepos[RFtruthtablepos=="Non-Slight"] = 1;
  RFtruthtablepos[RFtruthtablepos=="Slight"] = 0;
  RFtruthtableneg[RFtruthtableneg=="Non-Slight"] = 0;
  RFtruthtableneg[RFtruthtableneg=="Slight"] = 1;
  
  prdatapos = mmdata(scores = RFpreprob$`Non-Slight`, labels = RFtruthtablepos);
  prevalpos = evalmod(prdatapos);
  praucpostable = auc(prevalpos);
  prdataneg = mmdata(scores = RFpreprob$Slight, labels = RFtruthtableneg);
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
  CV1 = RFCV(train, val);
  train = train2;
  val = val2;
  CV2 = RFCV(train, val);
  train = train3;
  val = val3;
  CV3 = RFCV(train, val);
  train = train4;
  val = val4;
  CV4 = RFCV(train, val);
  train = train5;
  val = val5;
  CV5 = RFCV(train, val);
  
  CVall = ((CV1+CV2+CV3+CV4)*3544 + CV5*3545) / 17721;
  return(CVall);
}

# CART original
RFSMOTE = CalculateCV(set2345SMOTE,set1345SMOTE,set1245SMOTE,set1235SMOTE,set1234SMOTE,set1,set2,set3,set4,set5);