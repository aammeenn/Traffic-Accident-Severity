library(tree);
library(caret);
library(AUC);
library(precrec);

# CART CV function
CARTCV = function(train, val) {
  # build model
  mincut = 5;
  minsize = 10;
  mindev = 0.0015;
  control = tree.control(nobs = nrow(train), mincut = mincut, minsize = minsize, mindev = mindev);
  CART = tree(Severity~., data = train, control = control, weights = c(0.7,0.3));
  CARTpre = predict(CART, newdata = val, type = "class");
  CARTtruth = val$Severity;
  
  # calculate recall, precision, f-measure
  recallpos = recall(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  recallneg = recall(data = CARTpre, reference = CARTtruth, relevant = "Slight");
  precisionpos = precision(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  precisionneg = precision(data = CARTpre, reference = CARTtruth, relevant = "Slight");
  fmeaspos = F_meas(data = CARTpre, reference = CARTtruth, relevant = "Non-Slight");
  
  #calculate auc
  CARTpreprob = as.data.frame(predict(CART, newdata = val, type = "vector"));
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
CARTCostSens = CalculateCV(set2345,set1345,set1245,set1235,set1234,set1,set2,set3,set4,set5);