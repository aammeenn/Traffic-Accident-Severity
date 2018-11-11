library(randomForest);
library(caret);
library(AUC);
library(precrec);

# RF PR function
RFGetPR = function(train, val) {
  # build model
  mtry = 7;
  ntree = 100;
  nodesize = 40;
  set.seed(1)
  RF = randomForest(Severity~., data = train, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize);
  RFpre = predict(RF, newdata = val, type = "response");
  RFtruth = val$Severity;
  
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
  
  # return pr
  return(cbind(RFpreprob$`Non-Slight`,RFtruthtablepos));
}

# calculate PR curve
CalculatePR = function(train1,train2,train3,train4,train5,val1,val2,val3,val4,val5) {
  train = train1;
  val = val1;
  PR1 = RFGetPR(train, val);
  train = train2;
  val = val2;
  PR2 = RFGetPR(train, val);
  train = train3;
  val = val3;
  PR3 = RFGetPR(train, val);
  train = train4;
  val = val4;
  PR4 = RFGetPR(train, val);
  train = train5;
  val = val5;
  PR5 = RFGetPR(train, val);
  
  PRall = rbind(PR1,PR2,PR3,PR4,PR5);
  PRdata = mmdata(scores = PRall[,1], labels = PRall[,2]);
  PREval = evalmod(PRdata);
  return(PREval);
}

RFPROriginal = CalculatePR(set2345,set1345,set1245,set1235,set1234,set1,set2,set3,set4,set5);
plot(RFPROriginal, curvetype = "PRC");

RFPROver = CalculatePR(set2345over,set1345over,set1245over,set1235over,set1234over,set1,set2,set3,set4,set5);
plot(RFPROver, curvetype = "PRC");

RFPRUnder = CalculatePR(set2345under,set1345under,set1245under,set1235under,set1234under,set1,set2,set3,set4,set5);
plot(RFPRUnder, curvetype = "PRC");

RFPRSMOTE = CalculatePR(set2345SMOTE,set1345SMOTE,set1245SMOTE,set1235SMOTE,set1234SMOTE,set1,set2,set3,set4,set5);
plot(RFPRSMOTE, curvetype = "PRC");