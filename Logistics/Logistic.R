library(RWeka);
library(caret);
library(AUC);
library(precrec);

#logistic cv function
LogisticCV = function(train, val) {
  # building model
  logic = Logistic(Severity~., data = train);
  logicpre = predict(logic, newdata = val, type = "class");
  logictruth = val$Severity;
  
  # calculate recall, precision, f-measure
  recallpos = recall(data = logicpre, reference = logictruth, relevant = "Non-Slight");
  recallneg = recall(data = logicpre, reference = logictruth, relevant = "Slight");
  precisionpos = precision(data = logicpre, reference = logictruth, relevant = "Non-Slight");
  precisionneg = precision(data = logicpre, reference = logictruth, relevant = "Slight");
  fmeaspos = F_meas(data = logicpre, reference = logictruth, relevant = "Non-Slight");
  
  # calculate auc
  logicpreprob = as.data.frame(predict(logic, newdata = val, type = "prob"));
  logictruthtable = as.data.frame(logictruth);
  logictruthtable[] = lapply(logictruthtable, as.character);
  logictruthtablepos = logictruthtable;
  logictruthtableneg = logictruthtable;
  logictruthtablepos[logictruthtablepos=="Non-Slight"] = 1;
  logictruthtablepos[logictruthtablepos=="Slight"] = 0;
  logictruthtableneg[logictruthtableneg=="Non-Slight"] = 0;
  logictruthtableneg[logictruthtableneg=="Slight"] = 1;

  prdatapos = mmdata(scores = logicpreprob$`Non-Slight`, labels = logictruthtablepos);
  prevalpos = evalmod(prdatapos);
  praucpostable = auc(prevalpos);
  prdataneg = mmdata(scores = logicpreprob$Slight, labels = logictruthtableneg);
  prevalneg = evalmod(prdataneg);
  praucnegtable = auc(prevalneg);
  rocauc = praucpostable[1,4];
  praucpos = praucpostable[2,4];
  praucneg = praucnegtable[2,4];
  
  #posscore = logicprelabel$`Non-Slight`[logicprelabel$logictruth==0];
  #negscore = logicprelabel$`Non-Slight`[logicprelabel$logictruth==1];
  #prneg = pr.curve(scores.class0 = posscore, scores.class1 = negscore);
  #prpos = pr.curve(scores.class0 = negscore, scores.class1 = posscore);
  #praucpos = prpos$auc.integral;
  #praucneg = prneg$auc.integral;

  # return all results
  return(c(recallpos,recallneg,precisionpos,precisionneg,fmeaspos,rocauc,praucpos,praucneg));
}

# calculate original
train = set2345;
val = set1;
logic1 = LogisticCV(train, val);

train = set1345;
val = set2;
logic2 = LogisticCV(train, val);

train = set1245;
val = set3;
logic3 = LogisticCV(train, val);

train = set1235;
val = set4;
logic4 = LogisticCV(train, val);

train = set1234;
val = set5;
logic5 = LogisticCV(train, val);

logicoriginal = ((logic1+logic2+logic3+logic4)*3544 + logic5*3545) / 17721;

# calculate random over
train = set2345over;
val = set1;
logic1over = LogisticCV(train, val);

train = set1345over;
val = set2;
logic2over = LogisticCV(train, val);

train = set1245over;
val = set3;
logic3over = LogisticCV(train, val);

train = set1235over;
val = set4;
logic4over = LogisticCV(train, val);

train = set1234over;
val = set5;
logic5over = LogisticCV(train, val);

logicover = ((logic1over+logic2over+logic3over+logic4over)*3544 + logic5over*3545) / 17721;

# calculate random under
train = set2345under;
val = set1;
logic1under = LogisticCV(train, val);

train = set1345under;
val = set2;
logic2under = LogisticCV(train, val);

train = set1245under;
val = set3;
logic3under = LogisticCV(train, val);

train = set1235under;
val = set4;
logic4under = LogisticCV(train, val);

train = set1234under;
val = set5;
logic5under = LogisticCV(train, val);

logicunder = ((logic1under+logic2under+logic3under+logic4under)*3544 + logic5under*3545) / 17721;

# calculate SMOTE
train = set2345SMOTE;
val = set1;
logic1SMOTE = LogisticCV(train, val);

train = set1345SMOTE;
val = set2;
logic2SMOTE = LogisticCV(train, val);

train = set1245SMOTE;
val = set3;
logic3SMOTE = LogisticCV(train, val);

train = set1235SMOTE;
val = set4;
logic4SMOTE = LogisticCV(train, val);

train = set1234SMOTE;
val = set5;
logic5SMOTE = LogisticCV(train, val);

logicSMOTE = ((logic1SMOTE+logic2SMOTE+logic3SMOTE+logic4SMOTE)*3544 + logic5SMOTE*3545) / 17721;