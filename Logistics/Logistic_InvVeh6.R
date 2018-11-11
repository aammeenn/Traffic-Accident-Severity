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

  # return all results
  return(c(recallpos,recallneg,precisionpos,precisionneg,fmeaspos,rocauc,praucpos,praucneg));
}

# calculate random over
train = set2345over;
val = set1[set1$InvVeh>=6,];
logic1over = LogisticCV(train, val);

train = set1345over;
val = set2[set2$InvVeh>=6,];
logic2over = LogisticCV(train, val);

train = set1245over;
val = set3[set3$InvVeh>=6,];
logic3over = LogisticCV(train, val);

train = set1235over;
val = set4[set4$InvVeh>=6,];
logic4over = LogisticCV(train, val);

train = set1234over;
val = set5[set5$InvVeh>=6,];
logic5over = LogisticCV(train, val);

logicover = (logic1over*8 + logic2over*8 + logic3over*7 + logic4over*4 + logic5over*11) / 38;
