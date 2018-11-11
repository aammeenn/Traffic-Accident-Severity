library(rpart);

setorig = rbind(set1,set2,set3,set4,set5);
setover = rbind(set1234over,set1235over,set1245over,set1345over,set2345over);
setunder = rbind(set1234under,set1235under,set1245under,set1345under,set2345under);
setSMOTE = rbind(set1234SMOTE,set1235SMOTE,set1245SMOTE,set1345SMOTE,set2345SMOTE);

CARTIMP = function(train,minsplit) {
  cp = 0.0001;
  control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0);
  CART = rpart(Severity~., data = train, control = control);
  Imp = CART$variable.importance;
  return(Imp);
}

OrigImp = as.data.frame(CARTIMP(setorig,55));
OverImp = as.data.frame(CARTIMP(setover,200));
UnderImp = as.data.frame(CARTIMP(setunder,100));
SMOTEImp = as.data.frame(CARTIMP(setSMOTE,250));
