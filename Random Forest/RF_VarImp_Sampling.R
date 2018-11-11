library(randomForest);

setorig = rbind(set1,set2,set3,set4,set5);
setover = rbind(set1234over,set1235over,set1245over,set1345over,set2345over);
setunder = rbind(set1234under,set1235under,set1245under,set1345under,set2345under);
setSMOTE = rbind(set1234SMOTE,set1235SMOTE,set1245SMOTE,set1345SMOTE,set2345SMOTE);

RFIMP = function(train,mtry,ns) {
  ntree = 100;
  set.seed(1)
  RF = randomForest(Severity~., data = train, mtry = mtry, ntree = ntree, importance = TRUE, nodesize = ns);
  Imp = RF$importance;
  
  return(Imp);
}

OrigImp = RFIMP(setorig,4,7);
OverImp = RFIMP(setover,7,40);
UnderImp = RFIMP(setunder,5,5);
SMOTEImp = RFIMP(setSMOTE,5,10);
