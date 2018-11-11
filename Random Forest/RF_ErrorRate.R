library(randomForest);

GetER = function(train,mtry,nd) {
  mtry = mtry;
  ntree = 100;
  nodesize = nd;
  set.seed(1)
  RF = randomForest(Severity~., data = train, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize);
  er = RF$err.rate;
}

GetAllER = function(train1,train2,train3,train4,train5,mtry,nd) {
  er1 = GetER(train1,mtry,nd);
  er2 = GetER(train2,mtry,nd);
  er3 = GetER(train3,mtry,nd);
  er4 = GetER(train4,mtry,nd);
  er5 = GetER(train5,mtry,nd);
  
  aller = (er1+er2+er3+er4+er5) / 5;
  return(aller);
}

RFEROrig = GetAllER(set1234,set1235,set1245,set1345,set2345,4,7);
RFEROver = GetAllER(set1234over,set1235over,set1245over,set1345over,set2345over,7,40);
RFERUnder = GetAllER(set1234under,set1235under,set1245under,set1345under,set2345under,5,5);
RFERSMOTE = GetAllER(set1234SMOTE,set1235SMOTE,set1245SMOTE,set1345SMOTE,set2345SMOTE,5,10);
