library(tree);
library(randomForest);

TotalOriginal = rbind(set1234,set1235,set1245,set1345,set2345);
TotalOver = rbind(set1234over,set1235over,set1245over,set1345over,set2345over);
TotalUnder = rbind(set1234under,set1235under,set1245under,set1345under,set2345under);
TotalSMOTE = rbind(set1234SMOTE,set1235SMOTE,set1245SMOTE,set1345SMOTE,set2345SMOTE);

minsplit = 55;
cp = 0.0001;
control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
CARTOriginal = rpart(Severity~., data = TotalOriginal, control = control);
ImpOriginal = as.data.frame(CARTOriginal$variable.importance);

minsplit = 200;
cp = 0.0001;
control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
CARTOver = rpart(Severity~., data = TotalOver, control = control);
ImpOver = as.data.frame(CARTOver$variable.importance);

minsplit = 100;
cp = 0.0001;
control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
CARTUnder = rpart(Severity~., data = TotalUnder, control = control);
ImpUnder = as.data.frame(CARTUnder$variable.importance);

minsplit = 250;
cp = 0.0001;
control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
CARTSMOTE = rpart(Severity~., data = TotalSMOTE, control = control);
ImpSMOTE = as.data.frame(CARTSMOTE$variable.importance);

mtry = 4;
ntree = 100;
nodesize = 10;
set.seed(1)
RFOriginal = randomForest(Severity~., data = TotalOriginal, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize, importance = TRUE);
RFImpOriginal = RFOriginal$importance;

mtry = 7;
ntree = 100;
nodesize = 40;
set.seed(1)
RFOver = randomForest(Severity~., data = TotalOver, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize, importance = TRUE);
RFImpOver = RFOver$importance;

mtry = 5;
ntree = 100;
set.seed(1)
RFUnder = randomForest(Severity~., data = TotalUnder, mtry = mtry, ntree = ntree, replace = FALSE, importance = TRUE);
RFImpUnder = RFUnder$importance;

mtry = 5;
ntree = 100;
set.seed(1)
RFSMOTE = randomForest(Severity~., data = TotalSMOTE, mtry = mtry, ntree = ntree, replace = FALSE, importance = TRUE);
RFImpSMOTE = RFSMOTE$importance;