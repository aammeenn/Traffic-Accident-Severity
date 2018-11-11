library(randomForest)

train_orig = rbind(set1,set2,set3,set4,set5)
train_over = rbind(set1234over,set1235over,set1245over,set1345over,set2345over)
train_under = rbind(set1234under,set1235under,set1245under,set1345under,set2345under)

# original
mtry = 4;
ntree = 100;
nodesize = 7;
set.seed(1)
RF_orig = randomForest(Severity~., data = train_orig, mtry = mtry, ntree = ntree, replace = FALSE, nodesize = nodesize);
partial_orig_Po = partialPlot(RF_orig, pred.data = train_orig, x.var = "PoliceUnit", which.class = "Non-Slight")
partial_orig_Mon = partialPlot(RF_orig, pred.data = train_orig, x.var = "Month", which.class = "Non-Slight")
partial_orig_We = partialPlot(RF_orig, pred.data = train_orig, x.var = "WeekDay", which.class = "Non-Slight")
partial_orig_Acc = partialPlot(RF_orig, pred.data = train_orig, x.var = "AccType", which.class = "Non-Slight")
partial_orig_Avg = partialPlot(RF_orig, pred.data = train_orig, x.var = "AvgProYear", which.class = "Non-Slight", n.pt = 49)
partial_orig_Ro = partialPlot(RF_orig, pred.data = train_orig, x.var = "RoadWidth", which.class = "Non-Slight")

# over
mtry_over = 7;
nodesize_over = 40;
set.seed(1)
RF_over = randomForest(Severity~., data = train_over, mtry = mtry_over, ntree = ntree, replace = FALSE, nodesize = nodesize_over);
partial_over_Po = partialPlot(RF_over, pred.data = train_over, x.var = "PoliceUnit", which.class = "Non-Slight")
partial_over_Mon = partialPlot(RF_over, pred.data = train_over, x.var = "Month", which.class = "Non-Slight")
partial_over_We = partialPlot(RF_over, pred.data = train_over, x.var = "WeekDay", which.class = "Non-Slight")
partial_over_Acc = partialPlot(RF_over, pred.data = train_over, x.var = "AccType", which.class = "Non-Slight")
partial_over_Avg = partialPlot(RF_over, pred.data = train_over, x.var = "AvgProYear", which.class = "Non-Slight", n.pt = 49)
partial_over_Ro = partialPlot(RF_over, pred.data = train_over, x.var = "RoadWidth", which.class = "Non-Slight")
partial_over_maxage = partialPlot(RF_over, pred.data = train_over, x.var = "MaxAge", which.class = "Non-Slight")
partial_over_minage = partialPlot(RF_over, pred.data = train_over, x.var = "MinAge", which.class = "Non-Slight")


# under
mtry_under = 5;
nodesize_under = 5;
set.seed(1)
RF_under = randomForest(Severity~., data = train_under, mtry = mtry_under, ntree = ntree, replace = FALSE, nodesize = nodesize_under);
partial_under_Po = partialPlot(RF_under, pred.data = train_under, x.var = "PoliceUnit", which.class = "Non-Slight")
partial_under_Mon = partialPlot(RF_under, pred.data = train_under, x.var = "Month", which.class = "Non-Slight")
partial_under_We = partialPlot(RF_under, pred.data = train_under, x.var = "WeekDay", which.class = "Non-Slight")
partial_under_Acc = partialPlot(RF_under, pred.data = train_under, x.var = "AccType", which.class = "Non-Slight")
partial_under_Avg = partialPlot(RF_under, pred.data = train_under, x.var = "AvgProYear", which.class = "Non-Slight", n.pt = 49)
partial_under_Ro = partialPlot(RF_under, pred.data = train_under, x.var = "RoadWidth", which.class = "Non-Slight")
