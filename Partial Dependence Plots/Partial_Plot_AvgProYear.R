library(RWeka)
library(rpart)
library(e1071)
library(randomForest)
library(pdp)

train_orig = rbind(set1,set2,set3,set4,set5)
train_over = rbind(set1234over,set1235over,set1245over,set1345over,set2345over)
train_under = rbind(set1234under,set1235under,set1245under,set1345under,set2345under)

# Logistic
LR_over = Logistic(Severity~., data = train_over)
partial_over_Avg_LR = partial(LR_over, pred.var = "AvgProYear", which.class = "Non-Slight", type = "classification", train = train_orig, grid.resolution = 49)

# CART
minsplit = 1000;
cp = 0.0001;
control = rpart.control(minsplit = minsplit, cp = cp, maxsurrogate = 0, usesurrogate = 0, xval = 0)
CART_over = rpart(Severity~., data = train_over, control = control)
partial_over_Avg_CART = partial(CART_over, pred.var = "AvgProYear", which.class = "Non-Slight", type = "classification", train = train_orig, grid.resolution = 49)

# Random Forest
ntree = 100
mtry_over = 7;
nodesize_over = 40;
set.seed(1)
RF_over = randomForest(Severity~., data = train_over, mtry = mtry_over, ntree = ntree, replace = FALSE, nodesize = nodesize_over);
partial_over_Avg_RF = partial(RF_over, pred.var = "AvgProYear", which.class = "Non-Slight", type = "classification", train = train_orig, grid.resolution = 49)

# negative oc-svm
kernel = "linear"
nu = 0.008
train_neg = train_orig[!(train_orig$Severity=="Non-Slight"),]
svm_neg_orig = svm(Severity~., data = train_neg, type = "one-classification", kernel = kernel, nu = nu)
partial_orig_Avg_NGSVM = partial(svm_neg_orig, pred.var = "AvgProYear", type = "regression", train = train_orig, grid.resolution = 49)

# positive oc-svm
nu = 0.45
train_pos = train_orig[!(train_orig$Severity=="Slight"),]
svm_pos_orig = svm(Severity~., data = train_pos, type = "one-classification", kernel = kernel, nu = nu)
partial_orig_Avg_POSVM = partial(svm_pos_orig, pred.var = "AvgProYear", type = "regression", train = train_orig, grid.resolution = 49)