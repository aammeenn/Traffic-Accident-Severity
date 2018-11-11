OHE = function(transset) {
  transset = transset[,c(1:7,9:23,8)];
  varlist = colnames(transset);
  i = 1;
  col = as.data.frame(transset[,i]);
  colnames(col) = varlist[i];
  transcol = as.data.frame(model.matrix(~.+0,data=col));
  result = transcol;
  for (i in c(2:22)) {
    col = as.data.frame(transset[,i]);
    colnames(col) = varlist[i];
    # this is the binarization function
    # the "+0" means removing intercept and keeping all the binary vars, instead of (num of levels - 1) binary vars
    transcol = as.data.frame(model.matrix(~.+0,data=col));
    result = cbind(result,transcol);
  }
  col = as.data.frame(transset[,23]);
  colnames(col) = varlist[23];
  result = cbind(result,col);
  
  cat = c(1:87,91,92);
  result[,cat][result[,cat]==0] = -1;
  num = c(88:90,93,94);
  for (j in num) {
    maxj = max(result[,j]);
    minj = min(result[,j]);
    result[,j] = -1 + (result[,j]-minj) / (maxj-minj) * 2;
  }
  return(result);
}

t_set1 = OHE(set1);
t_set2 = OHE(set2);
t_set3 = OHE(set3);
t_set4 = OHE(set4);
t_set5 = OHE(set5);
t_set1234 = OHE(set1234);
t_set1235 = OHE(set1235);
t_set1245 = OHE(set1245);
t_set1345 = OHE(set1345);
t_set2345 = OHE(set2345);
t_set1234under = OHE(set1234under);
t_set1235under = OHE(set1235under);
t_set1245under = OHE(set1245under);
t_set1345under = OHE(set1345under);
t_set2345under = OHE(set2345under);
t_set1234over = OHE(set1234over);
t_set1235over = OHE(set1235over);
t_set1245over = OHE(set1245over);
t_set1345over = OHE(set1345over);
t_set2345over = OHE(set2345over);
t_set1234SMOTE = OHE(set1234SMOTE);
t_set1235SMOTE = OHE(set1235SMOTE);
t_set1245SMOTE = OHE(set1245SMOTE);
t_set1345SMOTE = OHE(set1345SMOTE);
t_set2345SMOTE = OHE(set2345SMOTE);

write.csv(t_set1,"tt_set1.csv");
write.csv(t_set2,"tt_set2.csv");
write.csv(t_set3,"tt_set3.csv");
write.csv(t_set4,"tt_set4.csv");
write.csv(t_set5,"tt_set5.csv");
write.csv(t_set1234,"tt_set1234.csv");
write.csv(t_set1235,"tt_set1235.csv");
write.csv(t_set1245,"tt_set1245.csv");
write.csv(t_set1345,"tt_set1345.csv");
write.csv(t_set2345,"tt_set2345.csv");
write.csv(t_set1234under,"tt_set1234under.csv");
write.csv(t_set1235under,"tt_set1235under.csv");
write.csv(t_set1245under,"tt_set1245under.csv");
write.csv(t_set1345under,"tt_set1345under.csv");
write.csv(t_set2345under,"tt_set2345under.csv");
write.csv(t_set1234over,"tt_set1234over.csv");
write.csv(t_set1235over,"tt_set1235over.csv");
write.csv(t_set1245over,"tt_set1245over.csv");
write.csv(t_set1345over,"tt_set1345over.csv");
write.csv(t_set2345over,"tt_set2345over.csv");
write.csv(t_set1234SMOTE,"tt_set1234SMOTE.csv");
write.csv(t_set1235SMOTE,"tt_set1235SMOTE.csv");
write.csv(t_set1245SMOTE,"tt_set1245SMOTE.csv");
write.csv(t_set1345SMOTE,"tt_set1345SMOTE.csv");
write.csv(t_set2345SMOTE,"tt_set2345SMOTE.csv");