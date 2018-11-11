total = rbind(set1,set2,set3,set4,set5);
total = rbind(set1234SMOTE,set1235SMOTE,set1245SMOTE,set1345SMOTE,set2345SMOTE)

total$Month = as.numeric(total$Month);
total$Month[is.element(total$Month,c(12,1,2))] = 100; # winter
total$Month[is.element(total$Month,c(3,4,5))] = 101; # spring
total$Month[is.element(total$Month,c(6,7,8))] = 102; # summer
total$Month[is.element(total$Month,c(9,10,11))] = 103; # fall

cs = table(total$Severity,total$Urban);
chi = chisq.test(cs)
chi$p.value
csf = cs[1,]/cs[2,];
csf;

var = names(total)
plist = c()
for (i in var){
  cs = table(total$Severity, total[,i])
  chi = chisq.test(cs)
  p = chi$p.value
  plist = c(plist,p)
}