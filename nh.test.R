nh.test<-function(x,group){
NH<-matrix("NA",nrow = length(names(x)[-1]),ncol=3, dimnames = list(names(x)[-1], c("N","H","False")))
library(car)
for (i in names(x)[-1]){
  NH[i,1]<-round(shapiro.test(x[,i])$p.value,9)
  NH[i,2]<-round(leveneTest(x[,i]~x$group)[1,3],9)
  if(round(shapiro.test(x[,i])$p.value,9) < 0.05 | round(leveneTest(x[,i]~x$group)[1,3],9) < 0.05) {
    NH[i,3] <- "*"}
  if(round(shapiro.test(x[,i])$p.value,9) > 0.05 & round(leveneTest(x[,i]~x$group)[1,3],9) > 0.05) {
    NH[i,3] <- " "}
}
write.csv(NH,file ="NH.csv",row.names = TRUE,quote = F)
print(paste("The results file named NH.csv is creared in ",getwd(),". By Leileilei Ding, from Guizhou Grassland Science Institute."))
}