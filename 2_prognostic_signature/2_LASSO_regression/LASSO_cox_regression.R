#

library("survival")
library("glmnet")

data = read.table("./ESCA_PCD_LASSO_data.txt", header=T, row.names=1, sep="\t")
x = as.matrix(data[, 3:ncol(data)])
y = data.matrix(Surv(data$time,data$status))
fit = glmnet(x, y, family="cox", alpha=1)
plot(fit,xvar="lambda",label=T)
cv_fit = cv.glmnet(x, y, family="cox", alpha=1,nfolds=10)
plot(cv_fit)
result = coef(cv_fit, s="lambda.min")
my_result = as.data.frame(as.matrix(result))
colnames(my_result) = c("coef")
final_result = subset(my_result, my_result$coef>0)
write.table(final_result, "./LASSO_result_coefficient.txt", row.names=T, col.names=T, sep="\t", quote=F)

