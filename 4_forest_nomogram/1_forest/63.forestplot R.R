library(survival)
data = read.table("./forest_data.txt", header=T, sep="\t", check.names=F, row.names=1)
#
uniTab=data.frame()
for(i in colnames(data[,3:ncol(data)])){
  cox <- coxph(Surv(time, status)~data[,i], data=data)
  coxSummary = summary(cox)
  HR=round(coxSummary$conf.int[,"exp(coef)"], 3)
  HR.95L=round(coxSummary$conf.int[,"lower .95"], 3)
  HR.95H=round(coxSummary$conf.int[,"upper .95"], 3)
  pvalue=signif(coxSummary$coefficients[,"Pr(>|z|)"], 4)
  interval=paste0(HR, "(", HR.95L, "-", HR.95H, ")")

  uniTab=rbind(uniTab,
               cbind(id=i,
                     HR=HR,
                     HR.95L=HR.95L,
                     HR.95H=HR.95H,
                     pvalue=pvalue,
                     interval=interval)
  )
}
write.table(uniTab, file="./uniCox.txt", sep="\t", row.names=F, quote=F)

###########################################################################################################
#
multiCox=coxph(Surv(time, status)~., data=data)
multiCoxSum=summary(multiCox)
multiTab=data.frame()
multiTab=cbind(
  HR=multiCoxSum$conf.int[,"exp(coef)"],
  HR.95L=multiCoxSum$conf.int[,"lower .95"],
  HR.95H=multiCoxSum$conf.int[,"upper .95"],
  pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"],
  interval=paste0(HR, "(", HR.95L, "-", HR.95H, ")")
)
multiTab=cbind(id=row.names(multiTab),multiTab)
write.table(multiTab,file="./multiCox.txt",sep="\t",row.names=F,quote=F)

############################################################################################################
#
head(data)
# id	HR	HR.95L	HR.95H	pvalue	interval
# BACH1	0.77	0.664	0.893	0.0005223	0.77(0.664-0.893)
# CEBPB	0.764	0.642	0.909	0.00236	0.764(0.642-0.909)
# CREBZF	0.773	0.65	0.92	0.003693	0.773(0.65-0.92)
# EGR3	0.847	0.762	0.941	0.002024	0.847(0.762-0.941)
# ELK3	0.806	0.694	0.937	0.004885	0.806(0.694-0.937)
# IRF1	0.762	0.688	0.844	1.819e-07	0.762(0.688-0.844)
#############################################################################################################
data = read.table("./uniCox.txt", header=T, sep="\t")
forestplot(data[,c(1,6,5)],                      #
	mean=data[,2],                               #
	lower=data[,3],                              #
	upper=data[,4],                              #
	zero=1,                                        #
	boxsize=0.3,                                   #
	graph.pos=4,                                   #
	col=fpColors(box='red', lines='black', zero="black", hrz_lines="black"),
	lwd.zero=2, lwd.ci=2, lwd.xaxis=2,
	xticks=c(1,2,3,4,5,6,7,8),
	txt_gp=fpTxtGp(label=gpar(cex=1.2), ticks=gpar(cex=1))
)






