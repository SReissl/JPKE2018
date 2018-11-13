library(PKSFC)
library(beepr)

read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  x = readLines(file)
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}


###To run the model & scenarios
modelAsensio <- sfc.model("modelAsensio.R")
source("asensioInit.R")
asensioInit()
values<-read.tcsv("asensioInit.csv")
for(j in 1:ncol(values)){
  varName<-colnames(values[j])
  Init<-values[1,j]
  modelAsensio<-sfc.editVar(modelAsensio,var=varName,init=Init)
}
modelAsensio<-sfc.addScenario(model=modelAsensio,vars=list(c("g")),values=list(c(205)),inits=50,ends=500)
modelAsensio<-sfc.addScenario(model=modelAsensio,vars=list(c("gamma1")),values=list(c(1.5)),inits=50,ends=500)
modelAsensio<-sfc.addScenario(model=modelAsensio,vars=list(c("lambda10")),values=list(c(0.27)),inits=50,ends=500)
modelAsensio<-sfc.addScenario(model=modelAsensio,vars=list(c("lambda20")),values=list(c(0.425)),inits=50,ends=500)
modelAsensio<-sfc.addScenario(model=modelAsensio,vars=list(c("r_h")),values=list(c(0.0055)),inits=50,ends=500)
modelAsensio<-simulate(modelAsensio,maxIter = 1000)
dataAsensio<-as.data.frame(modelAsensio$baseline)
dataAsensio1<-as.data.frame(modelAsensio$scenario_1)
dataAsensio2<-as.data.frame(modelAsensio$scenario_2)
dataAsensio3<-as.data.frame(modelAsensio$scenario_3)
dataAsensio4<-as.data.frame(modelAsensio$scenario_4)
dataAsensio5<-as.data.frame(modelAsensio$scenario_5)
beep(2)



###reproduce paper graphs

par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4,5,6), 2, 3))
par(oma=c(0,0,2,0))

temp<-dataAsensio1[1:250,c("r_cp","r_l")]
matplot(rownames(temp),temp[c("r_cp","r_l")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(0.01935,0.02035),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("r_cp","r_l"),ncol=2, cex=2,text.width=c(15,110))


temp<-dataAsensio1[1:250,c("YD")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1235.5,1285),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("YD"),cex=2,text.width=c(15))

temp<-dataAsensio1[1:250,c("l","cp")]
matplot(rownames(temp),temp[c("l","cp")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(712.5,745),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("l","cp"),ncol=2, cex=2,text.width=c(0,100))

temp<-dataAsensio1[1:250,c("INV","INV_t")]
matplot(rownames(temp),temp[c("INV","INV_t")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1425,1487.5),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("INV","INV_t"),ncol=2, cex=2,text.width=c(0,100))


temp<-dataAsensio1[1:250,c("gb")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1690,1750),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("GB"),cex=2,text.width=c(15))


temp<-dataAsensio1[1:250,c("V_h")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(3095,3195),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("V_h"),cex=2,text.width=c(15))

mtext("Figure 1: Effect of an increase in government expenditure", outer=TRUE, cex = 1.5)




##########################à
temp<-dataAsensio2[1:250,c("r_cp","r_l")]
matplot(rownames(temp),temp[c("r_cp","r_l")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(0.0195,0.023),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("r_cp","r_l"),ncol=2, cex=2,text.width=c(15,110))

temp<-dataAsensio2[1:250,c("YD")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1230.5,1270),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("YD"),cex=2,text.width=c(15))

temp<-dataAsensio2[1:250,c("l","cp")]
matplot(rownames(temp),temp[c("l","cp")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(705,770),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("l","cp"),ncol=2, cex=2,text.width=c(0,100))

temp<-dataAsensio2[1:250,c("INV","INV_t")]
matplot(rownames(temp),temp[c("INV","INV_t")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1427.5,1505),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("INV","INV_t"),ncol=2, cex=2,text.width=c(0,100))


temp<-dataAsensio2[1:250,c("gb")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1635,1705),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("GB"),cex=2,text.width=c(15))


temp<-dataAsensio2[1:250,c("V_h")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(3090,3150),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("V_h"),cex=2,text.width=c(15))
mtext("Figure 2: Effect of an increase in the target inventory-to-sales ratio", outer=TRUE, cex = 1.5)




#################################
temp<-dataAsensio3[1:250,c("r_cp","r_l")]
matplot(rownames(temp),temp[c("r_cp","r_l")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(0.0190,0.0245),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("r_cp","r_l"),ncol=2, cex=2,text.width=c(15,110))


temp<-dataAsensio3[1:250,c("YD")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1238.5,1243.5),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("YD"),cex=2,text.width=c(15))

temp<-dataAsensio3[1:250,c("l","cp")]
matplot(rownames(temp),temp[c("l","cp")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(685,735),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("l","cp"),ncol=2, cex=2,text.width=c(15,110))

temp<-dataAsensio3[1:250,c("INV","INV_t")]
matplot(rownames(temp),temp[c("INV","INV_t")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1422.5,1442.5),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("INV","INV_t"),ncol=2, cex=2,text.width=c(15,110))


temp<-dataAsensio3[1:250,c("gb")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(1692.5,1707.5),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("GB"),cex=2,text.width=c(15))


temp<-dataAsensio3[1:250,c("V_h")]
matplot(temp,col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(3097.5,3108),cex.axis=2)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("V_h"),cex=2,text.width=c(15))
mtext("Figure 3: Effect of an increase in the demand for commercial paper", outer=TRUE, cex = 1.5)




######################
layout(matrix(c(1,2,3,4,5,6), 1, 2))
par(oma=c(0,0,2,0))

temp<-dataAsensio4[1:250,c("r_cp","r_l")]
matplot(rownames(temp),temp[c("r_cp","r_l")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(0.0199,0.0201))
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("r_cp","r_l"))

temp<-dataAsensio4[1:250,c("M","H")]
matplot(rownames(temp),temp[c("M","H")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(550,1425))
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:2,lwd=2,bty='n',legend=c("M","H"))
mtext("Figure 4: Effect of an exogenous increase in central bank treasury bill holdings", outer=TRUE, cex = 1.5)



################
par(mfrow=c(1,1))
par(oma=c(0,0,2,0))
temp<-dataAsensio5[1:250,c("r_cp","r_l","r_h")]
matplot(rownames(temp),temp[c("r_cp","r_l")],col=c(1),type='l',lty=1:2,lwd=2,xlab="",ylab="",ylim=c(0.0199,0.0206),cex.axis=1.1)
par(new=TRUE)
matplot(rownames(temp),temp[c("r_h")],col=1,type='l',lty=3,lwd=3,xlab="",ylab="",yaxt="n",ylim=c(0.0049,0.0056),cex.axis=1.1)
axis(4,cex.axis=1.1)
abline(h=0,col=1)
grid()
legend("bottom",col=c(1),lty=1:3,lwd=2,bty='n',legend=c("r_cp","r_l","r_h (rhs)"),cex=1.2)
mtext("Figure 5: Effect of an increase in the interest rate on reserves", outer=TRUE, cex = 1.5)

