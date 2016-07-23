
#####validation######
case=read.csv("AngleClosure_ValidationCases.csv",header=TRUE)
control=read.csv("AngleClosure_ValidationControls.csv",header=TRUE)


r=c(case$rAOD750,control$rAOD750)
l=c(case$lAOD750,control$lAOD750)
log=is.na(r)
AOD750=r
AOD750[log]=l[log]

r=c(case$rTISA750,control$rTISA750)
l=c(case$lTISA750,control$lTISA750)
log=is.na(r)
TISA750=r
TISA750[log]=l[log]

r=c(case$rIT750,control$rIT750)
l=c(case$lIT750,control$lIT750)
log=is.na(r)
IT750=r
IT750[log]=l[log]

r=c(case$IT2000,control$rIT2000)
l=c(case$lIT2000,control$lIT2000)
log=is.na(r)
IT2000=r
IT2000[log]=l[log]

r=c(case$rITCM,control$rITCM)
l=c(case$lITCM,control$lITCM)
log=is.na(r)
ITCM=r
ITCM[log]=l[log]


r=c(case$rIAREA,control$rIAREA)
l=c(case$lIAREA,control$lIAREA)
log=is.na(r)
IAREA=r
IAREA[log]=l[log]

r=c(case$rICURV,control$rICURV)
l=c(case$lICURV,control$lICURV)
log=is.na(r)
ICURV=r
ICURV[log]=l[log]

ACA=c(case$ACA,control$ACA)
ACV=c(case$ACV,control$ACV)
LENSVAULT=c(case$LENSVAULT,control$LENSVAULT)
ACW_mm=c(case$ACWmm,control$ACW.mm.)


Val=c(matrix(1,1,dim(case)[1]),matrix(0,1,dim(control)[1]))
MyDataValidation=na.omit(data.frame(Val,AOD750,TISA750,IT750,IT2000,ITCM,IAREA,ICURV,ACW_mm,ACA,ACV,LENSVAULT))






dev.new(width=10,height=10)
par(mai=c(0.3,0.3,0.3,0.3),mfrow=c(3,3))

fit=randomForest(Y~.,data=DataOfficial,mtry=1,n.trees=10)
RF=predict(fit,newdata=MyDataValidation[,-1],"prob")[,2]
roc.plot(MyDataValidation$Val,RF, plot.thres = NULL)
mtext(paste("RF: ","mtry = 1 and Ntrees = 10"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,RF),digits=3)), bty ="n", pch=NA)


fit=nnet(Y~.,data=DataOfficial,size=20,decay=1)
nenet=predict(fit,newdata=MyDataValidation[,-1], probability = TRUE)[,1]
roc.plot(MyDataValidation$Val,nenet, plot.thres = NULL)
mtext(paste("Nnet: ","size = 15 and decay = 1"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,nenet),digits=3)), bty ="n", pch=NA)

fit=ada(Y~.,data=DataOfficial,iter=100,nu=0.2)
ada=predict(fit,newdata=MyDataValidation[,-1], probability = TRUE,"prob")[,2]
roc.plot(MyDataValidation$Val,ada, plot.thres = NULL)
mtext(paste("Ada: ","iter = 50 and NNu = 1"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,ada),digits=3)), bty ="n", pch=NA)

fit=svm(Y~.,data=DataOfficial,cost=1,kernel='linear',probability=TRUE)
svm=attr(predict(fit,newdata=MyDataValidation[,-1],type='response',probability=TRUE),"probabilities")[,1]
roc.plot(MyDataValidation$Val,svm, plot.thres = NULL)
mtext(paste("SVM: ","kernel = linear and cost = 1"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,svm),digits=3)), bty ="n", pch=NA)

fit=glm(Y~.,data=(step(glm(Y~., data=DataOfficial, family=binomial),steps=1)$model), family=binomial)
glm=predict(fit,newdata=MyDataValidation[,-1],type='response')
roc.plot(MyDataValidation$Val,glm, plot.thres = NULL)
mtext(paste("GLM: ","step = 1"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,glm),digits=3)), bty ="n", pch=NA)


predict_mat_val=cbind(RF,nenet,ada,svm,glm)


### unconstrained stack model
predUnconst=predict_mat_val%*%weight
roc.plot(MyDataValidation$Val,predUnconst, plot.thres = NULL)
mtext(paste("Stacked: Unconstrained"),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,predUnconst),digits=3)), bty ="n", pch=NA)


### constrained stack model
predConst=predict_mat_val%*%weightConst
roc.plot(MyDataValidation$Val,RF, plot.thres = NULL)
mtext(paste("Stacked: Constrained "),cex=0.8)
legend("bottomright",legend=paste("AUC = ", round(auc(MyDataValidation$Val,predConst),digits=3)), bty ="n", pch=NA)







