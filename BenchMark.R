
library(e1071)
library(pROC)
library(randomForest)
#install.packages("nnet")
library(nnet)
library(ada)
library(quadprog)
library(verification)
allData = read.csv("AngleClosure.csv",header=TRUE,na.strings=".")

colToRemove = sapply(attributes(allData)$names,function(name){
  if (name %in% c("EYE","GENDER","ETHNIC","HGT","WT","ASPH","ACYL","SE","AXL","CACD","AGE","CCT.OD","PCCURV_mm")){
    return(FALSE)
  } else {
    return(TRUE)
  }
})
DataWOcol = allData[,colToRemove]
#DataWOcol = data.matrix(allData[,colToRemove], rownames.force = NA)
#log1 = apply(DataWOcol, 2, as.numeric)

log = is.na(DataWOcol)

RowsToRemove = apply(log,1,any)
Data = DataWOcol[!RowsToRemove,]
Y = Data$ANGLE.CLOSURE
X = as.matrix(Data[ , !(names(Data) %in% "ANGLE.CLOSURE")])
DataOfficial=data.frame(Y,X)


# 
# # SVM
# 
# model <- svm(Y~X,probability=TRUE)
# 
# predictedY <- predict(model, X)
# predictedY1 = attr(predict(model,X,probability=TRUE),
#      "probabilities")[,1]
# #points(X, as.matrix(predictedY), col = "red", pch=4)
# 
# #auc(as.matrix(predictedY),X)
# #rocfit=roc(as.numeric(Y=="YES"),as.numeric(predictedY=="YES"))
# rocfit=roc(as.numeric(Y=="YES"),predictedY1)
# 
# 
# N_fold=10
# 
# #random forest\
# #Create 10 equally size folds
# AllDa <- AllDa[sample(nrow(AllDa)),]
# folds <- cut(seq(1,nrow(AllDa)),breaks=N_fold,labels=FALSE)
# 
# #Perform 10 fold cross validation
# AUC_vec=NULL
# xy=NULL
# AUC_RF_plot=list()
# mtoTry=c(1,5,10)
# ntreesToTry=c(500,1000,1500)
# AUCrange=NULL
# for (m in mtoTry){
#   xy=NULL
#   for (ntrees in ntreesToTry){
#     for(i in 1:2){
#       #Segement your data by fold using the which() function 
#       testIndexes <- which(folds==i,arr.ind=TRUE)
#       testData <- AllDa[testIndexes, ]
#       trainData <- AllDa[-testIndexes, ]
#       print(c(m,ntrees,i))
#       fit=randomForest(Y~.,data=trainData,mtry=m,n.trees=ntrees)
#       myPreds=predict(fit,newdata=testData[,-1], probability = TRUE)
#       AUC_vec= c(AUC_vec,auc(roc(as.numeric(testData[,1]),as.numeric(myPreds))))
#       #Use the test and train data partitions however you desire...
#     }
#     AUC_RF=mean(AUC_vec)
#     xy= cbind(xy,c(ntrees,AUC_RF))
#     AUCrange=c(AUCrange,AUC_RF)
#   }
#  
#   AUC_RF_plot[[which(m==mtoTry)]] = xy
#   
# }
# dev.off()
# layout(rbind(1,2), heights=c(7,1))
# plot(range(ntreesToTry), range(AUCrange), type="n", xlab="N trees",
#      ylab="AUC" )
# linetype <- c(1:length(mtoTry)) 
# colors <- rainbow(length(mtoTry)) 
# # add a title and subtitle 
# title("AUC for Random Forest")
# 
# 
# for (j in 1:length(mtoTry)){
#   x=AUC_RF_plot[[j]][1,]
#   y = AUC_RF_plot[[j]][2,]
#   lines(AUC_RF_plot[[j]][1,], AUC_RF_plot[[j]][2,], type="b", lwd=1.5,
#         lty=linetype[j], col=colors[j])
# }
# 
# # add a legend 
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend('center','groups', paste("m = ",mtoTry), cex=0.8, col=colors, lty=linetype, title="Legend",ncol=3)
# 
# 
# # second plot  EDIT: needs to have same ylim
# par(new = TRUE)
# plot(x, y2, ylim=range(c(y1,y2)), axes = FALSE, xlab = "", ylab = "")
# 
# 
# 
# N_fold=10
# 
# #random forest\
# #Create 10 equally size folds
# AllDa <- AllDa[sample(nrow(AllDa)),]
# folds <- cut(seq(1,nrow(AllDa)),breaks=N_fold,labels=FALSE)
# 
# #Perform 10 fold cross validation
# AUC_vec=matrix(NA,N_fold,1)
# for(i in 1:N_fold){
#   #Segement your data by fold using the which() function 
#   #testIndexes=sample(length(Y))[1:round(length(Y)/nFolds)]
#   testIndexes <- which(folds==i,arr.ind=TRUE)
#   testData <- AllDa[testIndexes, ]
#   trainData <- AllDa[-testIndexes, ]
#   
#   fit=nnet(Y~.,data=trainData,size=10,lambda=10)
#   myPreds=predict(fit,newdata=testData[,-1])
#   AUC_vec[i]= auc(as.numeric(testData[,1]),myPreds)
#   #Use the test and train data partitions however you desire...
# }
# AUC_nnet=mean(AUC_vec)
# 
# #neuralnet
# fit=nnet(Y~.,data=X,size=10)
# 
# myPreds=predict(fit,newdata=X, probability = TRUE)
# 
# 
# plot(x, y1, ylim=range(c(y1,y2)))
# 
# # second plot  EDIT: needs to have same ylim
# par(new = TRUE)
# plot(x, y2, ylim=range(c(y1,y2)), axes = FALSE, xlab = "", ylab = "")
# 
# 
# 
# 
# 
# 
# 










frameworkTuning<-function(AllDat,N_fold,param2,param1,modelName,axeName2,legendName1){
  #Create 10 equally size folds
  AllDa <- AllDat[sample(nrow(AllDat)),]
  folds <- cut(seq(1,nrow(AllDa)),breaks=N_fold,labels=FALSE)
  #Perform 10 fold cross validation
  AUC_vec=NULL
  xy=NULL
  AUC_RF_plot=list()
  #param1=c(1,5,10)
  #param2=c(500,1000,1500)
  AUCrange=NULL
  dev.new(width=10,height=10)
  par(mai=c(0.3,0.3,0.3,0.3),mfrow=c(length(param1),length(param2)))
  for (a in param1){
    xy=NULL
    for (b in param2){
      Act=NULL
      pre=NULL
      for(i in 1:N_fold){
        testIndexes <- which(folds==i,arr.ind=TRUE)
        testData <- AllDa[testIndexes, ]
        trainData <- AllDa[-testIndexes, ]
        print(c(a,b,i))
        if (modelName =="RF"){fit=randomForest(Y~.,data=trainData,mtry=b,n.trees=a)} 
        else if(modelName =="nnet"){fit=nnet(Y~.,data=trainData,size=b,decay=a)}
        else if(modelName =="ada"){fit=ada(Y~.,data=trainData,iter=a,nu=b)}
        else if(modelName =="svm"){fit=svm(Y~.,data=trainData,cost=b,kernel=a,probability=TRUE)} #
        else if(modelName =="glm"){fit=glm(Y~.,data=(step(glm(Y~., data=trainData, family=binomial),steps=b)$model), family=binomial)}
        
        if(modelName =="glm"){myPreds=predict(fit,newdata=testData[,-1],type='response')}
        else if(modelName =="svm"){myPreds=attr(predict(fit,newdata=testData[,-1],type='response',probability=TRUE),"probabilities")[,1]} 
        else if (modelName =="ada" || modelName =="RF"){myPreds=predict(fit,newdata=testData[,-1], probability = TRUE,"prob")[,2]} 
        else if (modelName =="nnet"){myPreds=predict(fit,newdata=testData[,-1], probability = TRUE)[,1]}
        AUC_vec= c(AUC_vec,auc(roc(as.numeric(testData[,1]),as.numeric(myPreds))))
        
        
        Act=c(Act,as.numeric(testData[,1])-1)
        pre=c(pre,as.numeric(myPreds))
      }
      AUC_RF=mean(AUC_vec)
      roc.plot(Act,pre,, plot.thres = NULL)
      mtext(paste(legendName1," = ",a, " and ",axeName2," = ",b ),cex=0.8)
      legend("bottomright",legend=paste("AUC = ", round(AUC_RF,digits=3)), bty ="n", pch=NA)
      
      xy= cbind(xy,c(b,AUC_RF))
      AUCrange=c(AUCrange,AUC_RF)
    }
    AUC_RF_plot[[which(a==param1)]] = xy
  }
  dev.new(width=10,height=10)
  layout(rbind(1,2), heights=c(7,1))
  plot(range(param2), range(AUCrange), type="n", xlab=axeName2,
       ylab="AUC" )
  # linetype <- c(1:length(param1)) 
  colors <- rainbow(length(param1)) 
  # add a title and subtitle 
  title(paste("AUC for ",modelName))
  #,lty=linetype[j]
  for (j in 1:length(param1)){lines(AUC_RF_plot[[j]][1,], AUC_RF_plot[[j]][2,], type="b", lwd=1.5, col=colors[j])}
  par(mar=c(0, 0, 0, 0))
  plot.new()
  #, lty=linetype
  legend('center','groups', paste(legendName1,paste(" = ",param1)), cex=0.8, col=colors, lty=1, title="Legend",ncol=3)
}

frameworkTuning(DataOfficial,2,c(1,2),c(10,20),"RF","mtry","Ntrees")
frameworkTuning(DataOfficial,10,c(1,3,7,10),c(10,500,1000,5000),"RF","mtry","Ntrees") #OK!! m=1, ntrees=10

frameworkTuning(DataOfficial,10,c(5,10,15,20),c(10^-7,10^-2,1,2),"nnet","size","decay")

frameworkTuning(DataOfficial,10,c(10^-7,10^-2,10^-1,0.2,0.5),c(50,100,200),"ada","nu","iter")
frameworkTuning(DataOfficial,10,c(0.01,1,100),c(10^-6,10^-1,2),"svm","Cost","Gamma")
frameworkTuning(DataOfficial,2,c(0.01,1,100,1000),c("linear","radial","polynomial"),"svm","Cost","kernel")
frameworkTuning(DataOfficial,10,c(1,10,30,50),c("linear"),"svm","Cost","kernel")
frameworkTuning(DataOfficial,10,c(1,2,4,5,6),c(1),"glm","step","")

myCosts = 10^(-2:2)
myGamma = 10^seq(-6,-1,0.5)



dev.new(width=10,height=10)
par(mai=c(0.3,0.3,0.3,0.3),mfrow=c(4,4))

plot(c(1,1),c(2,2))
plot(c(1,1),c(2,2))
plot(c(1,1),c(2,2))



N_fold=10
AllDa <- DataOfficial[sample(nrow(DataOfficial)),]
folds <- cut(seq(1,nrow(AllDa)),breaks=N_fold,labels=FALSE)
resultSTacked=NULL
Actual=NULL
for(i in 1:N_fold){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- AllDa[testIndexes, ]
  trainData <- AllDa[-testIndexes, ]
  print(c(a,b,i))
  fit=randomForest(Y~.,data=trainData,mtry=1,n.trees=10)
  RF=predict(fit,newdata=testData[,-1],"prob")[,2]
  fit=nnet(Y~.,data=trainData,size=15,decay=1)
  nenet=predict(fit,newdata=testData[,-1], probability = TRUE)[,1]
  fit=ada(Y~.,data=trainData,iter=100,nu=0.2)
  ada=predict(fit,newdata=testData[,-1], probability = TRUE,"prob")[,2]
  fit=svm(Y~.,data=trainData,cost=1,kernel="linear",probability=TRUE)
  svm=attr(predict(fit,newdata=testData[,-1],type='response',probability=TRUE),"probabilities")[,1]
  fit=glm(Y~.,data=(step(glm(Y~., data=trainData, family=binomial),steps=1)$model), family=binomial)
  glm=predict(fit,newdata=testData[,-1],type='response')
  
  predict_mat=cbind(RF,nenet,ada,svm,glm)
  
  
  resultSTacked=rbind(resultSTacked,predict_mat)
  
  Actual = c(Actual,as.numeric(testData[,1]=="YES"))
}

#unconstrained stacking
weight=solve((t(resultSTacked)%*%resultSTacked))%*%t(resultSTacked)%*%Actual
pred=resultSTacked%*%weight
auc(Actual,pred)

#constrained stacking

constraints=t(rbind(c(1,1,1,1,1),diag(1,5,5)))

weightConst = solve.QP( t(resultSTacked) %*% resultSTacked, t(Actual) %*% resultSTacked, constraints, c(1,0,0,0,0,0),meq=1)$solution
predConst=resultSTacked%*%weightConst



