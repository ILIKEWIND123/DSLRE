#In your test data, the first column should be the measured yield, column name Y, and the subsequent columns should be the reflectance of the bands at different wavelengths. The column names are the wavelengths

traindata<- read.table("clipboard",header=TRUE,sep='')#Input traindata data

testdata<- read.table("clipboard",header=TRUE,sep='')#Input test data

#Constructing a function for DSLRE model
DSLRE<-function(traindata,testdata,sample_rate_col,cycletime){
  
  set.seed(10)
  
  pre_matrix<-matrix(ncol=cycletime,nrow=nrow(testdata))
  
  for (i in 1:cycletime) {
    
    sample_index_col<-sample(2:ncol(traindata),floor(sample_rate_col*ncol(traindata)))
    
    sample_index_col<-append(1,sample_index_col)
    
    sample_index_row<-sample(1:nrow(traindata), replace = TRUE)
    
    sample_data<-traindata[sample_index_row,sample_index_col]
    
    model<-lm(Y~.,data=sample_data)
    
    sample_data_te<-testdata[,sample_index_col]
    
    pre_matrix[,i]<-predict(model,newdata=sample_data_te)
    
  }
  
  outpre_matrix<-cbind(testdata$Y,pre_matrix)
  
  means<-rowMeans(pre_matrix)
  
  out<-c()
  
  pre_matrix[pre_matrix < 0] <- 0
  
  meanp<-matrix(ncol=1,nrow=nrow(testdata))
  
  for (j in 1:nrow(testdata)) {
    
    inid<-which(pre_matrix[j,]==0,arr.ind=T)
    
    if(length(inid)>0){
      
      meanp[j,1]<-median(pre_matrix[j,-inid])
      
    }else{
      
      meanp[j,1]<-median(pre_matrix[j,])
      
    }
    
  }
  
  results<-list(final_pre=meanp,pre_matrix=pre_matrix,outpre_matrix=outpre_matrix)
  
  return(results)
  
}

PRE<-DSLRE(traindata=traindata,testdata=testdata,sample_rate_col=0.005,cycletime=500)$final_pre #Predictions on the test set
