#' @export
formodel=function(chloe,dist,metrics){
  metrics=metrics[rev(order(metrics))]
  
  temp=NULL
  for (i in metrics) {
    test=chloe[,c(1,2,3,grep(i, colnames(chloe)))]
    
    colnames(test)[4:ncol(test)]=gsub(paste("_",i,sep = ""), "", colnames(test)[4:ncol(test)])
    colnames(test)[4:ncol(test)]=gsub("w", "", colnames(test)[4:ncol(test)])
    test=cbind(test[,c(1,2,3)],test[,as.character(sort(as.integer(colnames(test)[4:ncol(test)])))])
    colnames(test)[4:ncol(test)]=sub("^", "w", colnames(test)[4:ncol(test)])
    test$Metric=i
    temp=rbind(temp,test)
    chloe=chloe[,-grep(i, colnames(chloe))]
  }
  temp$Metric=as.factor(temp$Metric)
  
  return(temp)
  
}