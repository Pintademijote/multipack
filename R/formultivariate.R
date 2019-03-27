#' @export
formultivariate=function(chloe,dist,metrics){
  metrics=metrics[rev(order(metrics))]
  
  temp=chloe[,c(1,2,3,grep(metrics[1], colnames(chloe)))]
  colnames(temp)[4:ncol(temp)]=gsub(paste("_",metrics[1],sep = ""), "", colnames(temp)[4:ncol(temp)])
  colnames(temp)[4:ncol(temp)]=gsub("w", "", colnames(temp)[4:ncol(temp)])
  temp=cbind(temp[,c(1,2,3)],temp[,as.character(sort(as.integer(colnames(temp)[4:ncol(temp)])))])
  colnames(temp)[4:ncol(temp)]=sub("^", "w", colnames(temp)[4:ncol(temp)])
  colnames(temp)[4:ncol(temp)]=paste(colnames(temp)[4:ncol(temp)],"_",metrics[1],sep="")
  chloe=chloe[,-grep(metrics[1], colnames(chloe))]
  
  for (i in metrics[2:length(metrics)]) {
    test=chloe[,c(1,grep(i, colnames(chloe)))]
    colnames(test)[2:ncol(test)]=gsub(paste("_",i,sep = ""), "", colnames(test)[2:ncol(test)])
    colnames(test)[2:ncol(test)]=gsub("w", "", colnames(test)[2:ncol(test)])
    test=cbind(test[,"id"],test[,as.character(sort(as.integer(colnames(test)[2:ncol(test)])))])
    colnames(test)[2:ncol(test)]=sub("^", "w", colnames(test)[2:ncol(test)])
    colnames(test)[2:ncol(test)]=paste(colnames(test)[2:ncol(test)],"_",i,sep="")
    colnames(test)[1]="id"
    temp=merge(temp,test,by.x = "id", by.y="id")
    chloe=chloe[,-grep(i, colnames(chloe))]
    
  }
  return(temp)
}