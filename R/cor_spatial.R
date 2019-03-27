#' @export
cor_spatial=function(directory,metrics,dist){
  
  
  
  metrics_temp=metrics
  files=list.files(directory,pattern = "\\.asc$")
  metrics=gsub('.', "-", metrics, fixed=TRUE)
  distname=paste("w",dist,"_",sep = "")
  metrics=metrics[rev(order(metrics))]
  
  temp=files[grep(distname[1],files)]
  temp=temp[grep(metrics[1],temp)]
  temp=paste(directory,temp,sep = "")
  r=raster(temp)
  
  columnames=NULL
  for (metric in metrics) {
    columnames=c(columnames,(paste(distname,"_",metric,sep="")))
  }
  
  test=matrix(nrow = length(seq(1,ncol(r),10))*length(seq(1,nrow(r),10)),ncol = length(metrics)*length(distname))
  test=as.data.frame(test)
  colnames(test)=columnames
  
  start_time <- Sys.time()
  pb <- txtProgressBar(min = 0, max = length(distname)*length(metrics), style = 3)
  repet=0
  shin_progress=length(distname)*length(metrics)
  
  for (dist in distname) {
    tempdist=files[grep(dist,files)]
    a=0
    for (metric in metrics) {
      repet=repet+1
      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, repet)
      
      
      temp=tempdist[grep(metric,tempdist)]
      temp=paste(directory,temp,sep = "")
      r=raster(temp)
      
      test[,paste(dist,"_",metric,sep="")]=as.vector(r[seq(1,nrow(r),10),seq(1,ncol(r),10)])   
      
      tempdist=tempdist[-grep(metric,tempdist)]
      
      Sys.sleep(0.1)
      incProgress(1/shin_progress)
      
    }
  }
  end_time <- Sys.time()
  end_time - start_time
  
  metrics=metrics_temp
  columnames=NULL
  for (metric in metrics) {
    columnames=c(columnames,(paste(distname,"_",metric,sep="")))
  }
  colnames(test)=columnames
  return(test)
}