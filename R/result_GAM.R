#' @export
result_GAM=function(temp,dist,metrics,Sp_Name,tab=F){
  tempdata=NULL
  for (j in metrics) {
    for (i in sub("^", "w",dist)) {
      form=as.formula(paste(Sp_Name,"~",i))
      templm=lm(form,data=temp[temp$Metric==j,])
      #radj=summary(templm)$adj.r.squared
      radj=summary(gam(form,family = poisson(),data = temp[temp$Metric==j,]))$r.sq
      #color=if(summary(templm)$coefficients[,4]<=0.05){"Significant"}else{"non significant"}
      tempdata=rbind(tempdata,c(j,sub("w", "", i),radj))
    }
  }
  tempdata=as.data.frame(tempdata)
  colnames(tempdata)=c("Metric","Distance","radj")
  tempdata$Distance=as.character(tempdata$Distance)
  tempdata$Distance=as.numeric(tempdata$Distance)
  tempdata$radj=as.character(tempdata$radj)
  tempdata$radj=as.numeric(tempdata$radj)
  #tempdata$color=as.factor(tempdata$color)
  if(tab==T){
    return(tempdata)
  }
  else{
    
    listplot=list()
    # myColors <- gg_color_hue(2)
    # names(myColors) <- levels(tempdata$color)
    # colScale <- scale_colour_manual(name = "color",values = myColors)
    for (i in metrics) {
      p1=ggplot(tempdata[tempdata$Metric==i,],aes(x = Distance,y=radj))+
        geom_point()+
        ggtitle(i) +
        xlab("Distance") + ylab("Adjusted-R")
      # p1=p1+colScale
      listplot[[i]]=p1
    }
    return(listplot)
  }
  
  
}