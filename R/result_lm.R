#' @export
result_lm=function(temp,dist,metrics,Sp_Name,tab=T,plots=T,individual_plots=F){
  tempdata=NULL
  for (j in metrics) {
    for (i in sub("^", "w",dist)) {
      form=as.formula(paste(Sp_Name,"~",i))
      templm=lm(form,data=temp[temp$Metric==j,])
      radj=summary(templm)$adj.r.squared
      corres=cor.test(temp[temp$Metric==j,Sp_Name],temp[temp$Metric==j,i])
      color=if(summary(templm)$coefficients[,4]<=0.05){"Significant"}else{"non significant"}
      tempdata=rbind(tempdata,c(j,sub("w", "", i),radj,corres$estimate,corres$p.value,color))
    }
  }
  tempdata=as.data.frame(tempdata)
  colnames(tempdata)=c("Metric","Distance","radj","CoefCor","pvalueCor","color")
  tempdata$Distance=as.character(tempdata$Distance)
  tempdata$Distance=as.numeric(tempdata$Distance)
  tempdata$radj=as.character(tempdata$radj)
  tempdata$radj=as.numeric(tempdata$radj)
  tempdata$CoefCor=as.character(tempdata$CoefCor)
  tempdata$CoefCor=as.numeric(tempdata$CoefCor)
  tempdata$pvalueCor=as.character(tempdata$pvalueCor)
  tempdata$pvalueCor=as.numeric(tempdata$pvalueCor)
  tempdata$color=as.factor(tempdata$color)
  if(plots==T){

    p1=ggplot(tempdata,aes(x = Distance,y=radj,color=Metric))+
      geom_point()
    p2=ggplot(tempdata,aes(x = Distance,y=CoefCor,color=Metric))+
      geom_point()
    p3=ggplot(tempdata,aes(x = Distance,y=pvalueCor,color=Metric))+
      geom_point()
    print(p1)
    print(p2)
    print(p3)
  }
  if(individual_plots==T){
    listplot=list()
    myColors <- gg_color_hue(2)
    names(myColors) <- levels(tempdata$color)
    colScale <- scale_colour_manual(name = "color",values = myColors)
    for (i in metrics) {
      p1=ggplot(tempdata[tempdata$Metric==i,],aes(x = Distance,y=radj,color=color))+
        geom_point()+
        ggtitle(i) +
        xlab("Distance") + ylab("R")
      p1=p1+colScale
      listplot[[i]]=p1
    }
    return(listplot)
  }
  if(tab==T){
    return(tempdata)
  }
}
