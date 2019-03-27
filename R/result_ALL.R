#' @export
result_ALL=function(temp,dist,metrics,Sp_Name,tab=F){
  tempdata=NULL
  for (j in metrics) {
    for (i in sub("^", "w",dist)) {
      form=as.formula(paste(Sp_Name,"~",i))
      form_gam=as.formula(paste(Sp_Name,"~s(",i,")"))
      temp_metrics=temp[temp$Metric==j,]
      templm=lm(form,data=temp_metrics)
      radj_lm=summary(templm)$adj.r.squared
      color_lm=if(nrow(summary(templm)$coefficients)>1){
        if(summary(templm)$coefficients[2,4]<=0.05){"Significant"}else{"non significant"}
      }else{"Cannot test"}
      if(length(unique(temp_metrics[,i]))<=10){
        radj_gam=0
        color_gam="Cannot test"
      }else{
        tempgam=summary(gam(form_gam,family = poisson(),data = temp_metrics))
        radj_gam=tempgam$r.sq
        color_gam=if(tempgam$s.pv[1]<=0.05){"Significant"}else{"non significant"}
      } 
      
      
      
      radj_mars=summary(earth(form,degree = 1,glm = list(family=poisson()),data = temp_metrics))$rsq
      color_mars="Unknow"
      #color=if(summary(templm)$coefficients[,4]<=0.05){"Significant"}else{"non significant"}
      tempdata=rbind(tempdata,c(j,sub("w", "", i),radj_lm,"lm",color_lm))
      tempdata=rbind(tempdata,c(j,sub("w", "", i),radj_gam,"GAM",color_gam))
      tempdata=rbind(tempdata,c(j,sub("w", "", i),radj_mars,"MARS",color_mars))
      
    }
  }
  tempdata=as.data.frame(tempdata)
  colnames(tempdata)=c("Metric","Distance","radj","type","color")
  tempdata$Distance=as.character(tempdata$Distance)
  tempdata$Distance=as.numeric(tempdata$Distance)
  tempdata$radj=as.character(tempdata$radj)
  tempdata$radj=as.numeric(tempdata$radj)
  tempdata$color=as.factor(tempdata$color)
  if(tab==T){
    return(tempdata)
  }
  else{
    
    listplot=list()
    # myColors <- gg_color_hue(2)
    # names(myColors) <- levels(tempdata$color)
    # colScale <- scale_colour_manual(name = "color",values = myColors)
    for (i in metrics) {
      p1=ggplot(tempdata[tempdata$Metric==i,],aes(x = Distance,y=radj,color=type))+
        geom_point(aes(shape=color),size=2)+
        xlab("Distance") + ylab("Adjusted-R")
      # p1=p1+colScale
      listplot[[i]]=p1
    }
    return(listplot)
  }
  
  
}