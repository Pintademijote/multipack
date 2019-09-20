#' @export
gam_select=function(rsq,threshold_rGAM,theme=c("classic","work")){
  theme=match.arg(theme)
  plots_gam=list()
  rsq$radj_GAM=NA
  temp=NULL
  list_select=NULL
  rsq_select=NULL
  for (i in metrics) {
    if(nrow(rsq[rsq$Metric==i,])<=9){
      plots_gam[[i]]=ggplot() + theme_void()
      # list_select=c(list_select,FALSE)
      next}
    b=gam(radj~s(Distance),data = rsq[rsq$Metric==i,], method = "REML")
    rsq[rsq$Metric==i,4]=predict(b)
    if(summary(b)$r.sq>=threshold_rGAM){col_line="blue"}else{col_line="red"}
    ggam=visreg(b,"Distance", gg=TRUE,band=F,line=list(col=col_line))
    ggam=ggam+ggtitle(i)
    col_dots=rep("transparent",length(rsq[rsq$Metric==i,4]))
    col_dots[max_find(rsq[rsq$Metric==i,4])]="black"
    size_dots=rep(0.85,length(rsq[rsq$Metric==i,4]))
    size_dots[max_find(rsq[rsq$Metric==i,4])]=1.7
    shape_dots=rep(16,length(rsq[rsq$Metric==i,4]))
    shape_dots[max_find(rsq[rsq$Metric==i,4])]=15
    ggam=ggam+geom_point(data=rsq[rsq$Metric==i,],aes(x=Distance,y=radj_GAM),size=size_dots,col=col_dots,
                         shape=shape_dots)+annotate("text", label = round(summary(b)$r.sq,4),
                                                    x = max(rsq[rsq$Metric==i,"Distance"]),
                                                    y = max(rsq[rsq$Metric==i,"radj"]),hjust=1)
    temp_rsq=rsq[rsq$Metric==i,]
    rsq_select=rbind(rsq_select,temp_rsq[max_find(rsq[rsq$Metric==i,4]),])

    if(summary(b)$r.sq>=threshold_rGAM){
      keep=rsq[rsq$Metric==i,]
      keep=keep[max_find(keep[keep$Metric==i,4]),]
      list_select=c(list_select,paste0("w",keep$Distance,"_",i))
    }else{
      list_select=c(list_select,paste0("w",seq_keep,"_",i))
    }

    if(theme=="classic"){
      ggam=ggam+theme_classic()
    }


    plots_gam[[i]]=ggam

    # temp=rbind(temp,data.frame(radj=pred,Distance=rsq[rsq$Metric==i,2],Metric=i))
  }
  return(list(select=list_select,rsq=rsq_select,plots=plots_gam))
}
