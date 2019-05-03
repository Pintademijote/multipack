#' @export

full_mod=function(Varia_paysage_model,dist,metrics,Sp_Name,tab=F){
tempdata=NULL
for (j in metrics) {
  for (i in sub("^", "w",dist)) {
    form=as.formula(paste(Sp_Name,"~",i))
    templm=lm(form,data=Varia_paysage_model[Varia_paysage_model$Metric==j,])
    #radj=summary(templm)$adj.r.squared
    radj=summary(earth(form,degree = 1,glm = list(family=poisson()),data = Varia_paysage_model[Varia_paysage_model$Metric==j,]))$rsq

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


res=Varia_paysage_model[Varia_paysage_model$Metric==metrics[1],c("id",Sp_Name),drop=FALSE]
res_list=list()
rep=0

for (i in metrics) {
  rep=rep+1
  temp=tempdata[tempdata$Metric==i,]
  temp=as.matrix(temp[which.max(temp[,3]),c(1,2)])
  dist=temp[2]
  temp=Varia_paysage_model[Varia_paysage_model$Metric==i,c("id",paste0("w",temp[2]))]
  temp=temp[!duplicated(temp$id),]
  colnames(temp)[2]=paste0(i,"_",colnames(temp)[2])
  res=merge(res,temp,by.x="id",by.y="id")
  res_list[[rep]]=c(i,dist)
}

res_merge=list(res,res_list)

return(res_merge)

}
