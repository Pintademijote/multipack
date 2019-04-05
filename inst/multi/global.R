library(shiny)
library(rgdal)
library(raster)
library(leaflet)
library(DT)
library(shinyFiles)
library(plotly)
library(shinycssloaders)
library(earth)
library(mgcv)
library(FactoMineR)
library(caret)


RV_COR_shiny=function(Varia_paysage_multi,metrics,dist){
  coefRV=matrix(ncol=length(metrics),nrow=length(metrics))
  pvalue=matrix(ncol=length(metrics),nrow=length(metrics))

  seq_group=seq(4,ncol(Varia_paysage_multi)+1,length(dist))

  pb <- txtProgressBar(min = 0, max = length(seq_group), style = 3)
  shin_progress=length(seq_group)*length(seq_group)
  for (i in 1:(length(seq_group)-1)) {
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
    for (j in 1:(length(seq_group)-1)) {
      temp=coeffRV(Varia_paysage_multi[,seq_group[i]:(seq_group[i+1]-1)],Varia_paysage_multi[,seq_group[j]:(seq_group[j+1]-1)])
      coefRV[i,j]=temp$rv
      pvalue[i,j]=temp$p.value
      Sys.sleep(0.1)
      incProgress(1/shin_progress)
    }
  }

  colnames(coefRV) <- rownames(coefRV)<- metrics
  colnames(pvalue) <- rownames(pvalue)<- metrics
  return(list(coefRV,pvalue))
}
