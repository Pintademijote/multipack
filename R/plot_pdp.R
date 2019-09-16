#' @import pdp
#' @export

plot_pdp=function(train_mod,res_variable_importance,sp_name,type){


  keep_var = as.character(res_variable_importance[res_variable_importance$type ==
                                                    type, 2])
  keep_var=gsub("`","",keep_var)
  keep_eq = row.names(train_mod$finalModel$dirs)[train_mod$finalModel$selected.terms]
  keep_eq=gsub("`","",keep_eq)
  interact_tab = NULL
  for (i in 1:length(keep_var)) {
    temp_interact = grep(keep_var[i], keep_eq)
    temp_interact_name = rep(keep_var[i], length(temp_interact))
    interact_tab = rbind(interact_tab, cbind(interac = temp_interact,
                                             var = temp_interact_name))
  }
  interact_tab = as.data.frame(interact_tab)
  interact_tab$interac = as.numeric(as.character(interact_tab$interac))
  unique_var = interact_tab$var[!(duplicated(interact_tab$interac) |
                                    duplicated(interact_tab$interac, fromLast = TRUE))]
  interact_var = interact_tab[(duplicated(interact_tab$interac) |
                                 duplicated(interact_tab$interac, fromLast = TRUE)), ]
  interact_tab = list()
  for (i in unique(interact_var$interac)) {
    temp_interact = interact_var[interact_var$interac ==
                                   i, 2]
    interact_tab[[as.character(i)]] = temp_interact
  }
  plot_list = list()
  rep = 0
  for (i in unique_var) {
    plot_list[[i]] = partial(train_mod, pred.var = i, grid.resolution = 80,rug = TRUE,plot = TRUE,plot.engine = "ggplot2") +ylab("Activity-Density")+ theme_classic()
  }
  if (length(interact_tab) > 0) {
    for (i in 1:length(interact_tab)) {
      plot_list[[paste(interact_tab[[i]], sep = "", collapse = "-")]] = partial(train_mod,plot = TRUE,
                                                                                pred.var = as.character(interact_tab[[i]]), grid.resolution = 80,plot.engine = "ggplot2",chull = TRUE)+
        labs(fill = "Legend Title\n",z="Legend Title\n")+ theme_classic()+
        scale_fill_continuous(name = "Activity-Density",type = "viridis")

    }
  }
  return(plot_list)
}
