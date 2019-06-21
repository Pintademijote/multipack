#' @export
#'
#'
findCorrelation_hi=function (cor_matrix, cor_pvalue_matrix, threshold)
{
  results = list()
  x1 = cor_matrix
  x2 = cor_pvalue_matrix
  keep_metric_all = NULL
  rm_metric_all = NULL
  diag(x1) = NA
  # x1[upper.tri(x1, diag = TRUE)]=NA
  order_increase = order(apply(x1, 1, function(x) {
    mean(x, na.rm = T)
  }))
  x1 = x1[order_increase, order_increase]
  for (i in colnames(cor_matrix)[order_increase]) {
    temp_cor_index = which(x1 > threshold, arr.ind = TRUE)
    temp_cor_index = temp_cor_index[apply(temp_cor_index,
                                          1, function(x) {
                                            length(unique(x)) == 2
                                          }), ]
    if (i %in% colnames(x1)) {
      if (any(x1[, i] >= threshold, na.rm = T)) {
        sup_thres = which(x1[, i] >= threshold)
        continue = TRUE
        rep = 0
        col_name = colnames(x1)[which(colnames(x1) ==
                                        i)]
        rm_metric = "ignore"
        nb_stop = length(which(x1[, i] >= threshold))
        # print(i)
        while (continue) {
          rep = rep + 1
          j = which(x1[, i] >= threshold)[1]
          if (x2[j, i] <= 0.05) {
            coef1 = mean(x1[, i], na.rm = T)
            coef2 = mean(x1[j, ], na.rm = T)
            coefs = c(coef1, coef2)
            name1 = colnames(x1)[which(colnames(x1) ==
                                         i)]
            name2 = colnames(x1)[j]
            names1_2 = c(name1, name2)
            keep_metric = names1_2[which.max(coefs)]
            rm_metric = names1_2[which.min(coefs)]
            # print(rm_metric)
            if (col_name == rm_metric) {
              continue = FALSE
            }
            x1 = x1[-c(which(colnames(x1) == rm_metric)),
                    -c(which(colnames(x1) == rm_metric))]
            x2 = x2[-c(which(colnames(x2) == rm_metric)),
                    -c(which(colnames(x2) == rm_metric))]
            keep_metric_all = c(keep_metric_all, keep_metric)
            rm_metric_all = c(rm_metric_all, rm_metric)
          }
          if (col_name != rm_metric) {
            if (rep == nb_stop) {
              continue = FALSE
            }
          }
        }
      }
    }
  }


  results$keep = keep_metric_all
  results$rm_names = rm_metric_all
  results$remove = which(colnames(cor_matrix) %in% rm_metric_all)
  return(results)
}

