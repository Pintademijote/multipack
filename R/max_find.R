#' Search peak in a vector
#'
#' `max_find()` search the peak in a vector
#' @param temp The tab generated by formodel() function.
#'
#' @param test vector including peak to find
#' @author Pierre-Gilles Lemasle <pg.lemasle@gmail.com>
#' @return return bolean vector where true indicate a peak
#' @export
#'
max_find=function(test){
  vec_true=NULL
  if(test[1]>=test[2]){
    vec_true=c(vec_true,TRUE)
  }else{
    vec_true=c(vec_true,FALSE)
  }
  for (i in 2:(length(test)-1)) {
    if(test[i]>=test[i-1] & test[i]>=test[i+1]){
      vec_true=c(vec_true,TRUE)
    }else{
      vec_true=c(vec_true,FALSE)
    }
  }
  if(test[length(test)]>=test[length(test)-1]){
    vec_true=c(vec_true,TRUE)
  }else{
    vec_true=c(vec_true,FALSE)
  }
  return(vec_true)
}
