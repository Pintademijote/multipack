#' @export
read.Chloe.properties=function(patch,type){
  temp=file(patch,open="r")
  test=readLines(temp)
  if(type=="Distance"){
    i=grepl("window_sizes=",test)
    word="window_sizes="
  }
  if(type=="Metrics"){
    i=grepl("metrics=",test)
    word="metrics="
  }
  temp=file(patch,open="r")
  test=readLines(temp)[i]
  close(temp)
  test=gsub('"', "", test, fixed=TRUE)
  test=gsub("\\{", '("', test)
  test=gsub("\\}", '")', test)
  test=gsub("\\;", '","', test)
  test=gsub(word, "", test)
  test=paste0("c",test,sep = "")
  test=eval(parse(text = test))
  # test=sub("-", ".",test) 
  # test=sub("-", ".",test)
  test
}