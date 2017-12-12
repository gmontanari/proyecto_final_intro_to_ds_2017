library(gmodels)
####Generando una funcion que limpie la fecha
# returns string w/o leading or trailing whitespace
##################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
##################################################


lower_case<-function(x){
  str_replace_all(tolower(x),"/",' ')
}


left = function (string,char){
  substr(string,1,char)
}

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

tab_stata_fun<-function(z,x,y){
  with(z, CrossTable(x, y, missing.include=TRUE))
}



tab_stata_one<-function(z,x){
  temporal<-z%>%group_by(.dots = lazyeval::lazy(x))%>%summarise(Observations=n())
  View(temporal)
}
