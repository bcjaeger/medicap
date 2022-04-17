

get_unique <- function(x){
  if(is.factor(x)) return(levels(droplevels(x)))
  unique(x)
}
