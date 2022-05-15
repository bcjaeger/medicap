

get_unique <- function(x){
  # use droplevels in case x was subsetted prior to this
  # and some levels are no longer relevant
  if(is.factor(x)) return(levels(droplevels(x)))
  sort(unique(x))
}
