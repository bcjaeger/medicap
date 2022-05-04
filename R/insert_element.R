

insert_element <- function(vec,
                           element_value,
                           element_name,
                           insert_before){


  if(is.character(insert_before)){

    if(insert_before %!in% vec)
      stop("insert_before should be a value in",
           " vec if you use a character value",
           call. = FALSE)

    insert_before <- which(vec == insert_before)

  }

  l <- length(vec)

  stopifnot(insert_before <= l)

  vec_new <- vector(mode = typeof(vec), length = l+1)

  if(insert_before == 1){
    vec_new[1] <- element_value
    vec_new[-1] <- vec
    names(vec_new) <- c(element_name, names(vec))

  } else if (insert_before == l){

    vec_new[l] <- element_value
    vec_new[seq(l-1)] <- vec[seq(l-1)]
    vec_new[l+1] <- vec[l]
    names(vec_new) <- c(names(vec)[seq(l-1)],
                        element_name,
                        names(vec)[l])

  } else {

    vec_new[seq(insert_before-1)] <- vec[seq(insert_before-1)]
    vec_new[insert_before] <- element_value
    vec_new[seq(insert_before+1, l+1)] <- vec[seq(insert_before, l)]

    names(vec_new) <- c(names(vec)[seq(insert_before-1)],
                        element_name,
                        names(vec)[seq(insert_before, l)])

  }

  vec_new

}
