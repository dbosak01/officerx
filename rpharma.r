


'%||%' <- function(x, y)paste0(x,y)
"new" %||% "operator"


add_blank_row <- function(x, ..., location="below"){
  
  rw <- x[0, ]
  for(i in seq_along(x)){
    if ("character" %in% class(rw[[i]])){
      rw[1, i] <- ""
    } else if("factor" %in% class(rw[[i]])){
      
      rw[[i]] <- as.vector(rw[[i]], mode="character")
      rw[1, i] <- ""
    }
  }
  
  parms <- list(...)
  for(n in names(parms)){
    if(n %in% names(rw))
      rw[[n]] <- parms[[n]]
  }
  
  ret <- NULL
  if (location == "below")
    ret <- bind_rows(x, rw)
  else if (location == "above")
    ret <- bind_rows(rw, x)
  else if (location == "both")
    ret <- bind_rows(rw, x, rw)
  
  return(ret)
}



