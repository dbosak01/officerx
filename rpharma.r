
#'##############################################################################
#' @title 
#' Simple Concatenation operator
#' 
#' @description 
#' A simple operator for concatenating strings.  This operator
#' is based on \code{paste0()}, and performs the same function, but with a
#' more compact syntax.
#' @example 
#' "Today is " %||% weekdays(Sys.Date()) %||% "."
#' @seealso [paste0()]
#' @export
#'##############################################################################
'%||%' <- function(x, y)paste0(x,y)
"new" %||% "operator"


#'##############################################################################
#' @title 
#' Add a blank row to a data frame
#' 
#' @description 
#' 
#' 
#' @examples 
#' 
#' 
#'##############################################################################
add_blank_row <- function(x, ..., location="below"){
  
  # Create a blank row with the same structure as the incoming dataframe.
  rw <- x[0, ]
  
  # For character columns, add a blank.  
  # For numeric columns, NA is generated automatically.
  # For factors, cast to vector if blank is not in level list.
  for(i in seq_along(x)) {
    if ("character" %in% class(rw[[i]])) {
      rw[1, i] <- ""
    } else if("factor" %in% class(rw[[i]])) {
      
      if (!"" %in% levels(rw[[i]]))
        rw[[i]] <- as.vector(rw[[i]], mode="character")
      
      rw[1, i] <- ""
    }
  }
  
  # Allow the user to seed columns with desired values.
  # This functionality is desirable for key columns.
  parms <- list(...)
  for(n in names(parms)) {
    if(n %in% names(rw))
      rw[[n]] <- parms[[n]]
  }
  
  # Add the blank row to the specified location.
  ret <- NULL
  if (location == "below")
    ret <- bind_rows(x, rw)
  else if (location == "above")
    ret <- bind_rows(rw, x)
  else if (location == "both")
    ret <- bind_rows(rw, x, rw)
  
  return(ret)
}


