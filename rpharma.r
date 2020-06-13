library(dplyr)

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
#' The purpose of this function is to add a blank row to the top or bottom 
#' of a dataframe.  Character columns will be set to an empty string.  Numeric
#' and date columns will be set to an NA. The function allows the user to pass
#' in values for specified columns.  This feature is useful for setting key
#' values.  Note that a blank value will be added to factor levels that do not
#' contain blanks.
#' @param x The dataframe to add blanks to.
#' @param ... Column names and non-blank values to assign.
#' @param location The location to add the blank row.  Valid values are "above",
#' "below", and "both".  The default value is "below".
#' @return  The input dataset with the blank row added at the specified 
#' location.
#' @example
#' library(dplyr)
#' s <- filter(iris, Species == "setosa")
#' b <- add_blank_row(s)
#' @export
#'##############################################################################
add_blank_row <- function(x, ..., location="below"){
  
  # Create a blank row with the same structure as the incoming dataframe.
  rw <- x[0, ]
  
  # For character columns, add a blank.  
  # For numeric columns, NA is generated automatically.
  # For factors, cast to vector if blank is not in level list.
  for (i in seq_along(x)) {
    if ("character" %in% class(rw[[i]])) {
      rw[1, i] <- ""
    } else if("factor" %in% class(rw[[i]])) {
      
      if (!"" %in% levels(rw[[i]])) { 
        levels(x[[i]]) <- c(levels(x[[i]]), "")
        levels(rw[[i]]) <- c(levels(rw[[i]]), "")
      }
        
      rw[1, i] <- ""
    }
  }
  
  # Allow the user to seed columns with desired values.
  # This functionality is desirable for key columns.
  parms <- list(...)
  for (n in names(parms)) {
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


#'##############################################################################
#' @title 
#' Add blank rows to a data frame after each by-group
#' 
#' @description 
#' The purpose of this function is to add a blank rows to a dataframe for each
#' by-group.  Character columns will be set to an empty string.  Numeric
#' and date columns will be set to an NA. The function allows the user to pass
#' in column names to group by.  Note that a blank value will be added to factor 
#' levels that do not contain blanks.
#' @param x The dataframe to add blanks to.
#' @param ... Column names for group variables.
#' @param var_list A character vector or list of column names to split by.
#' @return  The input dataset with the blank row added after each by-group.
#' @example
#' b <- add_blank_rows(iris, Species)
#' @export
#'##############################################################################
add_blank_rows <- function(x, ..., .var_list = NULL) {
  
  # Group dataframe
  if (is.null(.var_list))
    grp <- group_by(x, ...) 
  else 
    grp <- group_by(x, across(all_of(.var_list)))
  
  # Split by group variables
  lst <- group_split(grp)
  
  # Create a new list to avoid complaints
  # from tidyverse
  ret <- list()
  
  # Add blank row for each split
  for (i in seq_along(lst)) {

    ret[[i]] <- add_blank_row(lst[[i]])

  }

  # Combine splits
  ret <- bind_rows(ret)
  
  return(ret)
   
}




