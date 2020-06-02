library(officer)
library(flextable)
library(magrittr)
library(sjlabelled)
library(dplyr)


# Flextable and Officer Extended Functions ------------------------------------


# Normal theme for flextable.  Needs more work to eliminate defaults.
theme_normal <- function(x, fontname = "Courier New", fontsize=10){
  
  b_black = fp_border(color = "black")
  
  ret <- x %>% valign(valign = "top", part = "body") %>%
    font(fontname = fontname, part="all") %>%
    fontsize(size = fontsize, part = "all") %>%
    bold(bold = FALSE, part= "all") %>%
    italic(italic = FALSE, part="all")  %>%
    hline_bottom(border = b_black, part = "header") 
}


create_report <- function(page_template, object){
  
  # Write out document template
  write_page_template(page_template)
  

  my_doc <- read_docx(page_template$file_path) # %>%
    #cursor_begin() %>% body_remove() 
  if (class(object)[1] == "flextable")
    body_add_flextable(my_doc, object)
  if (class(object)[1] == "gg")
    body_add_gg(my_doc, object)
  
  print(my_doc, target = page_template$file_path)
  
  
  file_list <- list.files(path = tempdir(), 
                          full.names = TRUE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = TRUE, no.. = TRUE)
  
  unlink(file_list, recursive = TRUE, force=TRUE)
  
}




#' body_add_flextables  
#' 
#' Multi-table add function for flextable.  This function avoids the memory problems associated
#' with adding many multiple tables and creating large documents.
#' 
#'
#' @param x officer document to add the flextable to
#' @param flextable_list A list of flextables to add to the document
#' @param pgbreak Whether to add a page break after each table. 
#' @param other Note:  All other parameters are passed to body_add_flextable.  See documentation.
#' @return The original officer document with the tables from the list added to the document.
#' @examples
#' # Create sample data
#' rowsize <- 160
#' col1 <- 1:rowsize
#' col2 <- runif(rowsize, 0, 1000)
#' col3 <- rep(1:(rowsize /40), each=40)
#' data <- data.frame(col1, col2, col3)
#' dfs  <- split(data, col3)
#' 
#' # Create flextable objects
#' fts <- list()
#' b_black = fp_border(color = "black")
#' for(pg in dfs){
#'   fts[[length(fts) + 1]] <- flextable(pg, theme_fun=NULL)  %>%
#'     width(j=c(1, 2, 3), c(2,2,3)) %>%
#'     valign(valign = "top", part = "body") %>%
#'     font(fontname = "Courier New", part="all") %>%
#'     fontsize(size = 10, part = "all") %>%
#'     bold(bold = FALSE, part= "all") %>%
#'     italic(italic = FALSE, part="all")  %>%
#'     hline_bottom(border = b_black, part = "header")
#' }
#' # Add flextables to document
#' my_doc <- read_docx()
#' my_doc <- cursor_begin(my_doc)
#' my_doc <- body_remove(my_doc)
#' my_doc <- body_add_flextables(my_doc, fts)
#' print(my_doc, "test.docx")
body_add_flextables <- function(x, flextable_list, align = "center", pos = "after", split = FALSE, pgbreak=TRUE) {
  
  
  # Initialize variables
  counter <- 1
  length_list <- length(flextable_list)
  
  # Create temp file 
  temp_path <- tempfile(pattern = "", fileext = ".docx")
  
  
  for(tbl in flextable_list){
    
    # Create new document
    temp_doc <- read_docx()
    
    # Add flextable to new document
    temp_doc <- body_add_flextable(temp_doc, tbl, align = align, pos = pos, split = split)
    
    # Print document to temp file
    print(temp_doc, target = temp_path)
    
    # Add the temp file to the main document
    # This method avoids the memory problems 
    # associated with adding the flextables
    # directly to the main document.
    x <- body_add_docx(x, src= temp_path)
    
    # Add a page break after all but the last flextable 
    if (pgbreak == TRUE & counter < length_list){
      x <- body_add_break(x)
    }
    
    # Counter for tracking place in list
    counter <- counter + 1
  }
  
  # Remove temp file
  unlink(temp_path)
  
  return(x)
}

#' fit_to_page  
#' 
#' Fit a flex table to take up the amount of space specified by the \code{pgwidth} parameter.
#' 
#' @param ft The flextable to fit.
#' @param pgwidth The width of the available space in inches.
#' @return The flextable returned with column widths resized. 
#' @examples
#'   fts <- flextable(mtcars, theme_fun=NULL)  %>%
#'     font(fontname = "Courier New", part="all") %>%
#'     fontsize(size = 10, part = "all") %>%
#'     bold(bold = FALSE, part= "all") %>%
#'     italic(italic = FALSE, part="all")  %>%
#'     hline_bottom(border = b_black, part = "header") %>%
#'     fit_to_page(pgwidth=5)
#'     
fit_to_page <- function(ft, pgwidth = 9){
  
  # First run autofit to get proportions
  ft_out <- autofit(ft)
  
  # Adjust based on pgwidth parameter
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  
  return(ft_out)
}

#' use_data_labels  
#' 
#' Assigns the labels associated with a dataframe to the headers in the flextable.
#' 
#' @param x The flextable to modify.
#' @return The flextable returned with header labels assigned.
use_data_labels <- function(x){
  
  # Extract the dataframe from the flextable body
  ds <- x$body$dataset
  
  # Get the column names from the dataframe
  v1 <- names(ds)
  
  # Get the labels from the dataframe
  # Not so easy because not all columns have labels.
  # If a column has no labels, use the column name 
  # as the header string.
  v2 <- c()
  counter <- 1
  for(col in ds){
    if (!is.null(get_label(col))){
      v2 <- c(v2, get_label(col))
    } else {
      if (is.null( names(col))){
        v2 <- c(v2, v1[counter])
      } else {
        v2 <- c(v2, names(col))
      }
      
    }
    counter <- counter + 1
  }
  
  # Convert label vector to a list
  ls <- as.list(v2)
  
  # Assign names to list
  names(ls) <- v1
  
  # Assign the labels to the flextable using list constructed above 
  x <- set_header_labels(x, values = ls)
  
  return(x)
}


#' gen_groups
#' 
#' Creates group values based on a number of items and a vector of item counts per group.  
#' 
#' This function is used to create a vector of group values.  It can use used to dynamically subset a dataframe based on row counts rather than data values.
#' The last_indices parameter can be used to return the indices of the last item in each group.
#' 
#'
#' @param tot Total number of items in return vector.
#' @param group_cnt Number of items in each group.  This can be a single value or vector of values.  Will recycle if needed.
#' @param last_indices TRUE or FALSE value indicating whether to return the vector of values, or a vector of row indices where the breaks occur.
#' @return A vector of length \code{tot} broken into groups specified by \code{group_cnt}.  Groups will be identified by integers from 1 to n.
#' @examples
#' #' gen_groups(10, 3) 
#' >[1] 1 1 1 2 2 2 3 3 3 4
#' 
#' gen_groups(12, c(3, 2, 5, 2)) 
#' >[1] 1 1 1 2 2 3 3 3 3 3 4 4
#' 
#' #' #' gen_groups(10, 3, last_indices = TRUE) 
#' >[1] 3 6 9 10
#' 
#' gen_groups(12, c(3, 2, 5, 2), last_indices = TRUE) 
#' >[1] 3 5 10 12
gen_groups <- function(tot, group_cnt, last_indices = FALSE){
  
  # Create empty return vector
  ret <- c()
  
  # Initialize cursors
  cnt <- 0
  ind <- 1
  
  # Populate return vector with group values or last indices
  while(cnt <= tot){
    for(i in 1:length(group_cnt)){
      for(j in 1:group_cnt[i]){
        
        cnt <- cnt + 1
        
        if (cnt <= tot){
          if (last_indices){
            ret[ind] <- cnt
          }else{
            ret[cnt] <- ind
          }
        }
        
      }
      ind <- ind + 1
    }
  }
  
  return(ret)
}


#' split_df_pages
#' 
#' A function to split a dataframe into pages according to vectors rows and cols  
#' 
#' @param df A dataframe to split
#' @param rows A vector of row counts on which to split.  Will recycle if needed.
#' @param cols A vector of column counts on which to split.  Will recycle if needed.
#' @param idcols A vector of id columns to include on each page.
#' @return A list of dataframes sized according to the specifications in \code{rows} and \code{cols}
#' @examples
#' # With row labels and no identity column
#' split_df_pages(mtcars, 16, c(5, 6)) 
#' 
#' # With identity column
#' split_df_pages(starwars,10, 5, 1)
split_df_pages <- function(df, rows, cols, idcols = NULL){
  
  # Initialize list of dataframe to return
  ret <- list()
  
  # Get the row indicies for each target dataframe
  row_indices <- gen_groups(nrow(df), rows)
  
  # Split the incoming dataframe according to indicies
  split_data <- split(df, row_indices) 
  
  # Reapply the labels lost during the split
  data_labeled <- list()
  for(sds in 1:length(split_data)){
    data_labeled[[sds]] <- copy_labels(split_data[[1]], df)
  }
  
  # Generate the column indices to split data vertically
  col_indices <- gen_groups(ncol(df), cols, last_indices = TRUE)
  
  # Split data vertically and add each to return list
  counter <- 1
  for(i in data_labeled){
    startpos <- 1
    for(j in col_indices){
      if (is.null(idcols)){
        ret[[counter]] <- i[ , startpos:j]
      } else {
        ret[[counter]] <- i[ , unique(c(idcols, startpos:j))]
      }
      startpos <- j + 1
      counter <- counter + 1
    }
  }
  
  return(ret)
}

