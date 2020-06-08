library(flextable)
library(magrittr)


## To Do ##
# Automatic paging and wrapping
# Spanning header function

# ft <- report_table(final, n_format=NULL, page_var=page) %>%
#   define(var="variable", label="Variable", align="left", width=1.25, visible=TRUE) %>%
#   define(var="category", label="", align="left", width=2) %>%
#   define(var="placebo", label="Placebo", align="center", width=1, n=placebo_pop) %>%
#   define(var="drug", label="Drug", align="center",  width=1, n=drug_pop) %>%
#   define(var="overall", label="Overall", align="center", width=1, n=overall_pop, wrap=TRUE) %>%
#   spanning_header(span_cols=c("placebo", "drug"), label="Treatment Groups", label_align="center")


# Formats ----------------------------------------------------------------------


lowcase_parens <- function(x) {
  
  ret <- paste0("(n=", x, ")")
  
  return(ret)
}

upcase_parens <- function(x) {
  
  ret <- paste0("(N=", x, ")")
  
  return(ret)
  
}

lowcase_n <- function(x) {
  
  ret <- paste0("n=", x)
  
  return(ret)
}

upcase_n <- function(x) {
  
  ret <- paste0("N=", x)
  
  return(ret)
  
}

# Table Spec Functions ---------------------------------------------------------

create_table <- function(x, n_format = upcase_parens, page_var = NULL) {
  
  ret <- structure(list(), class = c("table_spec", "list"))
  
  if (!"data.frame" %in% class(x)) {
     stop(paste("ERROR: data parameter 'x' on",
                 "page_template() function is invalid.", 
                "\n\tValid values are a data.frame or tibble."))
  }

  
  ret$data <- x
  ret$n_format <- n_format
  ret$page_var <- page_var
  ret$col_defs <- list()
  ret$col_spans <- list()
  
  return(ret)
  
}


define <- function(x, var, label = NULL, col_type = NULL, 
                   align=NULL, label_align=NULL, width=NULL, 
                   visible=TRUE, n = NULL) {
  

  def <- list(var = deparse(substitute(var)), 
              label= label,
              col_type = col_type,
              align = align, 
              label_align = if (is.null(label_align) & !is.null(align)) 
                                align else label_align,
              width = width, 
              visible = visible, 
              n = n)
  
  x$col_defs[[length(x$col_defs) + 1]] <- def

  return(x)  
}

spanning_header <- function(x, span_cols, label = "", 
                            label_align = "center", level = 1, n = NULL) {
  
  sh <- list(span_cols = span_cols, 
             label = label, 
             label_align = label_align, 
             level = level,
             n = n)
  
  x$col_spans[[length(x$col_spans) + 1]] <- sh
  
  return(x)
}






# Create flextable (Replace this with something better)
# ft <- flextable(final, theme_fun = NULL)  %>%
#   theme_normal(fontname =  d$font_name) %>%
#   use_data_labels() %>%
#   align(j=c(1, 2), align="left", part="all") %>%
#   align(j=c(3,4, 5), align="center", part="header") %>%
#   width(j=c(1, 2,3, 4, 5), width=c(1.25, 2, 1, 1, 1))

get_labels <- function(dat, defs, nfmt){
  
  # Get the column names from the dataframe
  v1 <- names(dat)
  
  # Get the labels from the dataframe
  # Not so easy because not all columns have labels.
  # If a column has no labels, use the column name 
  # as the header string.
  v2 <- c()
  counter <- 1
  
  for (col in dat) {
    if (!is.null(attr(col, "label"))) {
      v2 <- c(v2, attr(col, "label"))
    } else {
      if (is.null( names(col))) {
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
  
  for (def in defs) {
    
    if (!is.null(def$label))
      ls[[def$var]] <- def$label 
   
    if (!is.null(def$n) ) {
      ls[[def$var]] <- paste0(ls[[def$var]], "\n",  nfmt(def$n))
    }
  }
  
  return(ls)
}



create_flextable <- function(rt, font_name = "Courier New") {

  
  ret <- flextable(rt$data, theme_fun = NULL) %>%
         theme_normal(fontname = font_name) %>%
         set_header_labels(values = get_labels(rt$data, 
                                               rt$col_defs, 
                                               rt$n_format))
  
  # mono, serif, sans
  fam <- case_when(font_name == "Courier New" ~ "mono",
                   font_name == "Arial" ~ "sans",
                   font_name == "Times New Roman" ~ "serif",
                   font_name == "Calibri" ~ "sans")

  
  
  # Sample data if large
  dat <- rt$data
  if (nrow(rt$data) > 1500)
    dat <- fdata[sample(1:nrow(fdata), 1500), ]
  
  # Set default widths based on length of data
  for (col in seq_along(dat)) {
    
    w <- max(strwidth(dat[[col]], units="inches", family=fam))
    if (w > 4)
      w <- 4
    else if (w < .5)
      w <- .5
    else
      w <- (ceiling(w * 100)/100) + .05
    
    #print(w)
    
    # Set default width
    ret <- width(ret, j=attr(dat[col], "name"), width = w)
    
    # Set default alignment
    if (class(dat[[col]]) == "character") {
      ret <- align(ret, j=attr(dat[col], "name"), align = "left", part = "all")
    } else {
      ret <- align(ret, j=attr(dat[col], "name"), align = "right", part = "all")
    }
    
  }
  
  
  for (def in rt$col_defs) {
    if (!is.null(def$width))
      ret <- width(ret, j=def$var, width=def$width)  
    if (!is.null(def$align))
      ret <- align(ret, j=def$var, align = def$align, part = "body")
    if (!is.null(def$label_align))
      ret <- align(ret, j=def$var, align = def$label_align, part = "header")
  }
  

  return(ret)
}



# Report Content Functions -----------------------------------------------------


report_content <- function() {
  
  ret <- structure(list(), class = c("report_content", "list"))
  
  return(ret)
  
}


#' @title 
#' Add content to a report
#' 
#' @description 
#' This function adds an object to the report content list. Valid objects
#' are a table_spec, a flextable, or a plot from ggplot.  Objects will be
#' appended to the report in order they are added.  By default, a page break
#' is added after the content.
#' 
#' @param x a report_spec to append content to
#' @param object the object to append
#' @param page_break whether to add a page break. Value values are "before",
#' "after", or "none"
#' @return The modified report_spec
#' @example 
#' create_report("listing_3_0.docx") %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#' 
#' @export
add_content <- function(x, object, page_break="after") {
  
  # Add page break before if requested
  if (page_break == "before")
    x$content[[length(x$content) + 1]] <- "page_break"
  
  # Add object to the content list
  x$content[[length(x$content) + 1]] <- object
  
  # Add page break after if requested, and by default
  if (page_break == "after")
    x$content[[length(x$content) + 1]] <- "page_break"
  
  return(x)
}






