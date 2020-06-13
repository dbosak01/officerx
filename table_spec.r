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


#' A function to format the population label
#' @noRd
lowcase_parens <- function(x) {
  
  ret <- paste0("\n(n=", x, ")")
  
  return(ret)
}

upcase_parens <- function(x) {
  
  ret <- paste0("\n(N=", x, ")")
  
  return(ret)
  
}

lowcase_n <- function(x) {
  
  ret <- paste0("\nn=", x)
  
  return(ret)
}

upcase_n <- function(x) {
  
  ret <- paste0("\nN=", x)
  
  return(ret)
  
}

# Table Spec Functions ---------------------------------------------------------

create_table <- function(x, n_format = upcase_parens, page_var = NULL, 
                         show_cols = "all", first_row_blank=FALSE) {
  
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
  ret$show_cols <- show_cols
  ret$first_row_blank <- first_row_blank
  
  return(ret)
  
  }


define <- function(x, var, label = NULL, col_type = NULL, 
                   align=NULL, label_align=NULL, width=NULL, 
                   visible=TRUE, n = NULL, blank_after=FALSE,
                   dedupe=FALSE) {
  

  def <- list(var = deparse(substitute(var)), 
              var_c = as.character(substitute(var)),
              label= label,
              col_type = col_type,
              align = align, 
              label_align = if (is.null(label_align) & !is.null(align)) 
                                align else label_align,
              width = width, 
              visible = visible, 
              n = n, 
              blank_after = blank_after, 
              dedupe=dedupe)
  
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

table_options <- function(x, first_row_blank=FALSE){
  
  
  x$first_row_blank = first_row_blank
  
  
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
      ls[[def$var]] <- paste0(ls[[def$var]],  nfmt(def$n))
    }
  }
  
  return(ls)
}



create_flextable <- function(rt, font_name = "Courier New") {

  if (rt$show_cols == "only" & length(rt$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"only\".")  
  }
  
  dat <- rt$data
  
  # Add blank rows
  ls <- c()
  for (def in rt$col_defs) {
    if (def$blank_after)
      ls[length(ls) + 1] <- def$var_c
  }
  if (length(ls) > 0) {
    dat <- add_blank_rows(dat, .var_list = ls)
  }

  
  if (rt$first_row_blank)
    dat <- add_blank_row(dat, location = "above")
  
  # show_cols option
  keys <- c()
  show_all <- FALSE 
  if (length(rt$show_cols) == 1 && rt$show_cols == "all") {
    keys <- names(dat)
    show_all <- TRUE
  }
  else if (length(rt$show_cols) == 1 && rt$show_cols == "all") {
    show_all <- FALSE
  }
  else if (all(rt$show_cols %in% names(dat))) 
    keys <- rt$show_cols
  
  # Dedupe and visible options
  for (def in rt$col_defs) {
    if (def$dedupe)
      dat[[def$var]][duplicated(dat[[def$var]])] <- ""
    
    if (show_all == FALSE & def$visible)
      keys[length(keys) + 1] <- def$var_c
    else if (show_all == TRUE & def$visible == FALSE)
      keys <- keys[!keys %in% def$var_c]
  }
  
  #print(keys)
  
  ret <- flextable(dat, theme_fun = NULL, col_keys = keys) %>%   
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
  samp <- select(rt$data, keys)
  if (nrow(rt$data) > 1500)
    samp <- samp[sample(1:nrow(samp), 1500), ]

  # Set default widths based on length of data
  for (col in seq_along(samp)) {

    w <- max(strwidth(samp[[col]], units="inches", family=fam))
    if (w > 4)
      w <- 4
    else if (w < .5)
      w <- .5
    else
      w <- (ceiling(w * 100)/100) + .05

    #print(w)

    # Set default width
    ret <- width(ret, j=attr(samp[col], "name"), width = w)

    # Set default alignment
    if (class(samp[[col]]) == "character") {
      ret <- align(ret, j=attr(samp[col], "name"), align = "left", part = "all")
    } else {
      ret <- align(ret, j=attr(samp[col], "name"), align = "right", part = "all")
    }

  }


  for (def in rt$col_defs) {

    if (!is.null(def$width) & def$visible)
      ret <- width(ret, j=def$var, width=def$width)
    if (!is.null(def$align) & def$visible)
      ret <- align(ret, j=def$var, align = def$align, part = "body")
    if (!is.null(def$label_align) & def$visible)
      ret <- align(ret, j=def$var, align = def$label_align, part = "header")
  }
  

  return(ret)
}




