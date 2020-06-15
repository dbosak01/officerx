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
                   dedupe=FALSE, id_var = FALSE) {
  

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
              dedupe = dedupe, 
              id_var = id_var)
  
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


create_flextable <- function(fi) {

  
  ret <- flextable(fi$data, theme_fun = NULL) %>%   
    theme_normal(fontname = fi$font_name) %>%
    set_header_labels(values = fi$labels)
  
  for(i in seq_along(fi$keys))
  {
    ret <- width(ret, j = i, width = fi$col_width[i])
    ret <- align(ret, j = i, align = fi$col_align[i], part = "body") 
    ret <- align(ret, j = i, align = fi$label_align[i], part = "header")
    
  }

  

  return(ret)
}

# Basic strategy of this function is to determine info for entire data table, 
# then split it up according to pages.  For each page, create a flextable
# for that page.  Collect flextables in a list and return.
create_flextables <- function(ts, body_size, font_name = "Courier New") {
  
  if (ts$show_cols == "only" & length(ts$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"only\".")  
  }
  
  family <- get_font_family(font_name)
  
  # Get vector of all included column names
  # Not all columns in dataset are necessarily included 
  # depends on show_all parameter on create_table and 
  # visible parameter on column definitions
  keys <- get_table_cols(ts)
  
  # Filter dataset by included columns
  dat <- ts$data[ , keys]
  
  # Get labels
  labels <- get_labels(dat, ts$col_defs, ts$n_format)
  
  # Get column alignments
  aligns <- get_aligns(dat, ts$col_defs)
  
  # Get alignment for labels
  # Follows column alignment by default
  label_aligns <- get_label_aligns(ts$col_defs, aligns)
  
  
  # Get column widths
  widths <- get_col_widths(dat, ts$col_defs, labels, font_family = family) 
  
  # Get available space for table data
  data_size <- get_data_size(body_size, widths, labels, font_family = family)
  
  
  # Break columns into pages
  wraps <- get_page_wraps(data_size, ts$col_defs, widths)
  
  
  # Add blank lines as specified
  dat <- prep_data(dat, ts$col_defs, first_blank = ts$first_row_blank)
  
  
  # split rows
  splits <- get_splits(dat, widths, data_size, font_family = family)
  #splits <- list(dat[1:15, ])
  
  flx_lst <- list()
  for(s in splits) {
    for(pg in wraps) {

      f <- flex_info(data= s[, pg], keys = pg, label=labels[pg], 
                     col_width = widths[pg], col_align = aligns[pg], 
                     font_name = font_name, label_align = label_aligns[pg])
      flx_lst[[length(flx_lst) + 1]] <- create_flextable(f)
    }
  }
  
  
  return(flx_lst)

}



# bs <- get_body_size(rpt)
# fts <- create_flextables(tb, bs)
 
# labels <- get_labels(tb$data, tb$col_defs, tb$n_format)


# Flex Info --------------------------------------------------------------------

flex_info <- function(data, keys, font_name, col_width, col_align,  
                      label, label_align) {
  
  ret <- structure(list(), class = c("flex_info", "list"))
  
  ret$data <- data
  ret$keys <- keys
  ret$font_name <- font_name
  ret$col_width <- col_width
  ret$col_align <- col_align
  ret$label <- label
  ret$label_align <- label_align
  
  
  return(ret)
  
}

# Utilities --------------------------------------------------------------------

get_splits <- function(dat, col_widths, data_size, font_family) {
  
  
  rh <- .3 #strheight("Test String", units = "inches", family = font_family)
  
  ws <- list()
  for (i in seq_along(dat)) {
    
    w_all <- strwidth(dat[[i]], units="inches", family=font_family)
    ws[[length(ws) + 1]] <- w_all
  }
  
  
  library(qlcMatrix)
  
  m <- matrix(unlist(ws), ncol=length(ws), byrow=FALSE)
  
  #print(m)
  
  #lin <- ceiling(m /col_widths)
  lin <- ceiling(t(t(m) / col_widths))
  
  #print(lin)
  
  #print(col_widths)
  
  # Get counts of carriage returns per cell
  library(stringi)
  
  rs <- list()
  for (col in seq_along(dat)) {
    rs[[length(rs)+1]] <- stri_count(dat[[col]], fixed = "\n")
  }
  
  r <- matrix(unlist(rs), ncol=length(rs), byrow=FALSE)
  
  # Add carriage returns to line counts
  lr <- lin + r
  
  
  lr[is.na(lr)] <- 0
  

  row_heights <- rowMax(lr)@x * rh
  


  row_pages <- get_pages(row_heights, data_size["height"])
  
  
  ret <- split(dat, row_pages)
  
  return(ret)
  
}


# b <- get_body_size(rpt)
# l <- get_labels(tb$data, tb$col_defs, tb$n_format)
# d <- get_data_size(b, NULL, l, "mono" )
# w <- get_col_widths(tb$data, tb$col_defs, l, "mono")
# p <- get_splits(tb$data, w, d, "mono")



get_page_wraps <- function(data_size, defs, widths) {
  
  id_vars <- c()
  for (def in defs) {
    if (!is.null(def$id_var) && def$id_var)
      id_vars[length(id_vars) + 1] <- def$var_c
    
  }
  
  
  ret <- list()
  pg <- c()
  tw <- data_size["width"]

  for (nm in names(widths)) {
    if (length(pg) == 0 && length(id_vars) > 0) {
      pg <- widths[id_vars]
      names(pg) <- id_vars
    }
      
    if (sum(pg, widths[nm]) < tw) {
      pg[nm] <- widths[nm]
      #names(pg[length(pg)]) <- nm
    } else {
      
      ret[[length(ret) + 1]] <- names(pg)
      pg <- c()
    }
  }
  

  if (length(pg) > 0)
    ret[[length(ret) + 1]] <- names(pg)
  
  return(ret)
  
}

# b <- get_body_size(rpt)
# l <- get_labels(tb$data, tb$col_defs, tb$n_format)
# d <- get_data_size(b, NULL, l, "mono" )
# w <- get_col_widths(tb$data, tb$col_defs, l, "mono")
# p <- get_page_wraps(d, tb$col_defs, w)
# p

prep_data <- function(dat, defs, first_blank) {
  

  ls <- c()
  for (def in defs) {
    if (def$blank_after)
      ls[length(ls) + 1] <- def$var_c
  }
  if (length(ls) > 0) {
    dat <- add_blank_rows(dat, .var_list = ls)
  }
  
  if (first_blank)
    dat <- add_blank_row(dat, location = "above")  

  return(dat)  
  
}



get_col_widths <- function(dat, defs, labels, font_family) {
  
  max_col_width = 5
  min_col_width = .5
  padding_buffer = .05
  
  dwidths <- c()  

  # Set default widths based on length of data
  for (i in seq_along(dat)) {
    
    w <- max(strwidth(dat[[i]], units="inches", family=font_family))
    if (w > max_col_width)
      w <- max_col_width
    else if (w < min_col_width)
      w <- min_col_width
    else
      w <- (ceiling(w * 100)/100) + padding_buffer
    
    # Determine width of words in label for this column
    s <- stri_split(labels[[i]], fixed=" ")
    l <- strwidth(s[[1]], units="inches", family=font_family)

    # If the max word width is greater than the data width,
    # set column width to max label word width
    # so as not to break any words in the label
    if (max(l) > w)
      dwidths[length(dwidths) + 1] <- max(l)
    else
      dwidths[length(dwidths) + 1] <- w

  }

  # Set names for easy access
  names(dwidths) <- names(dat)
  
  # Set default widths
  ret = dwidths
  
  # Let user settings override defaults
  for (def in defs) {
    
    if (def$var_c %in% names(dat) & !is.null(def$width) && def$width > 0) {
      ret[def$var_c] <- def$width    
    }
    
  }
  
  return(ret)
}


# widths not incorporated yet
get_data_size <- function(body_size, widths, labels, font_family) {
  
  ppi = 72
  
  sz <- c()
  for (n in labels) {
    sz[length(sz) + 1] <- strheight(n, units="inches", family=font_family)
    
  }
  
  ret <- body_size
  
  ret["height"] <- ret["height"] - max(sz)
  
  return(ret)
  
}


get_label_aligns <- function(defs, aligns) {
  
  
  ret <- aligns
  
  for (d in defs) {
    if (!is.null(d$label_align) & d$var_c %in% names(aligns))
      ret[d$var_c] <-  d$label_align
    
  }
  
  return(ret)
}



get_aligns <- function(dat, defs) {
  
  nms <- names(dat)
  ret <- c()
  
  # Get default alignments
  # based on data type
  # Character will go to left
  # Others goes to right
  for (nm in nms) {
    
    if (is.character(dat[[nm]]))
      ret[length(ret) + 1] <- "left"
    else 
      ret[length(ret) + 1] <- "right"
      
  }
  
  # Assign names to vector for easy access to alignment values
  names(ret) <- nms
  
  # Assign alignments from column definitions
  for (d in defs) {
    if (!is.null(d$align) & d$var_c %in% nms)
      ret[d$var_c] <- d$align
  }
  
  return(ret)
  
}



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




# Declare function to calculate pages
get_pages <- function(x, page_size){
  
  
  running_sum <- 0
  page <- 1
  
  get_pages_int <- Vectorize(function(x){
    
    if (running_sum + x > page_size) {
      page <<- page + 1
      running_sum <<- x
    } else {
      running_sum <<- running_sum + x
    }
    
    return(page)
  })
  
  return(get_pages_int(x))
}


# Declare function to calculate page breaks
get_page_breaks <- function(x, page_size){
  
  
  running_sum <- 0
  page_breaks <- c(1)
  counter <- 0
  
  get_pages_int <- Vectorize(function(x){
    
    counter <<- counter + 1
    
    if (running_sum + x > page_size) {
      page_breaks[length(page_breaks) + 1] <<- counter
      running_sum <<- x
    } else {
      running_sum <<- running_sum + x
    }
    
    
  })

  
  get_pages_int(x)

  page_breaks[length(page_breaks) + 1] <- length(x) + 1
  
  return(page_breaks)
}


split_by_page <- function(df, rows, cols, idcols = NULL){
  
  # Initialize list of dataframe to return
  ret <- list()
  
  
  # Split the incoming dataframe according to indicies
  split_data <- split(df, rows) 
  
  # Reapply the labels lost during the split
  data_labeled <- list()
  for(sds in seq_along(split_data)){
    data_labeled[[sds]] <- copy_labels(split_data[[sds]], df)
  }
  
  # Create vectors for vertical split
  ls <- list()
  for(i in seq_along(data)) {
    
    if (i %in% cols) {
      ls[[length(ls) + 1]] <- c(i)
      
    } else {
      
      l <- ls[[length(ls)]]
      l[length(l) + 1] <- i
      ls[[length(ls)]] <- l
      
    }
  }
  
  # Split data vertically and add each to return list
  ret <- list()
  for(spl in data_labeled) {
    for (cv in ls) {
      ret[[length(ret) + 1]] <- spl[ , cv]
    }
  }
  
  return(ret)
}

#' @param x The Table spec object
get_table_cols <- function(x) {
  
  dat <- x$data
  
  ret <- c()
  show_all <- FALSE 
  if (length(x$show_cols) == 1 && x$show_cols == "all") {
    ret <- names(dat)
    show_all <- TRUE
  }
  else if (length(x$show_cols) == 1 && x$show_cols == "all") {
    show_all <- FALSE
  }
  else if (all(x$show_cols %in% names(dat))) 
    ret <- x$show_cols
  
  # Deal with visible options
  for (def in x$col_defs) {

    if (show_all == FALSE & def$visible)
      ret[length(ret) + 1] <- def$var_c
    else if (show_all == TRUE & def$visible == FALSE)
      ret <- ret[!ret %in% def$var_c]
  }
  
  return(ret)
  
}


prep_data2 <- function(x, col_list) {
  
  # Filter selected columns
  dat <- x$data[ , col_list]
  
  # Dedupe indicated columns
  for (def in x$col_defs) {
    if (def$dedupe)
      dat[[def$var]][duplicated(dat[[def$var]])] <- ""
  }
  
  # Add blank rows (may need to move this after page splits)
  ls <- c()
  for (def in x$col_defs) {
    if (def$blank_after)
      ls[length(ls) + 1] <- def$var_c
  }
  if (length(ls) > 0) {
    dat <- add_blank_rows(dat, .var_list = ls)
  }
  
  # Add first row blank if indicated
  if (rt$first_row_blank)
    dat <- add_blank_row(dat, location = "above")
  
  return(dat)
}


# stri_split("This\nthere is\nsomething", fixed ="\n")
# 
# 
# 
# g <- c("This", "there is", "somethings\nelse2s")
# 
# 
# stri_split(g[3], fixed = "\n")
# 
# max(nchar(g))
# 
# max(nchar(g))
# 
# strwidth(g, units="inches", family=fam)
# 
# cell_width <- Vectorize(function(x) {
#   
#   spl <- stri_split(x, fixed = "\n")[[1]]
#   print(spl)
#   ret <- max(strwidth(spl, units = "inches", family = fam))
#   
#   return(ret)
#   
# })
# 
# cell_width(g)
# 
# strheight(g, units="inches", family=fam)
