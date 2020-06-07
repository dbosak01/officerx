library(flextable)
library(magrittr)

# Automatic guessing of widths
# Shorthand for column type: stub, group, analysis
# Automatic paging and wrapping
# Listings in one function call
# Column names without quotes
# ft <- report_table(final, n_format=NULL, page_var=page) %>%
#   define(var="variable", label="Variable", align="left", width=1.25, visible=TRUE) %>%
#   define(var="category", label="", align="left", width=2) %>%
#   define(var="placebo", label="Placebo", align="center", width=1, n=placebo_pop) %>%
#   define(var="drug", label="Drug", align="center",  width=1, n=drug_pop) %>%
#   define(var="overall", label="Overall", align="center", width=1, n=overall_pop, wrap=TRUE) %>%
#   spanning_header(span_cols=c("placebo", "drug"), label="Treatment Groups", label_align="center")


# Formats ----------------------------------------------------------------------


lowcase_parens <- function(x){
  
  ret <- paste0("(n=", x, ")")
  
  return(ret)
}

upcase_parens <- function(x){
  
  ret <- paste0("(N=", x, ")")
  
  return(ret)
  
}

lowcase_n <- function(x){
  
  ret <- paste0("n=", x)
  
  return(ret)
}

upcase_n <- function(x){
  
  ret <- paste0("N=", x)
  
  return(ret)
  
}

# Table Spec Functions ---------------------------------------------------------

table_spec <- function(x, n_format = upcase_parens, page_var = NULL){
  
  ret <- structure(list(), class = c("table_spec", "list"))
  
  # if (!orientation %in% c("landscape", "portrait"))
  # {
  #   stop(paste("ERROR: orientation parameter on page_template() function is invalid: '", orientation,
  #              "'\n\tValid values are: 'landscape' or 'portrait'."))
  #   
  # } 
  # 
  # if (!font_name %in% c("Courier New", "Times New Roman", "Arial", "Calibri"))
  # {
  #   stop(paste("ERROR: font_name parameter on page_template() function is invalid: '", font_name, 
  #              "'\n\tValid values are: 'Arial', 'Calibri', 'Courier New', and 'Times New Roman'.", sep=""))
  #   
  # } 
  
  ret$data <- x
  ret$n_format <- n_format
  ret$page_var <- page_var
  ret$col_defs <- list()
  ret$col_spans <- list()
  
  return(ret)
  
}


define <- function(x, var, label = NULL, col_type = NULL, align="center", label_align=NULL, width=1, visible=TRUE, n = NULL){
  
  def <- list(var = var, 
              label= label,
              col_type = col_type,
              align = align, 
              label_align = ifelse(is.null(label_align), align, label_align),
              width = width, 
              visible = visible, 
              n = n)

  
  x$col_defs[[length(x$col_defs) + 1]] <- def

  return(x)  
}

spanning_header <- function(x, span_cols, label = "", label_align = "center", level = 1, n = NULL){
  
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
  for(col in dat){
    if (!is.null(attr(col, "label"))){
      v2 <- c(v2, attr(col, "label"))
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
  
  for(def in defs){
    
    if(!is.null(def$label))
      ls[[def$var]] <- def$label 
   
    if(!is.null(def$n) ){
      ls[[def$var]] <- paste0(ls[[def$var]], "\n",  nfmt(def$n))
   }
  }
  
  return(ls)
}


## pt = page_template
## rt = report_template
create_flextable <- function(rt){

  
  ret <- flextable(rt$data, theme_fun = NULL) %>%
         theme_normal(fontname = "Courier New") %>%
         set_header_labels(values=get_labels(rt$data, rt$col_defs, rt$n_format))
  
  for(def in rt$col_defs){
    ret <- width(ret, j=def$var, width=def$width)  
    ret <- align(ret, j=def$var, align = def$align, part = "body")
    ret <- align(ret, j=def$var, align = def$label_align, part = "header")
  }
  

  return(ret)
}



# Report Content Functions -----------------------------------------------------


report_content <- function(){
  
  ret <- structure(list(), class = c("report_content", "list"))
  

  return(ret)
  
}


add_content <- function(x, object, page_break="after"){
  

  
  if (page_break == "before")
    x[[length(x) + 1]] <- "page_break"
  
  x[[length(x) + 1]] <- object

  
  if (page_break == "after")
    x[[length(x) + 1]] <- "page_break"
  

  
  return(x)
}


c <- report_content() %>%
  add_content(1) %>%
  add_content(2)

c[length(c) + 1] <- 1

c[length(c) + 1] <- 2

# Usage ------------------------------------------------------------------------


# d <- tribble(~subjid, ~name, ~age,
#              "001-100", "Sam", 45,
#              "001-200", "Wendy", 23
# )
# attr(d$age, "label") <- "Age"
# 
# r <- report_table(d) %>%
#   define("subjid", "Subject ID", align="left") %>%
#   define("name", "Subject Name", width=1.25, align = "left", label_align = "center", n=2) %>%
#   define("age", n=5)
#   #spanning_header(c("subjid", "name"), label="First Span")
# 
# 
# 
# # print(r)
# 
# ft <- create_flextable(r)
# 
# 
# #ft <- width(ft, j="subjid", width=1)
# #ft <- width(ft, j="name", width=1.25)
# 
# print(ft)




