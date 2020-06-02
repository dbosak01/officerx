library(scales)

cnt_pct_internal <- function(x, func, low_threshold = .01, low_label="< 1%", zero_label = "0.0%"){
  
  
  if (is.na(x)){
    ret = zero_label
  } else if(x > 0 & x < low_threshold){
    ret = low_label
  } else {
    ret = func(x)
  }
  
  return(ret)
}


cnt_pct <- function(a, b, precision = 0.1, low_threshold = .01, low_label="< 1%"){
  
  # Find length of precision
  p_length <- 0
  pos <- regexpr(pattern="\\.", as.character(precision))
  if (pos > 0)
    p_length <- nchar(substring(as.character(precision), pos + 1)) + 1
  
  zero_label = "0%"
  if (p_length > 1)
    zero_label = paste("0.", rep("0", p_length), "%", sep="")
  
  # Format Percentage column
  p <- format(sapply(b, cnt_pct_internal,
                     func=label_percent(accuracy=precision),
                     zero_label=zero_label, 
                     low_label = low_label),
              width=p_length + 4, justify = "right")
  
  # Concatenate count and percent
  ret <- paste(a, " (", p, ")", sep="")
  
  return(ret)
}



sex_fmt <- Vectorize(function(x){
  
  ret <- ""
  if (is.na(x)){
    ret <- "Unknown"
  } else {
    if (x == "F")
      ret <- "Female"
    else if(x == "M")
      ret <- "Male"
    else if(x == "O")
      ret <- "Other"
    else if(x == "UNK")
      ret <- "Unknown"
  }
  
  return(ret)
})

