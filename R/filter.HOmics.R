#' filters results using parameter thresholds
#'
#' @param res object of class HOmics, obtained from function HOmics
#' @param param probability to filter with p.pos and p.neg
#' @param threshold numerical, the threshold to select associated variables related to the specified param. Default is 0.95
#' @param as.data.frame collapse as data.frame. Default = TRUE

#' @import purrr
#' @import dplyr
#' @method filter HOmics
#' @export 



filter.HOmics <- function (res, param = "p.pos", threshold = 0.95, as.data.frame = T)
{
  if (class(res)!="HOmics") stop("res must be an HOmics class object")
  
  if (!param %in% c("p.pos","p.neg"))
    stop("param has to be one of the following parameters: p.pos or p.neg")
  
  results <- res$results
  if(is.null(results)) stop("no results in res")
  
  if (!(is.null(threshold))) {
    cat("notice that this is a probability, so values above threshold will be selected")
    
    res.f <- map(results, function(x) dplyr::filter(x,!!as.name(param)>threshold))
    res.f <- keep(res.f,function(x) nrow(x)>0)
    if (as.data.frame)  res.f <- bind_rows(res.f,.id="group")
    return(res.f)
    
  } 
}

