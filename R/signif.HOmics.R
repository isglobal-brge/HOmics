#' returns only significative results (containing no 0s)
#'
#' @param res object of class HOmics, obtained from function HOmics
#' @param as.data.frame collapse as data.frame. Default = TRUE

#' @import purrr
#' @import dplyr
#' 
#' @method signif HOmics
#' @export 

signif.HOmics <- function (res,as.data.frame = T)
{
  if (class(res)!="HOmics") stop("res must be an HOmics class object")
 
  results <- res$results
  if(is.null(results)) stop("no results in res")
  
  res.f <- map(results, function(x) filter(x, sign(`97.5%`)== sign(`2.5%`)))
  res.f <- keep(res.f,function(x) nrow(x)>0)
  if (as.data.frame)  res.f <- bind_rows(res.f,.id="group")
  return(res.f)
    
}

