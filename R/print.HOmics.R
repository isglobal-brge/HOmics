#' Print for an object of class HOmics
#'
#' @param res an HOmics object as obtained in HOmics or HOmics.meth()

#' @method print HOmics
#' @export

print.HOmics <- function(res)
{
    cat("Object of class HOmics \n")
   if(res$cont) {
     if (res$univ)  cat("A hierarchical logistic univariate model was fitted for each feature\n") else cat("A hierarchical logistic multivariate model was fitted for each group\n") 
   } else {
     if (res$univ)  cat("A hierarchical univariate model was fitted for each feature\n") else cat("A hierarchical multivariate model was fitted for each group\n") 
   }
    cat("\n")
    cat("Call: \n")
    print(res$call)
}
