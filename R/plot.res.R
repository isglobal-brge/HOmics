#' plots results in terms of 95CI
#'
#' @param res resultslist
#' @param element number or name of group of features to plot. Default = 1

#' @import dplyr
#' @import ggplot2
#' @export plot.res

plot.res <- function (res, element = 1)
{
  if (class(res)!="HOmics") stop("res must be an HOmics class object")
  if (!is.numeric(element) & !(element %in% names(res))) stop("element must be a numeric value or one of the element names of res")

   resi <- res[[element]]
   resi$feature <- factor(resi$feature, levels=sort(unique(resi$feature),decreasing = TRUE)) 
   resi <- resi %>% mutate(coef.=ifelse(sign(`97.5%`)== sign(`2.5%`), ifelse(sign(`2.5%`)==1,"pos","neg"),"ns"))
   resi$'coef.' <- factor(resi$'coef.',levels=c("neg","ns","pos"))
   
   colors <- c("neg"="#00BA38","ns"="darkgrey","pos"="#F8766D")
   p <- ggplot(data=resi) +
   geom_segment(aes(x=`2.5%`,y = feature, xend = `97.5%`,yend = feature, color = coef.),
                arrow = arrow(length = unit(0.15,"cm"), ends = 'both')) +
     geom_text(aes(x = `97.5%`, y = feature, label = n.eff, hjust = -0.3), 
               color = "black", size = 2) +
     geom_vline(linetype = 'dashed', xintercept = 0) +
     xlab ("95% credible interval (n.eff samples)") +
     scale_color_manual(values = colors) +
     theme(panel.border = element_blank(),
           axis.text.y = element_text(size = 6),
           axis.ticks.y = element_blank())
   p

}
