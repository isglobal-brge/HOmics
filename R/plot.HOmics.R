#' plots results in terms of 95\% credible interval
#'
#' @param res object of class HOmics, obtained from function HOmics
#' @param element number or name of group of features to plot. Default = 1

#' @import dplyr
#' @import ggplot2
#' 
#' @method plot HOmics
#' @export 

plot.HOmics <- function (res, element = 1)
{
  if (class(res)!="HOmics") stop("res must be an HOmics class object")
  
  if (!is.numeric(element) & !(element %in% names(res))) stop("element must be a numeric value or one of the element names of res")
  
  results <- res$results
  if(is.null(results)) stop("no results in res")
  
  resi <- results[[element]]
  size <- nrow(resi) 
  size.text <- ifelse(size < 10,8,ifelse(size < 50,6,5))
  
  cont <- res$cont
  tit <- ifelse(cont,"95% credible interval (n.eff samples)","log-odds 95% credible interval (n.eff samples)")
    resi$feature <- factor(resi$feature, levels=sort(unique(resi$feature),decreasing = TRUE)) 
    resi <- resi %>% mutate(coef.=ifelse(sign(`97.5%`)== sign(`2.5%`), ifelse(sign(`2.5%`)==1,"pos","neg"),"ns"))
    resi$'coef.' <- factor(resi$'coef.',levels=c("neg","n.s.","pos"))
    offs <- max(resi$`97.5%`-resi$`2.5%`)*0.1
     
    colors <- c("neg"="#00BA38","ns"="darkgrey","pos"="#F8766D")
    p <- ggplot(data=resi) +
         geom_segment(aes(x=`2.5%`,y = feature, xend = `97.5%`,yend = feature, color = coef.),
                  arrow = arrow(length = unit(0.15,"cm"), ends = 'both')) +
        geom_text(aes(x = `97.5%`, y = feature, label = n.eff, hjust = -0.3), 
                 color = "black", size = size.text*0.352777778) +
        geom_vline(linetype = 'dashed', xintercept = 0) +
        xlab (tit) + xlim(min(resi$`2.5%`) - offs, max(resi$`97.5%`) + offs) +
        scale_color_manual(values = colors) +
        theme(panel.border = element_blank(),
             axis.text.y = element_text(size = size.text, color="black"),
             axis.ticks.y = element_blank())
 
  p

}
