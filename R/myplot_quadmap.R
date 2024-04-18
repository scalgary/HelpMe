#' Title
#' @noRd
#' @param inputData data for quadmap 3 columns: label, performance and impact
#' @param isHE map for HE
#' @param masterbrand if it is HE
#' @param force argument for ggrepel
#' @param max.overlaps argument for ggrepel
#' @param
#'
#' @return
#'
#'
#' @examples
myplot_quadmap <- function(inputData, isHE = FALSE,
                           masterbrand = NULL, force = 10, TBdetail = "TB", max.overlaps = 10){
  myplot <- ggplot2::ggplot(inputData, ggplot2::aes(x = inputData[, 2],
                                                    y = inputData[, 3])) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = mean(inputData[, 2]), xmax = Inf,
                   ymin = mean(inputData[, 3]), ymax = Inf),
      fill = "mediumseagreen"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = mean(inputData[, 2]),
                   ymin = mean(inputData[, 3]), ymax = Inf),
      fill = "lightgreen"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = mean(inputData[, 2]), xmax = Inf,
                   ymin = -Inf, ymax = mean(inputData[, 3])),
      fill = "sandybrown"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = mean(inputData[, 2]),
                   ymin = -Inf, ymax = mean(inputData[, 3])),
      fill = "lightcoral"
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = mean(inputData[, 2])),
      color = "grey50", alpha = 0.5
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = mean(inputData[, 3])),
      color = "grey50", alpha = 0.5
    ) +
    ggplot2::geom_point(size = 2, color = "navy") +
    ggrepel::geom_text_repel(ggplot2::aes(label = inputData[, 1]),
                             size = 2.5, box.padding = ggplot2::unit(0.35, "lines"),force = force, max.overlaps = max.overlaps) +
    ggplot2::theme(
                   axis.title = ggplot2::element_text(size = 9),
                   axis.text.x = ggplot2::element_text(size = 7),
                   axis.text.y = ggplot2::element_text(size = 7))

  if(!isHE){
    myplot <- myplot +
      ggplot2::scale_x_continuous(breaks = c(min(inputData[,2]),max(inputData[,2])),
                                  labels = function(a) sprintf("%.1f%%", round(a, digits = 2))) +
      ggplot2::scale_y_continuous(breaks = c(min(inputData[,3]),max(inputData[,3])),
                                  labels = function(a) sprintf("%.2f", round(a, digits = 2))) +
      ggplot2::labs(x = paste0("Attribute Performance ",TBdetail), y = "Attribute Importance")}


  if(isHE){
    myplot <- myplot +

      ggplot2::scale_x_continuous(breaks = c(min(inputData[,2]),max(inputData[,2])),
                                  labels = function(a) sprintf("%.2f", round(a, digits = 2))) +
      ggplot2::scale_y_continuous(breaks = c(min(inputData[,3]),max(inputData[,3])),
                                  labels = function(a) sprintf("%.2f", round(a, digits = 2)))+
      ggplot2::labs(x = paste0("Impact of ",masterbrand ," on Sub-brands"),
                    y = paste0("Impact of Sub-brands on ",masterbrand))
  }
  return(myplot)
}
