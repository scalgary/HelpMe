#' from rvg packages
#'
#' @param code
#' @param ggobj
#' @param bg
#' @param fonts
#' @param pointsize
#' @param editable
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dml <- function(code, ggobj = NULL,
                bg = "white", fonts = list(),
                pointsize = 12, editable = TRUE, ...) {
  out <- list()
  out$code <- enquo(code)
  out$ggobj <- ggobj
  out$bg <- bg
  out$fonts <- fonts
  out$pointsize <- pointsize
  out$editable <- editable
  class(out) <- "dml"
  return(out)
}
