#' Title
#'
#' @param d
#' @param NaNtoNA
#' @param ...
#'
#' @return
#' @export
#' @importFrom haven read_sav
#' @examples
readSPSS <- function(d, NaNtoNA = FALSE, ...) {
  d <- haven::read_sav(d, encoding = "UTF-8")
  if (NaNtoNA) {
    tmp <- d
    tmp[] <- lapply(d, function(x) {
      x[is.nan(x)] <- NA
      x
    })
    d <- tmp
    rm(tmp)
  }
  data.frame(d, stringsAsFactors = FALSE)
}
