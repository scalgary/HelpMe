

#' Title
#' @noRd
#' @param x
#' @param y
#' @param weight
#' @param mean1
#' @param collapse
#' @param bootn
#'
#' @return
#'
#'
#' @examples
wtd.cor <- function (x, y = NULL, weight = NULL, mean1 = TRUE, collapse = TRUE,
                      bootn = 1000)
{
  x <- as.matrix(x)
  xnm <- colnames(x)
  if (is.null(weight)) {
    weight <- rep(1, dim(x)[1])
  }

  if (mean1 == TRUE)
    weight <- weight/mean(weight, na.rm = TRUE)
  if (is.null(y)) {
    y <- x
  }
  y <- as.matrix(y)
  ynm <- colnames(y)
  if (is.null(xnm))
    xnm <- "X"
  if (is.null(ynm))
    ynm <- "Y"
  if (dim(x)[1] != dim(y)[1])
    stop("Cannot Correlate Variables of Different Lengths")

  n_size <- lapply(as.data.frame(x), function(x) lapply(as.data.frame(y),
                                                        function(y) onecor.wtd(x, y, weight)$n))
  materset <- lapply(as.data.frame(x), function(x) lapply(as.data.frame(y),
                                                          function(y) onecor.wtd(x, y, weight)$corcoef))
  est <- sapply(materset, function(q) sapply(q, function(g) g[1]))
  se <- sapply(materset, function(q) sapply(q, function(g) g[2]))
  tval <- sapply(materset, function(q) sapply(q, function(g) g[3]))
  pval <- sapply(materset, function(q) sapply(q, function(g) g[4]))
  out <- list(correlation = est, std.err = se, t.value = tval,
              p.value = pval, n_size = n_size)


  if (is.vector(est) & collapse == TRUE || (1 %in% dim(est)) &
      collapse == TRUE) {
    outpre <- out

    out <- matrix(unlist(out), ncol = 5, byrow = FALSE)

    nom <- xnm
    if (length(xnm) == 1)
      nom <- ynm
    rownames(out) <- nom
    colnames(out) <- names(outpre)
  }
  out
}


