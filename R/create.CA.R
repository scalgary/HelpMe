#' Create CA object from df or file
#'
#' @param df
#' @param title
#' @param row.sup
#' @param graph
#' @param ncp
#' @param col.sup
#'
#' @return
#' @export
#'
#' @examples
create_CA_from_df<- function(df,title = NULL, row.sup = NULL, col.sup = NULL, graph = FALSE, ncp = 2) {
  mapdata <- as.data.frame(df[-1,-1], row.names = df[-1,1])
  result_ca <- CA(mapdata, row.sup = row.sup, col.sup = col.sup, graph = graph, ncp = ncp)
  if (is.null(title)) {title <- deparse(substitute(df))}
  result_ca$title <- title
  return(result_ca)
}

#' Title
#'
#' @param file_csv
#' @param folder
#' @param title
#' @param row.sup
#' @param graph
#' @param ncp
#' @param col.sup
#'
#' @return
#' @export
#'
#' @examples
create_CA_from_file <- function(file_csv, folder='.',title =NULL,
                                row.sup = NULL, col.sup = NULL, graph = FALSE, ncp = 2) {
  df <- read.file(file_csv =file_csv, folder= folder)
  df <- light.cleaning.names(df)
  if (is.null(title)) {title <- sub(".csv", "", basename(file_csv))}
  result_ca <- create_CA_from_df(df, title, row.sup, col.sup, graph, ncp )
}
