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
  mapdata <- as.data.frame(df[,-1], row.names = df[,1])
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


#' Title
#'
#' @param res.ca
#' @param where
#'
#' @return
#' @export
#'
#' @examples
#'
 export_CA <- function(res.ca, folder =NULL){
if (!inherits(res.ca, "CA")) stop("non convenient data")
#
if (is.null(folder)) {
  rds_path <- paste0(res.ca$title,".rds")
  coord_path <- paste0(res.ca$title,"_coord.csv")
  data_path <- paste0(res.ca$title,"_data.csv")
} else {
  if (!(dir.exists(folder))) {stop("The folder doesn't exist")}
  rds_path <- file.path(paste0(res.ca$title,".rds"))
  coord_path <- file.path(paste0(folder,res.ca$title,"_coord.csv"))
  data_path <- file.path(paste0(folder,res.ca$title,"_data.csv"))

}
  #useful if the first row of data is empty or contains total
#default will be save in working directory
#Robject saved
saveRDS(res.ca, rds_path)
#save coordonnates CA
write.csv(res.ca$PM_coord,coord_path, row.names = TRUE)
#save data used for CA
write.csv(res.ca$call$Xtot, data_path, row.names = TRUE)
}
