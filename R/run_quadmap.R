
#' Title
#'
#' @param file_csv file name or could be path
#' @param folder path for file name
#' @param isHE is it an HE
#' @param masterbrand masterbrand pour HE
#' @param force argument pour ggrepel
#' @param max.overlaps argument pour ggrepel
#'
#' @return
#' @export
#'
#' @examples
run_quadmap <- function(file_csv, folder= NULL, isHE = FALSE, TBdetail = "T2B",
                        masterbrand = NULL, force = 10, max.overlaps = 10){
  df <- read.file(file_csv, folder)
  df_list <- split_data_frame(df)
  plots <- lapply(df_list, function(df) myplot_quadmap(df, isHE, masterbrand, force, TBdetail,max.overlaps ))
  # Set the names of the list elements based on the last column of each dataframe
  plots <- stats::setNames(plots, sapply(df_list, function(df) utils::tail(names(df), n = 1)))
  return(plots)
}

run_quadmap_BG <- function(file_csv, folder= NULL, isHE = FALSE, TBdetail = "T2B",
                        masterbrand = NULL, force = 10, max.overlaps = 10){
  df <- read.file(file_csv, folder)
  df_list <- split_data_frame(df)
  plots <- lapply(df_list, function(df) myplot_quadmap_BG(df, isHE, masterbrand, force, TBdetail,max.overlaps ))
  # Set the names of the list elements based on the last column of each dataframe
  plots <- stats::setNames(plots, sapply(df_list, function(df) utils::tail(names(df), n = 1)))
  return(plots)
}
