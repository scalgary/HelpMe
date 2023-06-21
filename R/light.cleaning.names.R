
#' Light cleaning
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' names(Rhone)
#' names(light.cleaning.names(Rhone))
light.cleaning.names <- function(df) {
names(df) <- trimws(names(df))

# Mettre la première lettre en majuscule et le reste en minuscule
names(df) <- sapply(names(df), function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  substr(x, 2, nchar(x)) <- tolower(substr(x, 2, nchar(x)))
  return(x)
})

# Assume that df is your dataframe
names(df) <- gsub("_", " ", names(df))

return(df)
}
