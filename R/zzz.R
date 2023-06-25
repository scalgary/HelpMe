#' Title
#' @noRd
#' @param libname
#' @param pkgname
#'
#' @return
#'
#'
#' @examples
.onLoad <- function(libname, pkgname) {
  check <- FALSE
  required_packages <- c("officer", "rvg")

  # Check if all required packages are installed
  if (all(sapply(required_packages, function(p) require(p, character.only = TRUE, quietly = TRUE)))) {
    # All packages are installed
    print("All additional required packages are installed.")
    print("No warranty - should work with officer 0.6.2 and rvg 0.3.3")

    check <- TRUE
  } else {
    # At least one package is not installed
    missing_packages <- required_packages[!sapply(required_packages, function(p) require(p, character.only = TRUE, quietly = TRUE))]
    print(paste("The following packages are missing:", paste(missing_packages, collapse = ", ")))
    print("You can still get the coordinates and plot of the perceptual map but you can't export to PPTX ")
  }

  if (check) {
    source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))
    print("You can now use the function save_plot_pptx to export you CA object")
  }



}
