
#' Check officer and rvg before loading help functions
#' Without officer and rvg won't work!!
#' @return
#' @export
#'
#' @examples
#'
check_pptx.CA <- function() {

  check <- FALSE
  required_packages <- c("officer", "rvg")
  # Check if all required packages are installed
  if (all(sapply(required_packages, function(p) require(p, character.only = TRUE, quietly = TRUE)))) {
    # All packages are installed
    print("All additional required packages are installed.")
    print("No warranty - should work with officer 0.6.2 and rvg 0.3.3")

    check <- TRUE
  }

  if (check) {
    source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))
  }

  return(check)

}


