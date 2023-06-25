
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


  if (check) {
    source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))
  }

  return(check)

}


