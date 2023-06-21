##helper function to get 1 pptx with all the quadmaps
###export to pptx needs officer and rvg
library(officer)
library(rvg)

#mylist is a list of plots
custom_pptx <- function(mylist,use_template = TRUE) {

  #check if argumnet 1 is a list
  if (!(inherits(mylist, "list"))) {stop("You need a list of plots")}
  all_ggplot <- sapply(mylist, function(x) inherits(x, "ggplot"))
  if (!(all(all_ggplot))) {
    stop("Not all elements are of class ggplot.")
  }

  all_have_names <- !any(is.null(names(mylist)))

  if (!(all_have_names)) {
    stop("All elements should have names.")
  }
  if (use_template){
    init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))
    add_slide_pptx_template <- function(doc_pptx, i, myplots = mylist) {
      doc_pptx %>%
        officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
        officer::ph_with(names(myplots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
        officer::add_slide(layout = "OnlyTitle", master = "Custom Design")  %>%
        officer::ph_with(names(myplots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
        officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
        officer::ph_with(paste0(names(myplots)[i]," - Leveraging  Strengths and Weaknesses")
                         , location= ph_location_label(ph_label = "Title 4")) %>%
        officer::ph_with(rvg::dml(ggobj = myplots[[i]]),
                         location=ph_location_label(ph_label = "Content Placeholder 2"))
    }
    # Votre objet initial
    result <- Reduce(add_slide_pptx_template, seq_along(mylist), init = init_pptx)
    result <- result %>%
      officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
      officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4")) %>%
      officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
      officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4"))

  } else {
    init_pptx <- officer::read_pptx()  # Votre objet initial
    add_slide_pptx <- function(doc_pptx, i, myplots = mylist) {
      doc_pptx %>%
        officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
        officer::ph_with(names(myplots)[i], location= officer::ph_location_type(type = "title")) %>%
        officer::ph_with(rvg::dml(ggobj = myplots[[i]]),
                         location=officer::ph_location_type(type="body"))
    }
    result <- Reduce(add_slide_pptx, seq_along(mylist), init = init_pptx)

  }

  return(result)
}
