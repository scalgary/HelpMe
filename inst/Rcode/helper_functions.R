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



save_plot_pptx <- function(x, target = NULL, max.overlaps = 10, usetemplate = FALSE){
  res.ca <- x

  if (!inherits(res.ca, "CA")) stop("non convenient data")
    mytitle <- paste0(res.ca$title," Inertia Explained"," (",format(res.ca$eig[2,"cumulative percentage of variance"],nsmall=2,digits=2),"%)",sep="")
    if (is.null(target)) { target <- paste0(res.ca$title,".pptx")}
  if (usetemplate) {
    doc_pptx <-officer::read_pptx(system.file(package = "HelpMe", "template/templateISC.pptx")) %>%
      officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
      officer::ph_with( mytitle, location= ph_location_label(ph_label = "Title 4")) %>%
      officer::ph_with(rvg::dml(ggobj =  HelpMe:::plot_ISCA(res.ca, max.overlaps = max.overlaps)),
                       location=ph_location_label(ph_label = "Content Placeholder 2"))  %>%
      officer::remove_slide(index = 1)

  } else {
    doc_pptx <- officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(mytitle, location= officer::ph_location_type(type = "title")) %>%
      officer::ph_with(rvg::dml(ggobj =  HelpMe:::plot_ISCA(res.ca,max.overlaps = max.overlaps)),
                       location=officer::ph_location_type(type="body"))
  }
  print(doc_pptx, target)
}
