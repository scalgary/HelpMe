##helper function to get 1 pptx with all the quadmaps
add_slide_pptx <- function(doc_pptx, i) {


  doc_pptx %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(names(plots)[i], location= officer::ph_location_type(type = "title")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=officer::ph_location_type(type="body"))
}
add_slide_pptx_template <- function(doc_pptx, i) {


  doc_pptx %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
    officer::ph_with(names(plots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design")  %>%
    officer::ph_with(names(plots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
    officer::ph_with(paste0(names(plots)[i]," - Leveraging  Strengths and Weaknesses")
                     , location= ph_location_label(ph_label = "Title 4")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=ph_location_label(ph_label = "Content Placeholder 2"))



}
custom_reduce <- function(mylist,use_template) {
  if (use_template){
    init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))

    # Votre objet initial
    result <- Reduce(add_slide_pptx_template, mylist, init = init_pptx)
    result <- result %>%
      officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
      officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4")) %>%
      officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
      officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4"))

  } else {
    init_pptx <- officer::read_pptx()  # Votre objet initial
    result <- Reduce(add_slide_pptx, mylist, init = init_pptx)

  }

  return(result)
}
