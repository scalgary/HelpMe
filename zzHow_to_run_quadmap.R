
library(HelpMe)

###############Template QuadMap###############

file_csv <- "dataquad.csv" #your file 2n +1 columns
#1st column label
# 2 to n+1 Performance
#n+2 to 2n+1 impact
#columns impact give name quadmap

plots <- HelpMe::run_quadmap(file_csv)

#visualisation
plots

###export to pptx needs officer and rvg
library(officer)
library(rvg)
use_template <- FALSE
##helper function to get 1 pptx with all the quadmaps

gather <- function(use_template){
  if (use_template) {

  }
}


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
#######
#no template
init_pptx <- officer::read_pptx()  # Votre objet initial

result1 <- Reduce(add_slide_pptx, seq_along(plots), init = init_pptx)
print(result1, target ="new_1.pptx")


####with template
use_template <- TRUE
init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))  # Votre objet initial
result2 <- Reduce(add_slide_pptx, seq_along(plots), init = init_pptx)
print(result2, target ="new_2.pptx")



add_slide_ppt <- function(doc_pptx, i, use_template = FALSE) {

  if (use_template){
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
  } else {
    doc_pptx %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(names(plots)[i], location= officer::ph_location_type(type = "title")) %>%
      officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                       location=officer::ph_location_type(type="body"))
    }


}
#######

init_pptx <- officer::read_pptx()  # Votre objet initial

result1 <- Reduce(add_slide_pptx, seq_along(plots), init = init_pptx)
print(result1, target ="new1.pptx")

init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))  # Votre objet initial

result2 <- Reduce(add_slide_pptx_template, seq_along(plots), init = init_pptx)
print(result2, target ="new2.pptx")


