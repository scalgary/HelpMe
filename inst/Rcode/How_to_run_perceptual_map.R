
library(HelpMe)

###############Template Perceptual Map###############
file_csv <- "child_red.csv"
result_ca <- HelpMe::create_CA_from_file(file_csv,  #csv file could be a path
                                         folder =".", #folder where csv file could be
                                         title = "mine", #title if it is NULL it is csv file name
                                         row.sup = 1)  #mean remove first row of data
#useful if the first row of data is empty or contains total

#default will be save in working directory
#Robject saved
saveRDS(result_ca,paste0(result_ca$title,".rds"))
#save coordonnates CA
write.csv(result_ca$PM_coord,paste0(result_ca$title,"_coord.csv"), row.names = FALSE)
#save data used for CA
write.csv(result_ca$call$Xtot,paste0(result_ca$title,"_data.csv"), row.names = FALSE)

#if we want to visualize in R
HelpMe::plot_ISCA(result_ca)
plot(result_ca)
#

#if we want to export plot to pptx
# we need rvg and officer
library(rvg)
library(officer)


save_plot_pptx <- function(x, target, usetemplate = FALSE){
  res.ca <- x
  if (!inherits(res.ca, "CA")) stop("non convenient data")
  if (usetemplate) { doc_pptx <-officer::read_pptx(system.file(package = "HelpMe", "template/templateISC.pptx"))
  } else { doc_pptx <-officer::read_pptx()}
  doc_pptx %>%
    officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
    officer::ph_with(result_ca$title, location= ph_location_label(ph_label = "Title 4")) %>%
    officer::ph_with(rvg::dml(ggobj =  HelpMe::plot_ISCA(result_ca)),
                     location=ph_location_label(ph_label = "Content Placeholder 2"))  %>%
                       print(target)
}

save_plot_pptx(result_ca,"example.pptx")

#######################################
