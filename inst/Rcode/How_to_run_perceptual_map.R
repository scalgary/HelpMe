
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
source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))

save_plot_pptx(result_ca,"example.pptx")

#######################################
