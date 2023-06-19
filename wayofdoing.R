
library(HelpMe)


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


save_plot_pptx <- function(x, target){
  res.ca <- x
  if (!inherits(res.ca, "CA")) stop("non convenient data")
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(res.ca$title, location= officer::ph_location_type(type = "title")) %>%
    officer::ph_with(rvg::dml(ggobj =  HelpMe::plot_ISCA(res.ca)),
                              location=officer::ph_location_type(type="body")) %>%
                       print(target)
}

save_plot_pptx(result_ca,"example.pptx")




officer::read_pptx() %>%
  officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
  officer::ph_with(result_ca$title, location= officer::ph_location_type(type = "title")) %>%
  officer::ph_with(rvg::dml(ggobj =  plot_ISCA(result_ca),
                   location=officer::ph_location_type(type="body")) %>%
  print("new.pptx")





data <- read.csv(file_csv)
mapdata <- as.data.frame(data[-1,-1], row.names = data[-1,1])
result_ca <- CA(mapdata, row.sup = NULL, graph = TRUE, ncp = 2)
HelpMe::plot_ISCA(result_ca)

result_ca_file <- HelpMe::create_CA_from_file(file_csv, folder =".", title = "mine",
                                              row.sup = 1)
gg <- plot_ISCA(result_ca_file)


officer::read_pptx(templateIBN) %>%
officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
  officer::ph_with(result_ca_file$title, location= officer::ph_location_type(type = "title")) %>%
  officer::ph_with(rvg::dml(ggobj =  plot_ISCA(result_ca_file)),
                   location=officer::ph_location_type(type="body")) %>%
  print("new.pptx")


plots <- HelpMe::run_quadmap("inst/extdata/dataquad.csv")


init_object <- officer::read_pptx()   # Votre objet initial
A <- function(pres, i) {

  pres %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(names(plots)[i], location= officer::ph_location_type(type = "title")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=officer::ph_location_type(type="body"))
}



result <- Reduce(A, seq_along(plots), init = init_object)

print(result, target ="new.pptx")


templateIBN <- system.file(package = "HelpMe", "template/templateIBN.pptx")


df <- HelpMe::read.file("child_red.csv")


library(HelpMe)
library(rvg)
library(officer)


file_csv <- "child_red.csv"
create_CA_from_df <- function(df,title = NULL, row.sup = NULL, graph = FALSE, ncp = 2) {
  mapdata <- as.data.frame(df[-1,-1], row.names = df[-1,1])
  result_ca <- CA(mapdata, row.sup = row.sup, graph = graph, ncp = ncp)
  if (is.null(title)) {title <- deparse(substitute(df))}
  result_ca$title <- title
  return(result_ca)
}

result <- create_CA_from_file(file_csv, row.sup = 1)

res2 <- create_CA_from_df(Rhone)
