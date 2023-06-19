# example pour function CA
# example pour function create_CA_from_df
# example pour function create_CA_from_file

library(HelpMe)
library(rvg)
library(officer)

file_csv <- "child_red.csv"
data <- read.csv(file_csv)
mapdata <- as.data.frame(data[-1,-1], row.names = data[-1,1])
result_ca <- CA(mapdata, row.sup = NULL, graph = TRUE, ncp = 2)
HelpMe::plot_ISCA(result_ca)

result_ca_file <- HelpMe::create_CA_from_file(file_csv, folder =".", title = "mine",
                                              row.sup = 1)
gg <- plot_ISCA(result_ca_file)


officer::read_pptx() %>%
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


path <- system.file(package = "HelpMe", "template/dataquad.csv")


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
