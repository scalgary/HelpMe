
 where <-"L:/23-BrandPulse-AR112-01/likely/"
#library(here)
library(tidyverse)

ll <- list.files(where,"modelfit",recursive = TRUE, full.names = TRUE)
myfunction <- function(file) {
  df <- readr::read_csv(file) %>% mutate(from =str_replace_all(dirname(file),"[/.]","")) %>%
    mutate(brandindex=paste0("Brand",index))



}


purrr::map_dfr(ll,myfunction) %>% write_csv(file.path(where,"summary_fit.csv"))
summary_fit <- readr::read_csv(file.path(where,"summary_fit.csv"))

mygather <-function(from,brandindex,brand){
  df <- readr::read_csv(list.files(here(from,brandindex),"driver",full.names=TRUE)) %>%
    select(Name,Label,Driver) %>% mutate(from=paste0(from,"_",brand))
  return(df)
}

list_of_dataframes <- mapply(mygather,summary_fit$from,summary_fit$brandindex,summary_fit$brand, SIMPLIFY = FALSE)

result <- dplyr::bind_rows(list_of_dataframes) %>% pivot_wider(id_cols = c("Name","Label"),names_from = from,
                                                               values_from = Driver) %>%
  write.csv(file.path(where,"all_impacts.csv"))
