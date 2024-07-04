

library(here)
library(tidyverse)

where <-"J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\AUTO"
data_ibn <- "J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\data_SAT_ANDROID_AUTO.csv"
wheresaved <- "C:\\Users\\Sandrine.Lebon01\\MAIN\\usatibn\\AR1_2"

get_count <- function(by="brand"){
read.csv(data_ibn) %>%
  group_by(!!! syms(by) ) %>%
  summarise(count=n()) %>% write.csv(file.path(wheresaved ,"countsat.csv"))}


get_count("country")



myfunction <- function(file) {
  df_summary <- readr::read_csv(file) %>% mutate(from =dirname(file)) %>%
    mutate(brandindex=paste0("Brand",index)) %>%
    mutate(brand = case_when(
      brandindex =="Brand0" ~ 'Category',
      .default =as.character(str_to_title(brand)))) %>%
    select(-valid.n)


  return(df_summary)

}

ll <- list.files(where,"modelfit",recursive = TRUE, full.names = TRUE)
all <- purrr::map_dfr(ll,myfunction)




summary_fit <- purrr::map_dfr(ll,myfunction)
summary_fit %>% write_csv(file.path(wheresaved,"summary_fitsat.csv"))


read_and_transform <-function(from,brandindex,brand){
  df <- readr::read_csv(list.files(here(from,brandindex),"driver",full.names=TRUE)) %>%
    select(Name,Label,Driver) %>%
    mutate(from=brand)
  return(df)
}


list_of_dataframes <- pmap(list(summary_fit$from,summary_fit$brandindex,summary_fit$brand),read_and_transform)




result <- dplyr::bind_rows(list_of_dataframes)

write.csv(result,file.path(wheresaved,"resultsat.csv"))
result %>% pivot_wider(id_cols = c("Name","Label"), names_from= from, values_from = Driver) %>%
  write.csv(file.path(wheresaved,"driverssat.csv"))



myplots <- run_quadmap("J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\quadmap.csv",max.overlaps = Inf)
save_quadmap_mypptx(myplots,file.path("C:\\Users\\Sandrine.Lebon01\\MAIN\\usatibn\\AR1_2","test2.pptx"))


inputData <- read.csv(file = inputFile, encoding = "latin1") %>%
  select(Driver,TB0,Category)
j<- 2
nbbrand <- 1
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
myplot <- ggplot(inputData, aes(x = inputData[, 2], y = inputData[, 3])) +
  theme(text=element_text(family="mono")) +
  geom_rect(
    aes(xmin = mean(inputData[, 2]), xmax = Inf,
        ymin = mean(inputData[, 3]), ymax = Inf),
    fill = "mediumseagreen"
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = mean(inputData[, j]),
        ymin = mean(inputData[, j + nbbrand]), ymax = Inf),
    fill = "lightgreen"
  ) +
  geom_rect(
    aes(xmin = mean(inputData[, j]), xmax = Inf,
        ymin = -Inf, ymax = mean(inputData[, j + nbbrand])),
    fill = "sandybrown"
  ) +
  geom_rect(
    aes(xmin = -Inf, xmax = mean(inputData[, j]),
        ymin = -Inf, ymax = mean(inputData[, j + nbbrand])),
    fill = "lightcoral"
  ) +
  geom_vline(
    aes(xintercept = mean(inputData[, j])),
    color = "grey50", alpha = 0.5
  ) +
  geom_hline(
    aes(yintercept = mean(inputData[, j + nbbrand])),
    color = "grey50", alpha = 0.5
  ) +
  geom_point(size = 2, color = "navy") +
  geom_text_repel(aes(label = inputData[, 1]), size = 2.5, box.padding = unit(0.35, "lines"),force = 10
  ) +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  labs(x = "Attribute Performance", y = "Attribute Importance") +

  scale_x_continuous(breaks = c(min(inputData[,j]),max(inputData[,j])),
                     labels = function(a) sprintf("%.1f%%", round(a, digits = 2))) +
  scale_y_continuous(breaks = c(min(inputData[,j + nbbrand]),max(inputData[,j + nbbrand])),
                     labels = function(a) sprintf("%.2f", round(a, digits = 2)))


myplot
