#' Title
#' @noRd
#' @param x
#' @param graph.type
#'
#' @return
#'
#'
#' @examples
plot_ISCA <- function(x, graph.type = c("ggplot","classic"), max.overlaps = 10, want_flip = FALSE) {
if (!inherits(x, "CA")) stop("non convenient data")
df_data <- x$PM_coord
df1 <- df_data[which(df_data$Type=="Brand"),]
df2 <- df_data[which(df_data$Type!="Brand"),]
rownames(df1) <- gsub("\\.", " ", rownames(df1))

df_data <- rbind(df1,df2)
if (want_flip ){
  names(df_data)[names(df_data) == "Dim 1"] <- "DimY"
 names(df_data)[names(df_data) == "Dim 2"] <- "DimX"}
else {
  names(df_data)[names(df_data) == "Dim 1"] <- "DimX"
  names(df_data)[names(df_data) == "Dim 2"] <- "DimY"}
graph.type <- match.arg(graph.type[1],c("ggplot","classic"))

lab.x <- paste("Inertia Explained"," (",format(x$eig[2,"cumulative percentage of variance"],nsmall=2,digits=2),"%)",sep="")
# Get the max and min of x and y with a bit of padding
xlims <- c(min(df_data[,"DimX"]), max(df_data[,"DimX"])) + c(-0.1, 0.1)
ylims <- c(min(df_data[,"DimY"]), max(df_data[,"DimY"])) + c(-0.1, 0.1)

gg_graph  <- ggplot2::ggplot(data=df_data, ggplot2::aes(x=df_data[,"DimX"],
                                                             y = df_data[,"DimY"],
                                                             shape = df_data[,"Type"],
                                                             label = rownames(df_data),
                                                             colour = df_data[,"Type"])) +
  ggplot2::theme_void() +
  ggplot2::geom_point(size = 1.4) + ggplot2::scale_color_manual(values=c("dimgrey","Blue")) +
  ggplot2::scale_shape_manual(values = c(16, 17)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey70") + ggplot2::coord_fixed(ratio = 1) +
  ggplot2::geom_vline(xintercept = 0, color = "grey70") +
  ggplot2::guides(color = "none", shape = "none") +
  ggrepel::geom_text_repel(ggplot2::aes(colour=df_data[,"Type"]),
                           box.padding = ggplot2::unit(0.15, "lines"),
                           force = 6, size = 2.6, max.overlaps = max.overlaps)

  #ggplot2::annotate("text", x = max(xlims), y = min(ylims), label = lab.x, hjust = 1, vjust = 1, size = 2.6)

if (graph.type == "ggplot")  return(gg_graph)
}




