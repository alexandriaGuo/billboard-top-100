library(tidyverse)
library(patchwork)

plot_missing <- function(dataframe, percent = TRUE) {
  
  missing_patterns <- data.frame(is.na(dataframe)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  if (percent) {
    missing_patterns <- missing_patterns %>%
      mutate(count = 100 * count / nrow(dataframe))
  }
  
  main_data <- missing_patterns %>%
    rownames_to_column("pattern") %>%
    pivot_longer(cols=!c(pattern,count), names_to = "key")
  
  # sort by count, then alphabetically, then count again, to match ptop reorder()
  sorted <- sort(colSums(is.na(dataframe)), decreasing=TRUE)
  sorted <- sorted[order(factor(names(sorted), levels = sort(names(sorted))))] 
  sorted <- sort(sorted, decreasing=TRUE)
  
  main_data$key <- factor(main_data$key, levels=names(sorted), ordered=TRUE)
  main_data$pattern<- factor(main_data$pattern, levels=(nrow(missing_patterns):1), ordered=TRUE)
  
  if (percent) {sorted <- 100 * sorted / nrow(dataframe)}
  
  comp <- which(rowSums(missing_patterns[,-ncol(missing_patterns)])==0)
  if (length(comp) == 0) { comp <- 0 }
  
  pmain <- main_data %>%
    mutate(complete = ifelse(pattern==comp, TRUE, FALSE),
           value = ifelse(complete==TRUE, "complete", value)) %>%
    ggplot(aes(x=key, y=pattern, fill=factor(value))) +
    geom_tile(colour="white", alpha=0.8) +
    scale_colour_manual(values = c("complete" = "darkgray", 
                                   "TRUE" = "#9779bd", 
                                   "FALSE" = "gray"),
                        aesthetics = "fill") +
    theme(legend.position = "none", panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(x="variable", y="missing pattern")
  
  if (comp != 0) { pmain <- pmain + # adjustments due to grid starting at 0,0
    annotate("text", 
             x=ncol(missing_patterns)/2, 
             y=nrow(missing_patterns)+1-comp, 
             label="complete cases")}
  
  # top plot 
  ptop <- sorted %>%
    as.data.frame() %>%
    rename(count = ".") %>%
    rownames_to_column("key") %>%
    ggplot(aes(x=reorder(key,-count), y=count, ymin=0)) + 
    geom_col(fill="blue", alpha=0.4) +
    labs(x="", y=ifelse(percent, "% rows missing:", "num rows missing:"))
  
  # side plot
  pright <- missing_patterns %>%
    rownames_to_column("pattern") %>%
    mutate(complete = ifelse(pattern==comp, TRUE, FALSE)) %>%
    ggplot(aes(x=reorder(pattern, nrow(missing_patterns):1), y=count)) +
    geom_col(aes(alpha = complete), fill="blue") +
    scale_alpha_manual(values = c(0.4, 0.6)) +
    coord_flip() +
    labs(x="", y=ifelse(percent,"row %", "row count")) +
    theme(legend.position = "none")
  
  finalg <- ((ptop + plot_spacer() + plot_layout(ncol= 2, widths = c(2.8, 1.2))) / (pmain + pright + plot_layout(ncol = 2, widths = c(3,1)))) + plot_layout(nrow =2, heights =c(1,3))
  
  return(finalg)
}