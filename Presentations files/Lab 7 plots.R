
library(ggplot2)
library(magick)
library(geomtextpath)

data <- data.frame(Recall = c(13.5, 8.4, 
                              8.6, 11.4), 
                   Encoding = c("Land", "Land", 
                                "Water", "Water"),
                   Retrieval = c("Land", "Water", 
                                 "Land", "Water"))


encoding_plot <- ggplot(data, aes(x = Retrieval, y = Recall)) +
                 geom_point() + 
                 theme_classic()

print(encoding_plot)

ggsave("presentations files/images/encoding_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")