
library(ggplot2)
library(magick)
library(geomtextpath)

data <- data.frame(Recall = c(13.5, 8.4, 
                              8.6, 11.4), 
                   Encoding = c("Land", "Land", 
                                "Water", "Water"),
                   Retrieval = c(0, 1, 
                                 0, 1))


encoding_plot <- ggplot(data, aes(x = Retrieval, y = Recall)) +
                 geom_point(size = 1) + 
   scale_x_continuous(breaks = c( 0, 1),
                      labels = c("Land",
                                 "Water"),
                      limits = c(-.3,1.3)) +
                  geom_segment(aes(x = 0, 
                                   y = 8.6, 
                                   xend = 1, 
                                   yend = 11.4),
                                   color = "blue",
                                   linetype = "dotdash",
                                   alpha = 1) +
                  geom_segment(aes(x = 0, 
                                    y = 13.5, 
                                    xend = 1, 
                                    yend = 8.4),
                                    color = "red",
                                    linetype = "dotdash",
                                    alpha = 1) +
                  ggtitle("Godden and Baddeley (1975) Results") +     
                  theme_classic() +
                  theme(
                        panel.background = element_rect(fill='transparent', color = NA), 
                        plot.background = element_rect(fill='transparent', color = NA), 
                        legend.background = element_rect(fill='transparent', color = NA),
                        legend.box.background = element_rect(fill='transparent', color = NA),
                        plot.title = element_text(hjust = 0.5)) +
                  annotate("text", x = .3, y = 13, 
                          label = "Encoding on \n Land",
                          size = 3) +   
                  annotate("text", x =, .05, y = 9.6, 
                          label = "Encoding \n under Water",
                          size = 3) 
                 

print(encoding_plot)

ggsave("presentations files/images/encoding_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")