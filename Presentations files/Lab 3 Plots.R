
# lot's of GGplotting follows...


library(spData)
library(sf)
library(ggplot2)
library(magick)
library(geomtextpath)
library(extrafont)
library(ggpubr)

# data

data <- data.frame( Age = c(11, 12, 13, 13, 14, 15),
                    Height = c(17, 18, 19, 19, 20, 20))

######### empty plot ########

empty_plot <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point(alpha = 0) +
   labs(title = "", x = "Age", y = "Height") +
   geom_point(alpha = 0) +
   theme_classic() +
   # age 11
   geom_segment(aes(x = 11, 
                    y = 17, 
                    xend = 11, 
                    yend = 17.2),
                color = "blue",
                linetype = "dotdash",
                alpha = 1) +
   # age 12
   geom_segment(aes(x = 12, 
                    y = 17, 
                    xend = 12, 
                    yend = 17.2),
                color = "blue",
                linetype = "dotdash",
                alpha = 1) +
   # age 13
   geom_segment(aes(x = 13, 
                    y = 17, 
                    xend = 13, 
                    yend = 17.4),
                color = "blue",
                linetype = "dotdash",
                alpha = 1) +
   # age 14
   geom_segment(aes(x = 14, 
                    y = 17, 
                    xend = 14, 
                    yend = 17.2),
                color = "blue",
                linetype = "dotdash",
                alpha = 1) +
   # age 15
   geom_segment(aes(x = 15, 
                    y = 17, 
                    xend = 15, 
                    yend = 17.2),
                color = "blue",
                linetype = "dotdash",
                alpha = 1) +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.y = element_blank(),
      axis.line.y = element_line(color = "transparent"),
      axis.ticks.y = element_blank())


#print(empty_plot)

# Print the scatter plot

ggsave("images/empty_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")





######## mean plot #######

mean_plot <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point(alpha = 0) +
   labs(title = "", x = "Age", y = "Height") +
   geom_vline(xintercept = mean(data$Age),
              linetype = "dotted",
              color = "red") +
   geom_point(alpha = 0) +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.y = element_blank(),
      axis.line.y = element_line(color = "transparent"),
      axis.ticks.y = element_blank(),
      text = element_text(family = "A")) +
   annotate("text", x = 13.3, y = 19, label = "Mean Age")



# print(mean_plot)

# Print the scatter plot

ggsave("images/mean_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")



######## sd plot  ########

sd_plot <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point(alpha = 0) +
   labs(title = "", x = "Age", y = "Height") +
   geom_vline(xintercept = mean(data$Age),
              linetype = "dotted",
              color = "red",
              alpha = .5) +
   geom_segment(aes(x = 13 - sd(Age), 
                    y = 18 , 
                    xend = 13 + sd(Age), 
                    yend = 18),
                color = "maroon",
                linetype = "dashed",
                arrow = arrow( ends = "both"),
                alpha = 1) +
   geom_point(alpha = 0) +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.y = element_blank(),
      axis.line.y = element_line(color = "transparent"),
      axis.ticks.y = element_blank()) +
   annotate("text", x = 13 + sd(data$Age), y = 18.5, label = "1 SD\n above the mean") +
   annotate("text", x = 13 - sd(data$Age), y = 18.5, label = "- 1 SD\n below the mean")





# print(sd_plot)

# Print the scatter plot

ggsave("images/sd_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


######## empty scatterplot ########


empty_scatter_plot <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point(alpha = 0) + 
   labs(title = "", x = "Age", y = "Height") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   )

# Print the scatterplot

#print(empty_scatter_plot)

ggsave("images/empty_scatter_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")

######## scatterplot #######


scatter_plot <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point() +  
   labs(title = "", x = "Age", y = "Height") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
   )

# Print the scatterplot

#print(scatter_plot)

ggsave("images/scatter_plot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


####### cor scatterplot #######

scatter_plot_cor <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point() +  
   stat_cor(aes(label = ..r.label..)) +
   labs(title = "", x = "Age", y = "Height") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
   )

# Print the scatterplot

#print(scatter_plot_cor)

ggsave("images/scatter_plot_cor.png",
       width = 7, height = 4.5, dpi = 300, units = "in")

####### regression scatterplot #######


scatter_plot_reg <- ggplot(data, aes(x = Age, y = Height)) +
   geom_point() +  
   stat_smooth(method = "lm",
               formula = y ~ x,
               geom = "smooth",
               se = FALSE,
               linetype = "dashed",
               color = "red",
               linewidth = .5) +
   stat_regline_equation(label.x = 13, label.y = 17.4) +
   labs(title = "", x = "Age", y = "Height") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')) +
   ylim(17, 20)

# Print the scatterplot

#print(scatter_plot_reg)

ggsave("images/scatter_plot_reg.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


##### Barplot t test ######

age_t_test <- c(14, 15, 15, 16, 30, 31, 31, 32)

data <- data.frame(
   groups = c("Group1", "Group2"),
   means = c( 15, 31),
   SEs = c(sd(age_t_test[1:4])/2, sd(age_t_test[5:8])/2))

bar_colors <- c("red", "blue")

# Create the bar plot

barplot <- ggplot(data, aes(x = groups, 
                            y = means, 
                            fill = groups)) +
   geom_bar(stat = "identity", 
            position = "dodge", 
            show.legend = FALSE,
            width = 0.2,
            colour="black",
            alpha = .7) +
   geom_errorbar(aes(ymin = means - SEs, ymax = means + SEs), position = position_dodge(width = 0.9), width = 0.05) +
   labs(x = "Groups",
        y = "Mean Age") +
   scale_fill_manual(values = c("red", "blue")) + 
   theme_classic()+
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'))+
   scale_y_continuous(expand=c(0,0))

# Print the plot
#print(barplot)

ggsave("images/barplot.png",
       width = 7, height = 4.5, dpi = 300, units = "in")




##### Scatterplot t-test ######


data <- data.frame(
   Age = c(14, 15, 15, 16, 30, 31, 31, 32),
   Group = c(0, 0, 0, 0, 1, 1, 1, 1), 
   label = c("Group 1", "Group 1", "Group 1", "Group 1", 
             "Group 2", "Group 2", "Group 2", "Group 2")
)


scatterplot_t_test <- ggplot(data, aes(x = Group, y = Age)) +
   geom_point(aes(color = label), 
              show.legend = FALSE) +  
   labs(title = "", x = "Groups", y = "Age") +
   scale_color_manual(values = c("red","blue"))+
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   ) +
   annotate("text", x = 0.05, y = 17, label = "Group 1") +
   annotate("text", x = 1, y = 33, label = "Group 2")

#print(scatterplot_t_test)

ggsave("images/scatterplot_t_test.png",
       width = 7, height = 4.5, dpi = 300, units = "in")



##### Scatterplot t-test regression ######

scatterplot_t_test_regression <- ggplot(data, aes(x = Group, y = Age)) +
                                 geom_point(alpha = 0) +  
                                 stat_smooth(method = "lm",
                                             formula = y ~ x,
                                             geom = "smooth",
                                             se = FALSE,
                                             linetype = "dashed",
                                             color = "red",
                                             linewidth = .5) +
                                  stat_regline_equation(label.x = .5, label.y = 20) +
                                  labs(title = "", x = "Groups", y = "Age") +
                                  theme_classic() +
                                  theme(
                                        panel.background = element_rect(fill='transparent'), 
                                        plot.background = element_rect(fill='transparent', color=NA), 
                                        legend.background = element_rect(fill='transparent'),
                                        legend.box.background = element_rect(fill='transparent')) +
                                  ylim(14, 33) +
                                  xlim(0, 1)


# print(scatterplot_t_test_regression)

ggsave("images/scatterplot_t_test_regression.png",
       width = 7, height = 4.5, dpi = 300, units = "in")

# t-test results corresponds to significance of regression coefficients significance

#summary(lm(Age ~ Group, data = data))




#######Scatter plot One-way ANOVA######

data <- data.frame(
   Age = c(14, 15, 15, 16, 
           30, 31, 31, 32, 
           32, 33, 33, 34),
   
   Group = c(0, 0, 0, 0, 
             1, 1, 1, 1, 
             2, 2, 2, 2), 
   
   label = c("Group 1", "Group 1", "Group 1", "Group 1", 
             "Group 2", "Group 2", "Group 2", "Group 2",
             "Group 3", "Group 3", "Group 3", "Group 3")
)



# scatter plot ANOVA

scatterplot_ANOVA <- ggplot(data, aes(x = Group, y = Age)) +
   geom_point(aes(color = label), 
              show.legend = FALSE) +  
   labs(title = "", x = "Groups", y = "Age") +
   scale_color_manual(values = c("red","blue", "orange"))+
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   ) +
   annotate("text", x = 0.05, y = 17, label = "Group 1") +
   annotate("text", x = 1, y = 33, label = "Group 2") +
   annotate("text", x = 2, y = 35, label = "Group 3") 

print(scatterplot_ANOVA)

ggsave("images/scatterplot_ANOVA.png",
       width = 7, height = 4.5, dpi = 300, units = "in")




# scatter plot ANOVA reg line


scatterplot_ANOVA_reg <- ggplot(data, aes(x = Group, y = Age)) +
   geom_point(aes(color = label), 
              show.legend = FALSE,
              alpha = 0) +  
   geom_segment(x = 0,
                xend = 1,
                y = 15,
                yend = 31,
                color = "red",
                linetype = "dashed") +
   geom_segment(x = 1,
                xend = 2,
                y = 31,
                yend = 33,
                color = "red",
                linetype = "dashed") +
   labs(title = "", x = "Groups", y = "Age") +
   scale_color_manual(values = c("red","blue", "orange"))+
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent')) + 
   ylim(14, 35)

print(scatterplot_ANOVA_reg)

ggsave("images/scatterplot_ANOVA_reg.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


###### Two-Way ANOVA ######


# main effect of gender

data <- data.frame(Weight = c(60, 62, 75, 76, 
                              71, 72, 76, 77), 
                   Gender = c(0, 0, 0, 0,
                              1, 1, 1, 1),
                   Diet = c("A", "A", "B", "B", 
                            "A", "A", "B", "B"))

scatterplot_2_ANOVA_1 <- ggplot(data, aes(x = Gender, y = Weight)) +
   geom_point(show.legend = FALSE) +  
   geom_segment(x = 0,
                xend = 1,
                y = mean(data$Weight[1:4]),
                yend = mean(data$Weight[5:8]),
                color = "red",
                linetype = "dashed") +
   geom_point(aes(x=0, y = mean(Weight[1:4])), 
              colour="blue",
              shape=18,
              size =3) +
   geom_point(aes(x=1, y=mean(Weight[5:8])), 
              colour="orange",
              shape=18,
              size =3) +
   labs(title = "", x = "Gender", y = "Weight") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   ) +
   scale_x_continuous(breaks = c( 0, 1),
                      labels = c("Female",
                                 "Male"),
                      limits = c(-.05,1.05)) +
   annotate("text", x = 0, y = mean(data$Weight[1:4]) -1 , 
            label = "Mean \n Female Weight",
            size = 3) +
   annotate("text", x = .9, y = mean(data$Weight[5:8]) +1, 
            label = "Mean \n Male Weight",
            size = 3) 


print(scatterplot_2_ANOVA_1)

ggsave("images/scatterplot_2_ANOVA_1.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


# main effect of Diet


scatterplot_2_ANOVA_2 <- ggplot(data, aes(x = Gender, 
                                          y = Weight,
                                          color = Diet)) +
   geom_point(show.legend = FALSE,
              ) +
   scale_color_manual(values = c("magenta",
                                 "brown"))+
   geom_segment(x = .5,
                xend = .5,
                y = mean(data$Weight[c(1,2,5,6)]),
                yend = mean(data$Weight[c(3,4,7,8)]),
                color = "red",
                linetype = "dashed") +
   geom_point(aes(x=0.5, y = mean(Weight[c(1,2,5,6)])), 
              colour="magenta",
              shape=18,
              size =3) +
   geom_point(aes(x=.5, y = mean(Weight[c(3,4,7,8)])), 
              colour="brown",
              shape=18,
              size =3) +
   labs(title = "", 
        x = "Gender", 
        y = "Weight") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   ) +
   scale_x_continuous(breaks = c( 0, 1),
                      labels = c("Female",
                                 "Male"),
                      limits = c(-.05,1.05)) +
   annotate("text", x = .65, y = mean(data$Weight[c(1,2,5,6)]), 
             label = "Mean \n Diet A Weight",
             size = 3) +
    annotate("text", x = .35, y = mean(data$Weight[c(3,4,7,8)]) , 
             label = "Mean \n Diet B Weight",
             size = 3) 


print(scatterplot_2_ANOVA_2)

ggsave("images/scatterplot_2_ANOVA_2.png",
       width = 7, height = 4.5, dpi = 300, units = "in")


# Interaction effect


scatterplot_2_ANOVA_3 <- ggplot(data, aes(x = Gender, 
                                          y = Weight,
                                          color = Diet)) +
   geom_point(show.legend = FALSE,
   ) +
   scale_color_manual(values = c("magenta",
                                 "brown"))+
   geom_segment(y = mean(data$Weight[c(3,4)]),
                yend = mean(data$Weight[c(7,8)]),
                x = 0 ,
                xend = 1,
                color = "red",
                linetype = "dashed") +
   geom_segment(y = mean(data$Weight[c(1,2)]),
                yend = mean(data$Weight[c(5,6)]),
                x = 0 ,
                xend = 1,
                color = "red",
                linetype = "dashed") +
   geom_point(aes(x=0, y = mean(Weight[c(1,2)])), 
              colour="magenta",
              shape=18,
              size =2) +
   geom_point(aes(x=1, y = mean(Weight[c(5,6)])), 
              colour="magenta",
              shape=18,
              size =2) +
   geom_point(aes(x=0, y = mean(Weight[c(3,4)])), 
              colour="brown",
              shape=18,
              size =2) +
   geom_point(aes(x=1, y = mean(Weight[c(7,8)])), 
              colour="brown",
              shape=18,
              size =2) +
   labs(title = "", 
        x = "Gender", 
        y = "Weight") +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color=NA), 
      legend.background = element_rect(fill='transparent'), 
      legend.box.background = element_rect(fill='transparent') 
   ) +
   scale_x_continuous(breaks = c( 0, 1),
                      labels = c("Female",
                                 "Male"),
                      limits = c(-.05,1.05)) +
   annotate("text", x = 0, y = mean(data$Weight[c(1,2)]) + 3, 
            label = "Mean Weight \n Diet A X Females",
            size = 2) +
   annotate("text", x = 1, y = mean(data$Weight[c(5,6)]) - 2, 
            label = "Mean Weight \n Diet A X Males",
            size = 2) +
   annotate("text", x = 0, y = mean(data$Weight[c(3,4)]) - 2, 
            label = "Mean Weight \n Diet B X Females",
            size = 2) +
   annotate("text", x = 1, y = mean(data$Weight[c(7,8)]) - 2, 
            label = "Mean Weight \n Diet B X Males",
            size = 2) 


print(scatterplot_2_ANOVA_3)


ggsave("images/scatterplot_2_ANOVA_3.png",
       width = 7, height = 4.5, dpi = 300, units = "in")





#### p-values plot #####


# Z-test function

z_test <- function(s_mean, pop_mean, pop_sd, N)
{
   pnorm((s_mean - pop_mean)/(pop_sd/sqrt(N)), 
         mean = 0 ,
         sd = 1)
}

sample_size <- c(1:3000)
sd_pop <- 3
mean_pop <- 5
mean_1 <- 5.1
mean_2 <- 5.2
mean_3 <- 5.3
   

data <- data.frame(Sample_size = rep(sample_size, 
                                     3),
                   p_values = c(z_test(mean_pop, mean_1, sd_pop, N = sample_size),
                                z_test(mean_pop, mean_2, sd_pop, N = sample_size),
                                z_test(mean_pop, mean_3, sd_pop, N = sample_size)),
                   Difference = rep(c("Difference = .1", 
                                      "Difference = .2",
                                      "Difference = .3"), 
                           each = length(sample_size)))


# no lines

p_plot_empty <- ggplot(data, aes(x = Sample_size, y = p_values, color = Difference)) +
   geom_line(show.legend = FALSE,
             alpha = 0) +  # Use geom_line() for a line plot or geom_point() for points
   labs(title = "How many People for Significance?",
        x = "Sample size",
        y = "p-value") +
   geom_hline(yintercept = .05, 
              linetype = "dashed",
              color = "red") +
   scale_color_manual(values = c("blue",
                                 "orange",
                                 "brown")) +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent', color = NA), 
      plot.background = element_rect(fill='transparent', color = NA), 
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA),
      legend.position = c(.75, .75),
      plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = 1000, y = 0.08, label = "p < .05")+
   ylim(0, .5) 


# Print the plot
print(p_plot_empty)


ggsave("images/p_plot_empty.png",
       width = 7, height = 4.5, dpi = 300, units = "in")



# one line

p_plot_1l <- ggplot(data[1:3000,], aes(x = Sample_size, y = p_values, color = Difference)) +
   geom_line() +  # Use geom_line() for a line plot or geom_point() for points
   labs(title = "How many People for Significance?",
        x = "Sample size",
        y = "p-value") +
   geom_hline(yintercept = .05, 
              linetype = "dashed",
              color = "red") +
   scale_color_manual(values = c("blue")) +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent', color = NA), 
      plot.background = element_rect(fill='transparent', color = NA), 
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA),
      legend.position = c(.75, .75),
      plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = 1000, y = 0.08, label = "p < .05")+
   ylim(0, .5)

# Print the plot
print(p_plot_1l)


ggsave("images/p_plot_1l.png",
       width = 7, height = 4.5, dpi = 300, units = "in")




# two lines 


p_plot_2l <- ggplot(data[1:6000,], aes(x = Sample_size, y = p_values, color = Difference)) +
   geom_line() +  # Use geom_line() for a line plot or geom_point() for points
   labs(title = "How many People for Significance?",
        x = "Sample size",
        y = "p-value") +
   geom_hline(yintercept = .05, 
              linetype = "dashed",
              color = "red") +
   scale_color_manual(values = c("blue",
                                 "orange")) +
   theme_classic() +
   theme(
      panel.background = element_rect(fill='transparent', color = NA), 
      plot.background = element_rect(fill='transparent', color = NA), 
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA),
      legend.position = c(.75, .75),
      plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = 1000, y = 0.08, label = "p < .05")+
   ylim(0, .5) 


# Print the plot
print(p_plot_2l)


ggsave("images/p_plot_2l.png",
       width = 7, height = 4.5, dpi = 300, units = "in")



# full plot

   p_plot_full <- ggplot(data, aes(x = Sample_size, y = p_values, color = Difference)) +
      geom_line() +  # Use geom_line() for a line plot or geom_point() for points
      labs(title = "How many People for Significance?",
           x = "Sample size",
           y = "p-value") +
      geom_hline(yintercept = .05, 
                 linetype = "dashed",
                 color = "red") +
      scale_color_manual(values = c("blue",
                                    "orange",
                                    "brown")) +
      theme_classic() +
      theme(
         panel.background = element_rect(fill='transparent', color = NA), 
         plot.background = element_rect(fill='transparent', color = NA), 
         legend.background = element_rect(fill='transparent', color = NA),
         legend.box.background = element_rect(fill='transparent', color = NA),
         legend.position = c(.75, .75),
         plot.title = element_text(hjust = 0.5)) +
      annotate("text", x = 1000, y = 0.08, label = "p < .05")+
      ylim(0, .5)
   
   
   
# Print the plot
print(p_plot_full)


ggsave("images/p_plot_full.png",
       width = 7, height = 4.5, dpi = 300, units = "in")





