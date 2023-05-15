library(reshape2)
library(ggplot2)
library(corrplot)
library(cowplot)


hist_plot <- 
ggplot(
    iris, 
    aes(Sepal.Width, fill = Species)
) +
geom_histogram(
    alpha = .3,
    binwidth = 0.2,
    color = 'azure4'
)+
labs(title = "A. ",
    subtitle = "sbt1",
    x = "sepal width"
) + 
theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
)
hist_plot  

dense_plot <- 
ggplot(
    iris, 
    aes(Sepal.Width, fill = Species)
) +
geom_density(alpha=0.2
)+
labs(title = "B.",
    subtitle = "sbt2",
    x = "sepal width"
) + 
theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
)
dense_plot  

scatter_plot <- ggplot(iris, aes(x = Sepal.Width, y= Petal.Width, color = Species)) +
             geom_point(size = 3) +
             labs(title = "C. ",
                 subtitle = "sbt 3",
                 x = "Sepal width",
                 y = "Petal width")

scatter_plot

box_and_jitter <- ggplot(iris, aes(y = Sepal.Width, x = Species, fill = Species)) + 
                geom_boxplot(alpha = 0.7) +
                geom_jitter(size=0.5) + 
                labs(title = "D.",
                     subtitle = "sbt3",
                     x = "Species",
                     y = "SW") +
                theme(axis.text.x = element_text(angle = 90))

box_and_jitter

melted_cormat <- melt(cor(USArrests,method ="spearman"))

hm_plot <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(colour=c("blue", "green", "green", "green")[melted_cormat$Var1]), 
    axis.text.y = element_text(colour=c("blue", "green", "green", "green")[melted_cormat$Var1])) +
  labs(title = "USArrests corr", subtitle = "sbt4")
hm_plot


pg <- plot_grid(hist_plot, dense_plot, scatter_plot, box_and_jitter, ncol=2, nrow=2)
ggsave("hw4_4plots.png",dpi=300, width=15, height = 15)

pg <- plot_grid(hm_plot)
ggsave("hw4_hm.png",dpi=300, width=5, height = 5)