library(corrplot)
library(reshape2)
library(ggplot2)

hist1 <- hist(
    iris$Sepal.Width,
    col = "deepskyblue3", 
    border = 'azure3',
    main = "Sepal Width",
    
    sub = "hist",
    xlab = "Width",
    ylab = "Counts",
    breaks = 30, 
    lwd = 2,
    cex.axis = 2
)

dens1 <- plot(
    density(iris$Sepal.Widt),
    frame = FALSE,
    col = "deepskyblue3",  
    xlab = "Width",
    ylab = "Counts",
    main = "Sepal Width",
    sub = "dens",
    lwd = 2,
    cex.axis = 2
)

my_cols <- c("deepskyblue3", "goldenrod1", "green3")
my_pch <- c(1, 2, 5)

scatter1 <- plot(
    iris$Petal.Width, 
    iris$Sepal.Width,  
    main = "Plot petal width over sepal width",  
    sub = "scatter",  
    xlab = "Petal Width", 
    ylab = "Sepal Width",  
    cex = 3, 
    pch = my_pch[iris$Species],
    col = my_cols[iris$Species]
)

boxplot(
    iris$Sepal.Width ~ iris$Species,
    main = "Plot of width by species",
#     las=2,
    xlab = "sepal width",
    ylab = "species", 
    col = my_cols, 
    yaxt = "n"
)

stripchart(
    iris$Sepal.Width ~ iris$Species,
    vertical = TRUE, 
    method = "jitter", 
    add = TRUE, 
    pch = 21, 
    col = my_cols, 
    yaxt = "n",
    cex=2
)

corrplot(cor(USArrests,method ="spearman"), method = 'color', tl.col=c("blue", "green", "green", "green"), tl.cex = 1.5)

dev.new()
png('hw3_4plots.png', res = 300, width = 2600, height = 2800)
par(mfrow=c(2,2))

hist1 <- hist(
    iris$Sepal.Width,
    col = "deepskyblue3", 
    border = 'azure3',
    main = "Sepal Width",
    sub = "hist",
    xlab = "Width",
    ylab = "Counts",
    breaks = 30, 
    lwd = 2,
    cex.axis = 2
)
mtext("A.",
      side = 3, adj = -0.5, line = 3)

dens1 <- plot(
    dens,
     frame = FALSE,
    col = "deepskyblue3",  
    xlab = "Width",
    ylab = "Counts",
    main = "Sepal Width",
    sub = "dens",
    lwd = 2,
    cex.axis = 2
)
mtext("B.",
      side = 3, adj = -0.5, line = 3)

scatter1 <- plot(
    iris$Petal.Width, 
    iris$Sepal.Width,  
    main = "Plot petal width over sepal width",  
    sub = "scatter",  
    xlab = "Petal Width", 
    ylab = "Sepal Width",  
    cex = 3, 
    pch = my_pch[iris$Species],
    col = my_cols[iris$Species]
)
mtext("C.",
      side = 3, adj = -0.5, line = 3)

boxplot(
    iris$Sepal.Width ~ iris$Species,
    main = "Plot of width by species",
#     las=2,
    xlab = "sepal width",
    ylab = "species", 
    col = my_cols, 
    yaxt = "n"
)
stripchart(
    iris$Sepal.Width ~ iris$Species,
    vertical = TRUE, 
    method = "jitter", 
    add = TRUE, 
    pch = 21, 
    col = my_cols, 
    yaxt = "n",
    cex=2
)

mtext("D.",
      side = 3, adj = -0.5, line = 3)
dev.off()

dev.new()
tiff("hw3_hm.tif", res = 300,  width = 2600, height = 2800)
corrplot(cor(USArrests,method ="spearman"), method = 'color', tl.col=c("blue", "green", "green", "green"), tl.cex = 1.5)
dev.off()