# connected line plot 

d <- data.frame(education = c(">10yr", ">10yr", "<=10yr", "<=10yr"), 
                weight = c("overweight", "obesity", "overweight", "obesity"), 
                rr = c(1.26, 1.83, 0.92, 1.32), 
                lower = c(1, 1.44, 0.76, 1.09), 
                upper = c(1.6, 2.34, 1.11, 1.59))
d
# different color for education
# line for weight 


library(ggplot2)

p <- ggplot(data = d, aes(x = weight, y = rr, group = education, 
                          color = education))
p <- p + geom_line() 
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), 
                       width = 0.1,
                       position = position_dodge(0.05))
p <- p + theme_minimal()
p <- p + labs(title = "Your title", 
              x = "X axis title", 
              y = "Y axis title")
p
