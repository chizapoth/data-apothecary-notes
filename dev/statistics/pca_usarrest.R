# use USArrest data to verify a few concepts

USArrests
colnames(USArrests)

# mean and variance
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# PCA with prcomp ----

# by default data is centered
# we make it scale to sd=1
pc_res <- prcomp(USArrests, scale = T)
names(pc_res)

pc_res$center # the means
pc_res$scale # sd
pc_res$scale^2 # var

# pc loading (rotation)
pc_res$rotation

pc_res$x 
# scores: data %*% rotation (loading)

xxx <- as.matrix(scale(USArrests)) %*% pc_res$rotation
head(xxx)
head(pc_res$x)

# biplot
biplot(pc_res)

# what if only use the first two components

plot(pc_res$x[,1], pc_res$x[,2])
text(pc_res$x[,1], pc_res$x[,2], labels = rownames(pc_res$x))


# projection ----
phi1 <- pc_res$rotation[,1]
xxx <- as.matrix(scale(USArrests))
pc_res$x[1,]

find_projection(x_vec = xxx[1,], base_vec = phi1)

find_projection(x_vec = c(1,1,1), base_vec = c(0,1,0))
d <- USArrests[, c(1,2,3)]
d <- scale(d)
d
plot(d)
pc <- prcomp(d, scale = T)

# plot(x = 0, y = 0, ylim = c(-1, 10), xlim = c(-1, 10))
# segments(x0 = 0, y0 = 0, x1 = 3, y1 = 4)
# segments(x0 = 0, y0 = 0, x1 = 5, y1 = 3, col = 'red')
# points(x = 27/34*5, y = 27/34*3)

# data
x1 <- d[1,]
# basevec: pc1
pc1 <- pc$rotation[,1] # loading for pc1
pc1
x1 %*% pc1

sum(pc1^2) 
x_dot_u <- sum(x1 * pc1) # this is loading
u_dot_u <- sum(pc1 * pc1) # 1

# the projection is on the line of base_vec, and is a proportion
y_hat <- x_dot_u/u_dot_u * pc1 

pc$x[1,]

find_projection(x_vec = c(1,1,1), base_vec = c(0,1,0))
library(scatterplot3d)
library(plot3D)
# install.packages('plot3D')
scatterplot3d(x = x1[1],
              y = x1[2], 
              z = x1[3])

scatter3D(x = x1[1],
              y = x1[2], 
              z = x1[3], bty='g', ticktype = 'detailed',
          phi=0)

x0 <- 0
y0 <- 0
z0 <- 0
pc1
  
arrows3D(x0, y0, z0, -pc1[1], -pc1[2], -pc1[3],bty ="g", ticktype = "detailed")
points3D(x1[1], x1[2], x1[3], add = TRUE, col="darkred", 
         colkey = FALSE, pch = 19, cex = 1)

points3D(x0, y0, z0, add = TRUE, col="darkred", 
         colkey = FALSE, pch = 19, cex = 1)




