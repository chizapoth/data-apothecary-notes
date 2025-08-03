# NCI60 dataset

# 64 by 6830, gene expression values
# 64 cell lines of cancer types

library(ISLR)
NCI60
?NCI60
nci.labs <- NCI60$labs # Sample labels (tissue type)
nci.data <- NCI60$data # Gene expression data set

table(nci.labs)
nci.data[, 1] |> hist()
nci.data[, 2] |> hist()

# values of gene expressions are probably centered around 0
mins <- apply(nci.data, 2, min)
hist(mins)

maxs <- apply(nci.data, 2, max)
hist(maxs)

means <- apply(nci.data, 2, mean)
hist(means)


sd.data <- scale(nci.data)
data.dist <- dist(sd.data)


hclust(data.dist)





