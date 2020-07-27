## Feature selection

# load in the dataset

supermarket <- read.csv("C:/Users/leonb/Downloads/Supermarket_Dataset_1 - Sales Data.csv",
                        sep = ",", header = TRUE, row.names = 1)
head(supermarket)


# We install and load our wskm package
# ---
#
suppressWarnings(
  suppressMessages(if
                   (!require(wskm, quietly=TRUE))
    install.packages("wskm")))
library(wskm)

set.seed(2)

market <- supermarket[,c(6:8, 12, 14:16)]
head(market)

model <- ewkm(market[1:7], 3, lambda=2, maxiter=1000)

# Loading and installing our cluster package
# ---
#
suppressWarnings(
  suppressMessages(if
                   (!require(cluster, quietly=TRUE))
    install.packages("cluster")))
library("cluster")

# Cluster Plot against 1st 2 principal components
# ---
#
clusplot(market[1:7], model$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=1,main='Cluster Analysis for supermarket')

round(model$weights*100,2)


