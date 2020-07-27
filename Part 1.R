

# load in the dataset

supermarket <- read.csv("C:/Users/leonb/Downloads/Supermarket_Dataset_1 - Sales Data.csv", sep = ",", header = TRUE)
head(supermarket)

str(supermarket)

colSums(is.na(supermarket))

anyDuplicated(supermarket)

summary(supermarket
        )
# Dimensionality reduction
# PCA


# Loading our dataset
# ---
# 
market <- supermarket
head(market)


# Selecting the numerical data (excluding the categorical variables vs and am)
# ---
# 
market <- supermarket[,c(6:8, 12, 14:16)]
head(market)

# We then pass df to the prcomp(). We also set two arguments, center and scale, 
# to be TRUE then preview our object with summary
# ---
# 

which(apply(market, 2, var)==0)


market.pca <- prcomp(supermarket[,c(6:8, 12, 14:16)], center = TRUE, scale. = TRUE)
summary(market.pca)
  
str(market.pca)

library(devtools)
install_github("vqv/ggbiplot", force = TRUE)

234556


library(ggbiplot)
ggbiplot(market.pca)

# Adding more detail to the plot, we provide arguments rownames as labels
# 
ggbiplot(market.pca, labels=rownames(supermarket), obs.scale = 1, var.scale = 1)


market.customer <- c(rep("Normal", 499), rep("Member",501))
ggbiplot(market.pca,ellipse=TRUE,  labels=rownames(supermarket), groups=market.customer, obs.scale = 1, var.scale = 1)


# T-SNE
# Install rtse package
library(Rtsne)

# Curating the database for analysis 
# 
customer<-supermarket$Customer.type
customer<-as.factor(customer)

# For plotting
#
colors = rainbow(length(unique(customer)))
names(colors) = unique(customer)


# Executing the algorithm on curated data
# 
tsne <- Rtsne(supermarket[,-3], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

# Getting the duration of execution
# 
exeTimeTsne <- system.time(Rtsne(supermarket[,-3], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

# Plotting our graph and closely examining the graph
# 
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=customer, col=colors[customer])


# 2.
# Curating the database for analysis 
# 
price<-supermarket$Total
price<-as.factor(price)

# For plotting
#
color = rainbow(length(unique(price)))
names(color) = unique(price)


# Executing the algorithm on curated data
# 
model <- Rtsne(supermarket[,-16], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

# Getting the duration of execution
# 
exeTimeTsne <- system.time(Rtsne(supermarket[,-16], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

# Plotting our graph and closely examining the graph
# 
plot(tsne$Y, t='n', main="model")
text(tsne$Y, labels=price, col=colors[price])
