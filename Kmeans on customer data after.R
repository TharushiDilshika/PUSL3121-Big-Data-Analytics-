library(ggplot2)
library(dplyr)
library(factoextra)

data <- read.csv("C://Users//IPK COMPUTER//Desktop//customer_data_pca.csv")

#stndardization
scaled_data <- scale(data)
head(scaled_data)

#calculating PCA with eingvalues and eingvectors
pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_model

#transforming dataset into principal components
pca_transformed_data <- as.data.frame(pca_model$x)

#kmeans for PC1, PC2
set.seed(150)
cluster_results <- kmeans(pca_transformed_data[,1:2], centers = 4)

pca_transformed_data$Cluster <- as.factor(cluster_results$cluster)

pca_transformed_data


ggplot(pca_transformed_data, aes(x=PC1, y= PC2, color=Cluster))+
  stat_ellipse(aes(fill=Cluster), geom = "polygon", alpha = 0.4)+
  geom_point(size=3, alpha=0.8)+
  labs(title = "PC1 against PC2 distribution of Clusters ",
       x = "PC1",
       y = "PC2")+
  theme_minimal()


#drawing proper cluster boundries
#creating corvex hull for each cluster
compute_hull <- function(df) df[chull(df$PC1, df$PC2)]

install.packages("cluster")
library(cluster)

hull_data <- pca_transformed_data %>%
  group_by(cluster)%>%
  group_split()%>%
  lapply(compute_hull)%>%
  bind_rows()

ggplot(pca_transformed_data, aes(x=PC1, y=PC2, color=Cluster))+
  geom_point(size=3, alpha=0.8)+
  geom_polygon(data = hull_data, aes(x=PC1, y=PC2, color=Cluster), alpha=0.6, color="blue")
