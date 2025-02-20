library(ggplot2)
data <- read.csv("C://Users//IPK COMPUTER//Desktop//customer_data_pca.csv")
head(data)
str(data)

#kmeans model
set.seed(150)
cluster_results <- kmeans(data[2:3], centers = 4, nstart = 25)

#adding labels to clusters
data$Cluster <-as.factor(cluster_results$cluster)
summary(data)

#draWing cluster results
ggplot(data, aes(x=Age, y= Spending_Score, color=Cluster))+
  stat_ellipse(aes(fill=Cluster), geom = "polygon", alpha = 0.4)+
  geom_point(size=4, alpha=0.8)+
  labs(title = "Age against Spending_Score distribution of Clusters ",
       x = "Age",
       y = "Spending_Score")+
  theme_minimal()

#draWing cluster results
ggplot(data, aes(x=Annual_Income, y= Spending_Score, color=Cluster))+
  stat_ellipse(aes(fill=Cluster), geom = "polygon", alpha = 0.4)+
  geom_point(size=4, alpha=0.8)+
  labs(title = "Annual_Income against Spending_Score distribution of Clusters ",
       x = "Annual_Income",
       y = "Spending_Score")+
  theme_minimal()


#draWing cluster results
ggplot(data, aes(x=Age, y= Annual_Income, color=Cluster))+
  stat_ellipse(aes(fill=Cluster), geom = "polygon", alpha = 0.4)+
  geom_point(size=4, alpha=0.8)+
  labs(title = "Age against Annual_Income distribution of Clusters ",
       x = "Age",
       y = "Annual_Income")+
  theme_minimal()
