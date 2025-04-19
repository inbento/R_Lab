library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(scatterplot3d)
library(lattice)
library(parameters)
library(factoextra)
library(cluster)
library(NbClust)
library(mclust)

# 1. Дескриптивный анализ данных
data <- read.csv("N:\\train_clean.csv", stringsAsFactors = FALSE) %>% 
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass))

num_vars <- data %>% 
  select(Age, Fare, Parch, SibSp, Family_Size) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(num_vars, aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

cat_vars <- data %>% 
  select(Sex, Pclass, Embarked, Title, Survived) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(cat_vars, aes(x = value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Оценка оптимального числа кластеров
cluster_data <- data %>% 
  select(Age, Fare, Parch, SibSp, Family_Size) %>%
  na.omit() %>%
  scale() %>% 
  as.data.frame()

fviz_nbclust(cluster_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Метод локтя")

fviz_nbclust(cluster_data, kmeans, method = "silhouette") +
  labs(subtitle = "Метод силуэта")

gap_stat <- clusGap(cluster_data, FUN = kmeans, nstart = 100, K.max = 10, B = 20, iter.max = 100)
fviz_gap_stat(gap_stat) +
  labs(subtitle = "Статистика разрыва")

n_clust <- n_clusters(cluster_data,
                      package = c("NbClust", "mclust", "factoextra"),
                      standardize = FALSE)
n_clust
plot(n_clust)


# 3. Иерархическая кластеризация и дендрограмма

dist_matrix <- dist(cluster_data, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")
hc$labels <- data$Name 

hcd <- as.dendrogram(hc)
par(mar = c(16, 4, 4, 6))
plot(cut(hcd, h = 4)$upper, main = "Верхняя часть дендрограммы")
plot(cut(hcd, h = 4)$lower[[2]], main = "Branch 2")

pdf("dendrogram.pdf", width = 85, height = 40)
plot(hc, cex = 0.5, las = 2, hang = -1)
dev.off()
cluster_result <- cutree(hc, k = 2)
cluster_data_df <- as_tibble(cluster_data) %>%
  mutate(Cluster = as.factor(cluster_result))


# 4. Диаграммы со столбчатыми диаграммами и боксплотами групп
cluster_data_df %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Распределение переменных по кластерам")

data_with_clusters <- data %>%
  na.omit() %>%
  mutate(Cluster = as.factor(cluster_result))

ggplot(data_with_clusters, aes(x = Pclass, fill = Cluster)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Распределение классов по кластерам")




# 5. Кластеризация по k-means

set.seed(123)
km_res <- kmeans(cluster_data, centers = 2, nstart = 25)

fviz_cluster(km_res, data = cluster_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

cluster_data_df_km <- as_tibble(cluster_data) %>%
  mutate(Cluster = as.factor(km_res$cluster))

cluster_data_df_km %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Mean") %>%
  ggplot(aes(x = Variable, y = Mean, fill = Cluster)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Средние значения переменных по кластерам (k-means)")


# 6. Построение scatterplot
pairs(cluster_data[,1:3], 
      col = km_res$cluster,
      pch = 19,
      main = "Scatterplot матрица для Age, Fare и Parch")


# 7. Трехмерная кластеризация
scatterplot3d(cluster_data[,1:3],
              color = km_res$cluster,
              pch = 19,
              main = "3D кластеризация пассажиров Титаника",
              xlab = "Age",
              ylab = "Fare",
              zlab = "Parch")


data_with_clusters_km <- data %>%
  na.omit() %>%
  mutate(Cluster = as.factor(km_res$cluster))

ggplot(data_with_clusters_km, aes(x = Cluster, fill = as.factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Процент", fill = "Выжил", 
       title = "Выживаемость по кластерам") +
  theme_minimal()

