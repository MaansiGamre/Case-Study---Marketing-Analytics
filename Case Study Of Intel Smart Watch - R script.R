# ---- STEP 1: Load Necessary Libraries ----
library(readxl)    # Read Excel files
library(dplyr)     # Data manipulation
library(ggplot2)   # Visualization
library(factoextra) # Clustering visualization
library(cluster)   # Clustering algorithms
library(tidyverse) # Data manipulation and visualization
library(ggdendro)  # Improved hierarchical clustering visualization

# ---- STEP 2: Load and Inspect Data ----
# Load the dataset (Ensure the correct file path)
data <- SmartWatch_Data_File  

# Display summary of data
summary(data)

# Define the working dataframe
df <- data  # Assuming 'data' is the loaded dataset

# ---- STEP 3: Select Relevant Variables for Clustering ----
feature_cols <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style")
df_features <- df[, feature_cols]

# Standardize the data to ensure equal weighting in clustering
df_scaled <- scale(df_features)

# ---- STEP 4: Determine Optimal Number of Clusters ----
# 4.1 Elbow Method (Include this plot in the report under "Elbow Method Analysis")
wss <- numeric(10)
for (k in 1:10) {
  kmeans_model <- kmeans(df_scaled, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}

# Plot the Elbow Method
ggplot(data.frame(Clusters = 1:10, WSS = wss), aes(x = Clusters, y = WSS)) +
  geom_point(size = 4, color = "red") +
  geom_line(color = "black", size = 1) +
  labs(title = "Elbow Method for Optimal k", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

# 4.2 Silhouette Analysis (Include this plot in the report under "Silhouette Analysis")
fviz_nbclust(df_scaled, kmeans, method = "silhouette") + 
  ggtitle("Silhouette Analysis for Optimal k") +
  theme_minimal()

# ---- STEP 5: Hierarchical Clustering ----
# Compute the Euclidean distance
distance <- dist(df_scaled, method = "euclidean")

# Perform hierarchical clustering using Ward’s method
hc_model <- hclust(distance, method = "ward.D2")

# ---- STEP 6: Improved Hierarchical Dendrogram ----
# Reduce dataset size for better visualization (sampling 200 observations)
set.seed(42)
sample_indices <- sample(1:nrow(df_scaled), 200)
df_sample <- df_scaled[sample_indices,]

# Compute hierarchical clustering on the sampled dataset
distance_sample <- dist(df_sample, method = "euclidean")
hc_sample <- hclust(distance_sample, method = "ward.D2")

# 6.1 Plot the improved dendrogram with cluster cuts (Include in "Dendrogram Analysis")
plot(hc_sample, labels = FALSE, main = "Hierarchical Cluster Dendrogram (Sampled Data)",
     xlab = "Observations", ylab = "Height (Dissimilarity)", sub = "",
     cex.main = 1.5, cex.axis = 1.2, lwd = 2, col = "black")

# Highlight the four main clusters
rect.hclust(hc_sample, k = 4, border = c("red", "blue", "green", "purple"))

# Save a high-quality dendrogram image
png("Hierarchical_Cluster_Dendrogram_Cleaned.png", width = 1400, height = 800)
plot(hc_sample, labels = FALSE, main = "Hierarchical Cluster Dendrogram (Sampled Data)",
     xlab = "Observations", ylab = "Height (Dissimilarity)", sub = "",
     cex.main = 1.5, cex.axis = 1.2, lwd = 2, col = "black")
rect.hclust(hc_sample, k = 4, border = c("red", "blue", "green", "purple"))
dev.off()

# ---- STEP 7: Assign Clusters from Hierarchical Clustering to Original Dataset ----
df$Segment <- as.factor(cutree(hc_model, k = 4))

# ---- STEP 8: K-means Clustering Validation ----
set.seed(42)
kmeans_model <- kmeans(df_scaled, centers = 4, nstart = 25)

# Assign K-means clusters to the dataset
df$KMeans_Segment <- as.factor(kmeans_model$cluster)

# ---- STEP 9: PCA Cluster Visualization ----
# PCA visualization of clusters (Include this plot in the report under "Cluster Visualization - PCA")
fviz_cluster(kmeans_model, data = df_scaled, geom = "point",
             ellipse.type = "convex", repel = TRUE,
             ggtheme = theme_minimal()) +
  ggtitle("PCA Cluster Visualization")

# ---- STEP 10: Cluster Membership Bar Chart ----
# Show segment distribution (Include this plot in the report under "Cluster Distribution Bar Chart")
segment_count <- table(df$Segment)

barplot(segment_count, col = c("red", "blue", "green", "purple"), 
        main = "Cluster Membership Distribution", 
        xlab = "Segment", ylab = "Number of Respondents", 
        border = "black", cex.main = 1.5, cex.axis = 1.2)

# ---- STEP 11: Generate Cluster Summaries ----
# Compute the mean feature scores for each cluster
segment_summary <- df %>%
  group_by(Segment) %>%
  summarise(across(all_of(feature_cols), mean))

# Print segment profiles (Include these values in the report under "Segment Descriptions")
print(segment_summary)

# ---- STEP 12: Analyze Demographics Across Segments ----
# Compute demographic characteristics for each segment
demographic_summary <- df %>%
  group_by(Segment) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE),
            Income_Level = mean(Income, na.rm = TRUE),
            Female_Percentage = mean(Female, na.rm = TRUE) * 100,
            Prime_Membership = mean(AmznP, na.rm = TRUE) * 100)

# Print demographic insights (Include this table in the report under "Demographic Analysis")
print(demographic_summary)

# ---- STEP 13: Save Outputs for Report ----
write.csv(df, "Segmented_SmartWatch_Data.csv", row.names = FALSE)
write.csv(segment_summary, "Segment_Profile.csv", row.names = FALSE)
write.csv(demographic_summary, "Segment_Demographics.csv", row.names = FALSE)
