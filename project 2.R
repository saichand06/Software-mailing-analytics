#Installing all necessary packages and libraries

install.packages(c("tidyverse","ggplot2","caret","corrplot","gains","pROC","randomForest","factoextra","cluster"))
library(corrplot)
library(caret)
library(pROC)
library(tidyverse)
library(ggplot2)
library(VIM)
library(car)
library(gains)


#Loading dataset
mail <- read.csv("C:/Users/06sai/Downloads/Software_Mailing_List.csv")

#Viewing the dataset
View(mail)

#Printing the dimensions of dataset 
dim(mail)

#datatype of each variable
str(mail)

#Summary of variables
summary(mail)

#Checking missing values
colSums(is.na(mail))

#Total missing values
sum(is.na(mail))


#Histogram for frequency of purchases
ggplot(mail, aes(x = Freq)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(x = "Frequency", y = "Count", title = "Frequency of Purchases")

#Histogram for spending
ggplot(mail, aes(x = Spending)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(x = "Spending", y = "Count", title = "Distribution of Spending")


#Boxplot for Purchase vs spending
ggplot(mail, aes(x= factor(Purchase),y = Spending, fill = factor(Purchase))) +
  geom_boxplot() +
  labs(title = "spending") +
  theme_minimal()

#Scatter plot - Spending vs Frequency 

ggplot(mail, aes(x = Freq, y=Spending)) +
  geom_point(alpha =0.6, color ="steelblue") +
  geom_smooth(method = "lm", se = FALSE,color = "darkred") +
  labs(title = "Spending vs Frequency",
       x = "frequency of Purchases",
       y = "Total Spending") +
  theme_minimal()

#Bar plot -Spending by Source and top 10 sources

source_cols <- grep("^source_", names(mail), value = TRUE)

mail_sources <- mail %>% 
  pivot_longer(cols = all_of(source_cols), names_to ="Source", values_to = "Flag") %>%
  filter(Flag == 1) %>%
  group_by(Source) %>%
  summarise(Total_Spending = sum(Spending, na.rm = TRUE)) %>%
  arrange(desc(Total_Spending)) %>%
  slice_head(n = 10)

ggplot(mail_sources, aes(x= reorder(Source, Total_Spending), y= Total_Spending, fill = Source)) +
         geom_bar(stat = "identity", fill = "blue") +
         coord_flip()+
         labs(title = "Top 10 Sources by Total Spending",
              x = "Source",
              y = "Total Spending($)") +
         theme_minimal()

#Scatter plot Recency vs Spending 

ggplot(mail, aes(x = last_update_days_ago, y= Spending))+
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Recency vs Spending",
       x= "Days Since Last Update",
       y= "Spending($)")
  theme_minimal()
               

#Boxplot for each gender spending

colnames(mail)[colnames(mail) == "Gender=male"] <- "Gender" 

unique(mail$Gender)

table(mail$Gender)

mail$Gender <- factor(mail$Gender, 
                      levels = c(0, 1),
                      labels = c("Female", "Male"))

ggplot(mail, aes(x = factor('Gender'), y = Spending, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Spending", 
       title = "Spending by Gender") +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_fill_manual(values = c("pink", "lightblue"),
                    labels = c("Female", "Male"))

#Correlation Heatmap

install.packages("ggcorrplot")
library(ggcorrplot)

numerical_mail <- mail %>% select(where(is.numeric))

cor_matrix <- cor(numerical_mail, use = "complete.obs")

ggcorrplot(cor_matrix,
           type = "full",
           lab  = FALSE,
           colors =c("red","white","blue"),
           title = "Correlation Heatmap",
           ggtheme = ggplot2::theme_minimal())


# PREDICTOR ANALYISIS AND RELEVANCY

#ANOVA for numeric predictors

numeric_vars <- mail %>% select(where(is.numeric)) %>% names()

for(var in numeric_vars){
  formula <- as.formula(paste(var, "~Purchase"))
  model <- aov(formula, data = mail)
  cat("\n ANOVA for :", var, "\n")
  print(summary(model))
}



#VIF

if("Gender = male" %in% names(mail)) names(mail)[names(mail) == "Gender = male"]

mail <- mail %>%
  mutate(
    Purchase       = factor(Purchase, labels = c("Non-Buyer","Buyer")),
    Gender         = factor(Gender.male, labels =c("Female","Male")),
    Web.Order      = factor(Web.order),
    Address_is_res = factor(Address_is_res)
  )

colnames(mail)

lm_model_mail <- lm(Spending ~ Freq + Web.order + Gender.male + Address_is_res + last_update_days_ago +
                      X1st_update_days_ago + source_a +source_b + source_c + source_d + source_e+ 
                      source_m + source_o + source_h + source_r + source_s + source_t + source_u + source_p +
                      source_x + source_w, 
                    data = mail)

vif(lm_model_mail)

#DATA ENGINEERING AND TRANSFORMATION

#Feature scaling
num_cols <- mail %>% select(where(is.numeric)) %>% names()

#Scaling for clustering
mail_scaled <- mail
mail_scaled[num_cols] <- scale(mail[num_cols])



#DATA PARTITIONING

set.seed(1234)

train_index <- createDataPartition(mail$Purchase, p= 0.7, list = FALSE)

train_data <- mail[train_index, ]
test_data  <- mail[-train_index, ]

#################### Logistic Regression Model   #######################
#Target variable is purchase and excluding spending and first update days ago variables(highly correlated)

log_model <- glm(Purchase ~ Freq + Web.order + Gender.male + Address_is_res + last_update_days_ago +
         source_a +source_b + source_c + source_d + source_e+ 
        source_m + source_o + source_h + source_r + source_s + source_t + source_u + source_p +
        source_x + source_w, 
      data = train_data, family = binomial)

#Summary of logistic model
summary(log_model)

#Predictions

log_pred <- predict(log_model, newdata = test_data, type= "response")

#Actual Values

actual <- test_data$Purchase

#we are checking with different cuttoffs because we want to find at which cutoff it is capturing more buyers

cutoffs <- c(0.4, 0.5,0.6,0.7)

results <- data.frame(
  Cutoff = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric()
)

for(c in cutoffs){
  log_class <- ifelse(log_pred > c, "Buyer", "Non-Buyer")
  
  cm <- confusionMatrix(factor(log_class, levels = c("Non-Buyer","Buyer")),
                        actual,
                        positive = "Buyer")
  cm_table <- cm$table
  
  TN <- cm_table[1,1]
  FP <- cm_table[1,2]
  FN <- cm_table[2,1]
  TP <- cm_table[2,2]
  
  
  
  results <- rbind(results, data.frame(
    Cutoff = c,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    TP= TP, TN=TN, FP=FP, FN=FN
  ))
 
}
  
print(results)  

#Plotting cutoff performance

results_cutoff <- tidyr::pivot_longer(

        results,
        cols = c("Accuracy","Sensitivity","Specificity"),
        names_to ="Metric",
        values_to = "Value"
)                                      
                                  
print(head(results_cutoff))

ggplot(results_cutoff,aes(x= Cutoff, y= Value, color = Metric)) +
  geom_line(size =1.2) +
  geom_point(size=2)+
  labs(title = "Cutoff Performance Curve - Logistic Regression",
       x= "Cutoff Probability",
       y="Metric Value") +
  scale_color_manual(values = c("Accuracy" ="blue", "Sensitivity"="green", "Specificity"="red"))+
  theme_minimal(base_size =14)

'''
For North Point, cutoff = 0.4 is the optimal choice, 
as it maximizes the ability to capture true buyers (91% sensitivity), which aligns with the goal of prioritizing buyers in promotions.
While specificity is slightly lower, the benefit of capturing more actual buyers outweighs the cost of sending some extra unwanted mails
'''
  
#ROC Curve and AUC

actual <- ifelse(test_data$Purchase == "Buyer",1,0)

log_pred <- predict(log_model, newdata = test_data, type = "response")


roc_obj <- roc(actual,log_pred)

#Plotting Roc curve with absolute line for better understanding

plot(roc_obj, col = "blue", lwd =3,
     main = "ROC Curve for Logistic Regression")
abline(a=0, b=1, lty =2, col = "red")

#AUC value for Logistic

auc_value <- auc(roc_obj)
cat("AUC Logistic:", auc_value, "\n")

########## RANDOM FOREST CLASSIFICAION  #########

set.seed(1234)

library(randomForest)

rf_model <- randomForest(Purchase ~ Freq + Web.order + Gender.male + Address_is_res + last_update_days_ago +
                               source_a +source_b + source_c + source_d + source_e+ 
                               source_m + source_o + source_h + source_r + source_s + source_t + source_u + source_p +
                               source_x + source_w, 
                             data = train_data, ntree = 500, mtry= 5, importance = TRUE)

print(rf_model)

#Predictions

rf_pred <- predict(rf_model, newdata = test_data, type = "prob")[,2]

rf_class <- ifelse(rf_pred > 0.5 ,"Buyer","Non-Buyer")

cm_rf <- confusionMatrix(factor(rf_class, levels = c("Non-Buyer","Buyer")),
                         test_data$Purchase,
                         positive = "Buyer")
print(cm_rf)


#ROC Curve and AUC

roc_rf <- roc(ifelse(test_data$Purchase== "Buyer",1,0), rf_pred)

plot(roc_rf, col="darkgreen", lwd=3, main = "ROC Curve - Random Forest ")
abline(a=0, b=1, lty =2, col ="red")
              
auc_rf <- auc(roc_rf)
cat("Random Forest AUC:", auc_rf, "\n")

#Variable Importance plot

varImpPlot(rf_model, main = "Variable Importance - Random Forest")


###############COMPARISON OF LOGISTIC VS RANDOM FOREST CLASSIFICATION

auc_log <- auc(roc_obj)

auc_rf <- auc(roc_rf)

#Confusion matrices

acc_log <- cm$overall["Accuracy"]
sensi_log <- cm$byClass["Sensitivity"]
speci_log <- cm$byClass["Specificity"]

acc_rf <- cm_rf$overall["Accuracy"]
sensi_rf <- cm_rf$byClass["Sensitivity"]
speci_rf <- cm_rf$byClass["Specificity"]

comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(round(acc_log,3), round(acc_rf,3)),
  Sensitivity = c(round(sensi_log,3), round(sensi_rf,3)),
  Specificity = c(round(speci_log,3), round(speci_rf,3))

)

print(comparison)

# Reduction in wasted mailings and response rate

wasted_log <- mean(ifelse(log_pred >0.4 & test_data$Purchase == "Non-Buyer",1,0))

wasted_rf <- mean(ifelse(rf_pred >0.5 & test_data$Purchase == "Non-Buyer",1,0))

response_log <- mean(ifelse(log_pred >0.4 & test_data$Purchase == "Buyer",1,0))

response_rf <- mean(ifelse(rf_pred >0.5 & test_data$Purchase == "Buyer",1,0))

impact <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Wasted_mail_rate = c(round(wasted_log,4), round(wasted_rf,4)),
  Response_rate = c(round(response_log,4), round(response_rf,4))
  
)

print(impact)

####################### CLUSTERING #####################

library(factoextra)

#Selecting features for Clustering

cluster_data <- mail_scaled %>%
  select(Freq, last_update_days_ago)



#Deciding number of clusters by using Elbow Method


wss <- sapply(1:10, function(k){
  kmeans(cluster_data, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, wss, type="b", pch=19, col="blue",
     xlab= "Number of Clusters(k)", ylab = "Within Sum of Squares",
     main = "Elbow Method for optimal k")

#Building K-means models 

set.seed(1234)

kmeans_model <- kmeans(cluster_data, centers=3, nstart =25)

#Adding cluster Labels back to data

mail_scaled$Cluster <- as.factor(kmeans_model$cluster)

#Checking Cluster sizes

table(mail_scaled$Cluster)

#Visualizing clusters
fviz_cluster(kmeans_model, data = cluster_data,
             geom ="point", ellipse.type = "convex",
             palette="jco", ggtheme= theme_minimal())


#Calculating  averages of key numeric variables by cluster
#Cluster Profile

cluster_profile_km <- mail_scaled %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Spending   = mean(Spending, na.rm = TRUE),
    Avg_Freq       = mean(Freq, na.rm = TRUE),
    Avg_Recency    = mean(last_update_days_ago, na.rm = TRUE),
    Purchase_Rate  = mean(as.numeric(Purchase == "Buyer"), na.rm =TRUE),
    Count = n()
    
  )

print(cluster_profile_km)

#Barplot - Purchase rate per cluster

ggplot(cluster_profile_km, aes(x=Cluster, y= Purchase_Rate, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Purchase Rate by Cluster",
       x= "Cluster",
       y= "Proportion of Buyers")+
  theme_minimal()


#Barplot- Count of customers in each cluster

ggplot(cluster_profile_km, aes(x= Cluster, y=Count, fill =Cluster))+
  geom_bar(stat ="identity") +
  labs(title = "Customer Count by Cluster",
       x= "Cluster",
       y="Number of Customers") +
  theme_minimal()

#Cluster Profile Visualization

cluster_profile_km_long <- cluster_profile_km %>%
  pivot_longer(cols= c(Avg_Spending, Avg_Freq, Purchase_Rate,Count),
               names_to = "Metric", values_to = "Value")

#Plotting Cluster Profile

ggplot(cluster_profile_km_long, aes(x=Cluster, y= Value, fill= Metric))+
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Cluster Profile: Spending, Frequency, Purchase Rate & Count",
       x= "Cluster", y="Value") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size =14)

################## HIERARCHICAL CLSUTERING ##################

#Distance matrix

dist_matrix <- dist(cluster_data, method = "euclidean")

#Hierarchical clustering with ward's method

hc_model <- hclust(dist_matrix, method = "ward.D2")

#Dendrogram

factoextra::fviz_dend(hc_model,
         k=3,
         rect=TRUE,
         rect_border = "jco",
         rect_fill = TRUE,
         show_labels = FALSE,
         cex= 0.6,
         main = "Enhanced Dendrogram - Hierarchical Clustering",
         xlab = "Customer",
         ylab= "Height(Dissimilarity)")


mail_scaled$HC_Cluster <- as.factor(cutree(hc_model, k=3))

table(mail_scaled$HC_Cluster)

#Profiling Clusters

cluster_profile_hc <- mail_scaled %>%
  group_by(HC_Cluster) %>%
  summarise(
    Avg_Spending  = mean(Spending, na.rm =TRUE),
    Avg_Freq      = mean(Freq, na.rm = TRUE),
    Avg_Recency   = mean(last_update_days_ago, na.rm=TRUE),
    Purchase_Rate = mean(as.numeric(Purchase == "Buyer"), na.rm =TRUE),
    Count =n()
  )
print(cluster_profile_hc)

#Barplot - Purchase rate per cluster

ggplot(cluster_profile_hc, aes(x= HC_Cluster, y= Purchase_Rate, fil = HC_Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Purchase Rate by Cluster(Hierarchical",
       x= "Cluster",
       y= "Proportion of Buyers")+
  theme_minimal()


#Barplot- Count of customers in each cluster

ggplot(cluster_profile_hc, aes(x= HC_Cluster, y=Count, fill = HC_Cluster))+
  geom_bar(stat ="identity") +
  labs(title = "Customer Count by Cluster(Hierarchical)",
       x= "Cluster",
       y="Number of Customers") +
  theme_minimal()

#Cluster Profile Visualization

cluster_profile_hc_long <- cluster_profile_hc %>%
  pivot_longer(cols= c(Avg_Spending, Avg_Freq, Purchase_Rate,Count),
               names_to = "Metric", values_to = "Value")

#Plotting Cluster Profile

ggplot(cluster_profile_hc_long, aes(x= HC_Cluster, y= Value, fill= Metric))+
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Hierarchical Cluster Profile: Spending, Frequency, Purchase Rate & Count",
       x= "Cluster", y="Value") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size =14)



# COMPARISION OF k-means and Hierarchical
install.packages("cluster")
library(cluster)

#Computing Silhouette scores for k-means

sil_kmeans <- silhouette(kmeans_model$cluster, dist(cluster_data))

avg_sil_kmeans <- mean(sil_kmeans[,3])

cat("Avg Silhouette (k-means):", avg_sil_kmeans, "\n")

#Computing Silhouette scores for Hierarchical

sil_hc <- silhouette(as.numeric(mail_scaled$HC_Cluster), dist(cluster_data))

avg_sil_hc <- mean(sil_hc[,3])

cat("Avg Silhouette(Hierarchical:", avg_sil_hc, "\n")

comparision_cluster <- data.frame(
  Model = c("K-means", "Hierarchical"),
  Avg_Silhouette = c(avg_sil_kmeans, avg_sil_hc)
)

print(comparision_cluster)
