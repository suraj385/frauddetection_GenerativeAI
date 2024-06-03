library(caret)
library(dplyr)
library(ggplot2)
library(caTools)
library(ROSE)
library(smotefamily)
library(rpart)
library(rpart.plot)
library(stats)
repeat {
  transaction_limit <- as.numeric(readline("Enter the transaction limit: "))
  if (transaction_limit < 500 ) {
    cat("Invalid amount. Please enter a valid transaction limit.\n")
    next
    
  } else if (transaction_limit <= 500) {
    num_data_points <- 80
    break  
  } else if (transaction_limit <= 1000) {
    num_data_points <- 160
    break 
  } else if (transaction_limit <= 2000) {
    num_data_points <- 200
    break  
  } else if (transaction_limit <= 5000) {
    num_data_points <- 520
    break  
  } else if (transaction_limit <= 7000) {
    num_data_points <- 600
    break 
  } else if (transaction_limit <= 10000) {
    num_data_points <- 800
    break  
  } else {
    cat("Transaction limit exceeds the card's limit of 10,000. Please enter a valid transaction limit.\n")
  }
}
percent_greater_than_limit <- 0.4

num_greater_than_limit <- round(num_data_points * percent_greater_than_limit)
num_smaller_than_limit <- num_data_points - num_greater_than_limit


data_greater_than_limit <- data.frame(
  Transaction = runif(num_greater_than_limit, min = transaction_limit + 1, max = 2 * transaction_limit),
  Merchant_Rating = sample(10:1499, num_greater_than_limit, replace = TRUE),
  Class = 1
)


data_smaller_than_limit <- data.frame(
  Transaction = runif(num_smaller_than_limit, min = 1, max = transaction_limit),
  Merchant_Rating = sample(1500:3000, num_smaller_than_limit, replace = TRUE),
  Class = 0
)


final_data <- rbind(data_greater_than_limit, data_smaller_than_limit)


colnames(final_data) <- c("Transaction", "Merchant_Rating", "Class")

csv_file <- "transaction_database.csv"
write.csv(final_data, file = csv_file, row.names = FALSE)

  G <- num_data_points
  
  # Calculate S, Q, W, E, R
  S <- round(0.10 * G)
  Q <- round(0.50 * S)
  W <- S - Q
  E <- round(0.50 * S)
  R <- S - E
  
  # Calculate U, I, O, P, H, J, T, Y
  U <- round(0.50 * Q)
  I <- Q - U
  O <- round(0.50 * W)
  P <- W - O
  H <- round(0.50 * E)
  J <- E - H
  T <- round(0.50 * R)
  Y <- R - T
  
  # Generating additional features based on the mapping conditions
  # Map O and J with Class 0
  class_1_data_oj <- data.frame(
    Transaction = runif(O, min = transaction_limit + 1, max = 2 * transaction_limit),
    Merchant_Rating = sample(10:1499, O, replace = TRUE),
    Class = rep(1, O)
  )
  
  # Map I and T with Class 1
  class_1_data_it <- data.frame(
    Transaction = runif(I, min = 1, max = transaction_limit),
    Merchant_Rating = sample(1500:3000, I, replace = TRUE),
    Class = rep(1, I)
  )
  
  # Map U and H with Class 0
  class_0_data_uh <- data.frame(
    Transaction = runif(U, min = 1, max = transaction_limit),
    Merchant_Rating = sample(10:1499, U, replace = TRUE),
    Class = rep(0, U)
  )
  
  # Map P and Y with Class 1
  class_1_data_py <- data.frame(
    Transaction = runif(P, min = transaction_limit + 1, max = 2 * transaction_limit),
    Merchant_Rating = sample(1500:3000, P, replace = TRUE),
    Class = rep(1, P)
  )
  
  # Combine all the data frames
  additional_data <- rbind(
    class_1_data_oj,
    class_1_data_it,
    class_0_data_uh,
    class_1_data_py
  )
  
  # Load the existing data
  existing_data <- read.csv("transaction_database.csv")
  
  # Append the additional data to the existing data
  combined_data <- rbind(existing_data, additional_data)
  
  # Save the combined data to the same file
  write.csv(combined_data, file = "transaction_database.csv", row.names = FALSE)
private_data <- read.csv("transaction_database.csv")
private_data$Class <- factor(private_data$Class,levels = c(0,1))
summary(private_data)
table(private_data$Class)
prop.table(table(private_data$Class))
labels <-c("legit","fraud")
labels <-paste(labels, round(100*prop.table(table(private_data$Class)),2))
labels <-paste0(labels, "%")
pie(table(private_data$Class),labels,col = c("dodgerblue2","red"),
    main = "Pie chart of Transactions")
ggplot(data = private_data, aes(x = Transaction , y = Merchant_Rating , col = Class)) +
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
data_for_pca <- private_data %>% select(Transaction, Merchant_Rating)
pca_result <- prcomp(data_for_pca, scale. = TRUE)
V1_values <- pca_result$x[, 1]
V2_values <- pca_result$x[, 2]
pca_data <- data.frame(V1 = V1_values, V2 = V2_values, Class = private_data$Class)
write.csv(pca_data, file = "p1.csv", row.names = FALSE)
#go to AI generative 
pca1_data <- read.csv("combined_dataset.csv")
pca1_data$Class <- factor(pca1_data$Class,levels = c(0,1))
table(pca1_data$Class)
prop.table(table(pca1_data$Class))
labels <-c("legit","fraud")
labels <-paste(labels, round(100*prop.table(table(pca1_data$Class)),2))
labels <-paste0(labels, "%")
pie(table(private_data$Class),labels,col = c("dodgerblue2","red"),
    main = "Pie chart of Transactions")
ggplot(data = pca1_data, aes(x = V1 , y = V2, col = Class)) +
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
set.seed(123)
data_sample2 = sample.split(pca1_data$Class,SplitRatio = 0.80)
train_data2 = subset(pca1_data,data_sample2==TRUE)
test_data2 = subset(pca1_data,data_sample2==FALSE)
table(train_data2$Class)
table(test_data2$Class)
CART_model2 <- rpart(Class ~ . , train_data2)
rpart.plot(CART_model2,extra = 0, type = 5,tweak = 1.2)
predicted_val <- predict(CART_model2,test_data2,type = 'class')
confusionMatrix(predicted_val,test_data2$Class)
