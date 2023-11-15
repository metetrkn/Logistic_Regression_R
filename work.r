# Importing data and checking it
data <- read.csv('../Social_Network_Ads.csv')
head(data)

# Create a design matrix for the "Gender" variable
design_matrix <- model.matrix(~ Gender -1, data=data)

# Create encoded data
encoded_df <- cbind(data, design_matrix)

# Removing original gender column from data
encoded_df <- encoded_df[, -which(names(encoded_df) == 'Gender')]

# Checking ready version of data for linear regression
head(encoded_df)

# Dividing Data into training-test data sets 80%-20%
total_records <- nrow(encoded_df) # 400 observations/records

n_training <- round(total_records * 0.8) # number of training dataset in df
n_test <- total_records - n_training # number of test dataset in df

# Getting indexes of training,test datasets randomly
set.seed(52)
index_training <- sample(1:total_records, n_training, replace=F)
index_test <- setdiff(1:total_records, index_training)

# Creating training/test dataset
training_df <- encoded_df[index_training,] 
test_df <- encoded_df[index_test, ]

# Creating Logistic Regression to check if item bought by user
glm1 <- glm(formula=Purchased ~ Age + EstimatedSalary + GenderFemale
            + GenderMale, data=training_df, family=binomial())

# Extracting target column from test df
target_test <- test_df[,which(colnames(test_df) == 'Purchased')]

# Dropping target column from test df
test_df <- test_df[,-which(colnames(test_df) == 'Purchased')] 

# Predicting, Evaulating Model on test dataset
preds <- predict(glm1, newdata=test_df, type='response')

# Setting threshold as 0.5 and creating binary result
preds_binary <- ifelse(preds >= 0.5, 1, 0)

# Calculating accuracy of  model
accuracy_model <- mean(preds_binary == target_test)

# Reporting the accuracy of model
print(paste("Model's accuracy with custom threshold (0.5) ----> ", round(accuracy_model, 2)*100, "%", sep=''))
