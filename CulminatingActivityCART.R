library(dplyr)
library(rpart)
library(rpart.plot)
library(knitr)
library(psych)
library(caTools)
library(tree)

data <- read.csv('C:/Data Analysis/DataAnalysisCulminatingActivity/cs_students.csv')
kable(head(data),
      caption = "Descriptive Statistics of CS Students",
      align = "c")

str(data)

new_data <- data[,c(3,4,5,9,10,11,12)]
str(new_data)
head(new_data)

new_data$Career_Group <- case_when(
  # Artificial Intelligence / Machine Learning
  data$Future.Career %in% c(
    "Machine Learning Researcher", "Machine Learning Engineer", "AI Researcher",
    "NLP Research Scientist", "NLP Engineer", "Computer Vision Engineer"
  ) ~ "AI/ML",
  
  # Data & Analytics
  data$Future.Career %in% c(
    "Data Scientist", "Data Analyst", "Bioinformatician", "Geospatial Analyst"
  ) ~ "Data Science",
  
  # Software and Application Development
  data$Future.Career %in% c(
    "Software Engineer", "Web Developer", "Mobile App Developer", "Game Developer",
    "Graphics Programmer", "VR Developer"
  ) ~ "SAD",
  
  # Systems and Cloud Engineering
  data$Future.Career %in% c(
    "Cloud Solutions Architect", "DevOps Engineer", "Distributed Systems Engineer",
    "Database Administrator"
  ) ~ "SCE",
  
  # Cybersecurity
  data$Future.Career %in% c(
    "Information Security Analyst", "Security Analyst", "Ethical Hacker", "Digital Forensics Specialist",
    "Data Privacy Specialist"
  ) ~ "Cybersecurity",
  
  # Emerging and Applied Technologies
  data$Future.Career %in% c(
    "Embedded Software Engineer", "Robotics Engineer", "IoT Developer", "Quantum Computing Researcher",
    "Blockchain Engineer", "Healthcare IT Specialist", "UX Designer", "SEO Specialist"
  ) ~ "EAT"
)

str(new_data)

final_data <- new_data[,c(1,2,3,5,6,7,8)]
final_data
set.seed(123)

split <- sample.split(final_data$Career_Group, SplitRatio = 0.7)

train_data <- subset(final_data, split == TRUE)
test_data <- subset(final_data, split == FALSE)

dim(train_data)
dim(test_data)

result <- describe(final_data)
kable(result, digits = 2, caption = "Descriptive Statistics")

str(final_data)

tree <- tree(Career_Group ~., method = 'recursive.partition', data = final_data, split=c("deviance","gini"))
tree
plot(tree)

full_tree <- rpart(Career_Group ~ ., method = 'class',
                   control = rpart.control(cp = 0.0, minsplit = 2), data = train_data)
plot(full_tree)


plotcp(full_tree)

new_tree <- rpart(Career_Group ~ ., method = 'class',
                  data = train_data, control = rpart.control(cp = 0.011)) # 0.011  0.0077
plot(new_tree, margin = 0.1)

rpart.plot(new_tree, type = 3, digits = 3, fallen.leaves = TRUE)

# Predict using test data
predictions <- predict(new_tree, test_data, type = "class")

# Create confusion matrix
conf_matrix <- table(
  Actual = test_data$Career_Group,
  Predicted = predictions
)

# Display formatted confusion matrix
kable(
  conf_matrix,
  caption = "Confusion Matrix for CART Classification Model",
  align = "c"
)

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print accuracy
cat("Model Accuracy:", round(accuracy * 100, 2), "%")