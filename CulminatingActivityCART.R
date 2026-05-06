library(rpart)
library(rpart.plot)
library(knitr)
library(psych)

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
  ) ~ "Software and Application Development",
  
  # Systems and Cloud Engineering
  data$Future.Career %in% c(
    "Cloud Solutions Architect", "DevOps Engineer", "Distributed Systems Engineer",
    "Database Administrator"
  ) ~ "Systems and Cloud Engineering",
  
  # Cybersecurity
  data$Future.Career %in% c(
    "Information Security Analyst", "Security Analyst", "Ethical Hacker", "Digital Forensics Specialist",
    "Data Privacy Specialist"
  ) ~ "Cybersecurity",
  
  # Emerging and Applied Technologies
  data$Future.Career %in% c(
    "Embedded Software Engineer", "Robotics Engineer", "IoT Developer", "Quantum Computing Researcher",
    "Blockchain Engineer", "Healthcare IT Specialist", "UX Designer", "SEO Specialist"
  ) ~ "Emerging and Applied Technologies"
)

str(new_data)

final_data <- new_data[,c(1,2,3,5,6,7,8)]
final_data

result <- describe(final_data)
kable(result, digits = 2, caption = "Descriptive Statistics")

str(final_data)

full_tree <- rpart(Career_Group ~ ., method = 'class',
                   control = rpart.control(cp = 0.0, minsplit = 2), data = final_data)
plot(full_tree)

plotcp(full_tree)

new_tree <- rpart(Career_Group ~ ., method = 'class',
                  data = final_data, control = rpart.control(cp = 0.0077)) # 0.011  0.0077
plot(new_tree, margin = 0.1)

rpart.plot(new_tree, type = 3, digits = 3, fallen.leaves = TRUE)