# DS LAB-6 C
# Logistic Regression: Admission Prediction
# Dataset: student admission (from UCLA)

require(foreign)
require(MASS)

admission_data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(admission_data)
str(admission_data)

# Logistic Regression (admit ~ gre + gpa + rank)
logit_model <- glm(admit ~ gre + gpa + rank, data = admission_data, family = "binomial")
summary(logit_model)

# Model Fit Check
print(paste("Null Deviance:", logit_model$null.deviance))
print(paste("Residual Deviance:", logit_model$deviance))
anova(logit_model, test = "Chisq")

# Predict probabilities
admission_data$Predicted <- predict(logit_model, admission_data, type = "response")
head(admission_data)

# Accuracy check
admission_data$Pred_Class <- ifelse(admission_data$Predicted > 0.5, 1, 0)
accuracy <- mean(admission_data$Pred_Class == admission_data$admit)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))
