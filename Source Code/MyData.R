install.packages("ROCR")
install.packages("dplyr")
install.packages("skimr")
install.packages("gt")
install.packages("caret")
install.packages(("h2o"))
library(ggplot2)
library(ROCR)
library(dplyr)
library(skimr)
library(gt)
library(h2o)
diabetes <- read.csv("D:\Universitate\Anul 3\sem 1\AD\diabetes.csv")

#Preprocesarea datelor

#Stergerea liniilor care au date lipsă
diabetes_test = diabetes |> filter_at(vars("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI"), all_vars(. != 0))

#Eliminarea outlierilor
ggplot(diabetes_filtred, aes(y = BMI)) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal()

out <- boxplot.stats(diabetes_filtred$Insulin)$out
out_rows <- which(diabetes_filtred$Insulin %in% c(out))
out_rows

diabetes_filtred <- diabetes_filtred |> filter(!(row_number() %in% out_rows))

#Completarea datelor lipsă
diabetesv2 <- diabetesv2 |> mutate(Glucose = ifelse(Glucose == 0, median(Glucose), Glucose))

diabetesv2 <- diabetesv2 |> mutate(BloodPressure = ifelse(BloodPressure == 0, median(BloodPressure), BloodPressure))

diabetesv2 <- diabetesv2 |> mutate(SkinThickness = ifelse(SkinThickness == 0, median(SkinThickness), SkinThickness))

diabetesv2 <- diabetesv2 |> mutate(Insulin = ifelse(Insulin == 0, median(Insulin), Insulin))

diabetesv2 <- diabetesv2 |> mutate(BMI = ifelse(BMI == 0.0, median(BMI), BMI))

#Împărțirea pe grupe
diabetes_filtred <- diabetes_filtred |> mutate(AgeGroup = ifelse(Age <= 25, "Young", ifelse(Age > 25 & Age <= 40, "Middle-Aged", "Old")))

diabetes_filtred <- diabetes_filtred |> mutate(BMIGroup = ifelse(BMI <= 24.9, "Normal", ifelse(BMI > 24.9 & BMI <= 29.9, "Overweight", "Obese")))

diabetes_filtred <- diabetes_filtred |> mutate(BloodPressureGroup = ifelse(BloodPressure <= 80, "Low", ifelse(Age > 80 & Age <= 120, "Normal", "High")))

diabetesv2 |> select(BloodPressure, BloodPressureGroup)

diabetes |> select(Insulin) |> filter(Insulin == 0)

diabetes |> median(Insulin)

diabetesv2 |> select(Insulin) |> filter(Insulin == median(Insulin)) |> head(5)

write.csv(diabetes_filtred, "D:\\Universitate\\Anul3\\sem1\\AD\\Proiect\\diabetes_filtred.csv", row.names=FALSE)

#Histogram for continuous variables to understand the distribution
ggplot(diabetes_filtred, aes(x = BloodPressure)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Glucose Levels", x = "Glucose Level", y = "Count") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = Insulin)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Insulin Levels", x = "Insulin Level", y = "Count") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = Glucose)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Glucose Levels", x = "Glucose Level", y = "Count") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = SkinThickness)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Insulin Levels", x = "Insulin Level", y = "Count") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = BMI)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Insulin Levels", x = "Insulin Level", y = "Count") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = DiabetesPedigreeFunction)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribution of Insulin Levels", x = "Insulin Level", y = "Count") +
  theme_minimal()

#Boxplot to understand the spread and potential outliers
ggplot(diabetes_filtred, aes(y = Glucose, x = as.factor(Outcome))) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal()

#Boxplot to understand the spread and potential outliers
ggplot(diabetesv2, aes(y = Glucose)) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Glucose Levels", y = "Glucose Level") +
  theme_minimal()

#Bar plots for categorical or discrete variables:
# For Pregnancies
ggplot(diabetes_filtred, aes(x = as.factor(Pregnancies))) + 
  geom_bar(fill = "lightpink", color = "black") +
  labs(title = "Distribution of Number of Pregnancies", x = "Number of Pregnancies", y = "Count") +
  theme_minimal()

# For Outcome (as it's a binary variable)
ggplot(diabetes_filtred, aes(x = as.factor(Outcome))) + 
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Diabetes Outcome", x = "Outcome (0 = No Diabetes, 1 = Diabetes)", y = "Count") +
  theme_minimal()

#Scatter plot to understand relationships between two continuous variables:
# For Glucose vs. Insulin
ggplot(diabetes_filtred, aes(x = Glucose, y = Insulin)) + 
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter plot of Glucose vs. Insulin", x = "Glucose Level", y = "Insulin Level") +
  theme_minimal()


skim_data <- skim(diabetesv2)

skim_data %>%
  dplyr::filter(variable %in% c("mean", "sd", "hist")) %>%
  gt::gt() %>%
  gt::fmt_number(columns = starts_with("value"), decimals = 2)

head(skim_data)


ggplot(diabetesv2, aes(Age, BloodPressure)) +
  stat_summary(fun = mean, geom = "point")

ggplot(diabetesv2, aes(Pregnancies, Glucose)) +
  stat_summary(fun = mean, geom = "line")

ggplot(diabetesv2, aes(SkinThickness, Insulin)) +
  stat_summary(fun = mean, geom = "point")

ggplot(diabetesv2, aes(Glucose, Outcome)) +
  stat_summary(fun = mean, geom = "bar")

InsulinGlucose <- 
  model <- lm(Insulin ~ Glucose, data = diabetesv2)
summary(model)

ggplot(diabetes_filtred, aes(x = Glucose, y = Insulin)) + 
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter plot of Glucose vs. Insulin", x = "Glucose Level", y = "Insulin Level") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", linewidth = 1.5)

ggplot(diabetes_filtred, aes(x = SkinThickness, y = BMI)) + 
  geom_point(color = "blue", alpha = 0.5) +
  theme_minimal()


plot(diabetesv2$Outcome, diabetesv2$Pregnancies)

#Ipoteza 1
ggplot(diabetes_filtred, aes(y = SkinThickness, x = as.factor(Outcome))) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal()

t.test(Glucose ~ Outcome, data = diabetes_filtred)

#Ipoteza 2
diabetes_filtred %>%
  group_by(AgeGroup) %>%
  slice_sample(n = 10, replace = FALSE) %>%
  group_by(AgeGroup) %>%
  summarise(count_outcome_1 = sum(Outcome == 1, na.rm = TRUE)) %>%
  ggplot(aes(x = AgeGroup, y = count_outcome_1)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Group", y = "Count of Outcome 1", title = "Count of Outcome 1 by Group (10 random samples per category)") +
  theme_minimal()

# Contingency table of AgeGroup and Outcome
cont_table <- diabetes_filtred %>%
  group_by(AgeGroup, Outcome) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = Outcome, values_from = count, values_fill = 0)

# Performing the chi-squared test
chi_squared_test <- chisq.test(cont_table[,2:3])

# Display test results
chi_squared_test

ggplot(diabetes_filtred, aes(x = as.factor(AgeGroup))) + 
  geom_bar(fill = "lightcoral", color = "black") +
  theme_minimal()


#Ipoteza 3

ggplot(diabetes_filtred, aes(x = as.factor(BMIGroup))) + 
  geom_bar(fill = "lightcoral", color = "black") +
  theme_minimal()

ggplot(diabetes_filtred, aes(x = BMIGroup, y = Insulin)) +
  geom_boxplot() +
  labs(title = "Distribution of Insulin for Each BMI Group",
       x = "BMI Group",
       y = "Insulin")

ggplot(diabetes_filtred, aes(x = Insulin, fill = BMIGroup)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~BMIGroup) +
  labs(title = "Distribution of Insulin for Each BMI Group",
       x = "Insulin",
       y = "Count") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"))

mod1 <- aov(Insulin ~ BMIGroup, data = diabetes_filtred)
summary(mod1)

#Ipoteza 4

ggplot(diabetes_filtred, aes(x = SkinThickness, y = BMI)) + 
  geom_point(color = "blue", alpha = 0.5) +
  theme_minimal()
install.packages("ggstatsplot")

library(ggstatsplot)

ggscatterstats(data = diabetes_filtred, x = SkinThickness, y = BMI, type = "parametric")

#Ipoteza 5

ggscatterstats(data = diabetes_filtred, x = Glucose, y = Insulin, type = "parametric")


#Splitting the data into training and testing
train_indices <- sample(nrow(diabetes_filtred), 0.7 * nrow(diabetes_filtred))

# Create the training and testing sets
training_data <- diabetes_filtred[train_indices, ]
testing_data <- diabetes_filtred[-train_indices, ]

testing_data_subset2 <- testing_data[, c("Glucose", "Outcome")]
testing_data_subset3 <- testing_data[, c("Glucose", "DiabetesPedigreeFunction", "Outcome")]


#Logistic regression
plot(diabetes_filtred)

newdata <- data.frame(Glucose = 100, Pregnancies = 20, BloodPressure = 88, SkinThickness = 23, Insulin = 200, BMI = 28, Age = 90, DiabetesPedigreeFunction = 0.1)
newdata <- data.frame(Age = 80)


model <- glm(Outcome ~., data = training_data, family="binomial")
model2 <- glm(Outcome ~ Glucose, data = training_data, family="binomial")
model3 <- glm(Outcome ~ Glucose + DiabetesPedigreeFunction, data = training_data, family="binomial")

summary(model2)
pred_class <- ifelse(predict(model, newdata = testing_data, type = "response") >= 0.5, 1, 0)
pred_class

accuracy <- mean(pred_class == testing_data$Outcome)
accuracy

# Create the confusion matrix
conf_matrix <- table(testing_data$Outcome, pred_class)

# Show the confusion matrix
print(conf_matrix)

# Convertirea matricei de confuzie într-un data frame pentru a fi utilizat în ggplot
conf_matrix_melted <- as.data.frame(as.table(conf_matrix))

# Redenumirea coloanelor pentru claritate
colnames(conf_matrix_melted) <- c("Predicted Class", "True Class", "Frequency")

# Inversarea ordinii factorilor pentru Predicted Class
conf_matrix_melted$`Predicted Class` <- factor(conf_matrix_melted$`Predicted Class`, levels = rev(levels(conf_matrix_melted$`Predicted Class`)))

# Crearea graficului cu ggplot2
library(ggplot2)
ggplot(data = conf_matrix_melted, aes(x = `True Class`, y = `Predicted Class`)) +
  geom_tile(aes(fill = `Frequency`), color = 'white') +
  geom_text(aes(label = sprintf("%d", `Frequency`)), vjust = 1, size = 6) +
  scale_fill_gradient(low="white", high="lightblue") +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45,hjust=1, size=14), # mărește dimensiunea fontului pentru etichetele axei x
        axis.text.y=element_text(size=14), # mărește dimensiunea fontului pentru etichetele axei y
        plot.title=element_text(size=16)) + # mărește dimensiunea fontului pentru titlul graficului
  labs(fill='Frequency', x='True Class', y='Predicted Class', title='Confusion Matrix')


# Calculate accuracy---------------------------------------------------------------------------
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))


#Importanta variabileor
importance <- varImp(model, scale = FALSE)

importance_df <- as.data.frame(importance)

# Create a ggplot bar plot
ggplot(importance_df, aes(x = rownames(importance_df), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Evaluarea modelelor ---------------------------------------------------------------------

# Predict probabilities for each model
predictions_model1 <- predict(model, testing_data, type = "response")
predictions_model2 <- predict(model2, testing_data_subset2, type = "response")
predictions_model3 <- predict(model3, testing_data_subset3, type = "response")

# Create prediction objects for ROCR
prediction_obj_model1 <- prediction(predictions_model1, testing_data$Outcome)
prediction_obj_model2 <- prediction(predictions_model2, testing_data_subset2$Outcome)
prediction_obj_model3 <- prediction(predictions_model3, testing_data_subset3$Outcome)

# Calculate performance measures for each model
performance_obj_model1 <- performance(prediction_obj_model1, "tpr", "fpr")
performance_obj_model2 <- performance(prediction_obj_model2, "tpr", "fpr")
performance_obj_model3 <- performance(prediction_obj_model3, "tpr", "fpr")

# Create an empty plot
plot(performance_obj_model1, col = "white", lwd = 2,
     main = "ROC Curve Comparison", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Add ROC curves for all three models
plot(performance_obj_model1, col = "blue", lwd = 2, add = TRUE)
plot(performance_obj_model2, col = "red", lwd = 2, add = TRUE)
plot(performance_obj_model3, col = "green", lwd = 2, add = TRUE)

# Add legend
legend("bottomright", legend = c("All variables", "Only Glucose", "Glucose and DPF"), 
       col = c("blue", "red", "green"), lwd = 2, cex = 0.7)

# Calculate AUC for each model
auc_model1 <- performance(prediction_obj_model1, "auc")@y.values[[1]]
auc_model2 <- performance(prediction_obj_model2, "auc")@y.values[[1]]
auc_model3 <- performance(prediction_obj_model3, "auc")@y.values[[1]]

cat("Area under the ROC curve (AUC) - All variables:", auc_model1, "\n")
cat("Area under the ROC curve (AUC) - Only Glucose:", auc_model2, "\n")
cat("Area under the ROC curve (AUC) - Glucose and DPF:", auc_model3, "\n")

