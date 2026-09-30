# STEP 5 - Exploratory Data Analysis

# Number of rows and columns
dim(happiness)

# Column names
names(happiness)

# Summary statistics
summary(happiness)

# Check missing values
colSums(is.na(happiness))
# STEP 6 - Data Cleaning

# Check missing values in important columns
colSums(is.na(happiness))

# Remove rows with missing values
happiness_clean <- na.omit(happiness)

# Check the cleaned dataset
dim(happiness_clean)

# Check missing values again
colSums(is.na(happiness_clean))
# STEP 7 - Descriptive Statistics

# Life evaluation
mean(happiness_clean$`Life evaluation (3-year average)`, na.rm = TRUE)

median(happiness_clean$`Life evaluation (3-year average)`, na.rm = TRUE)

sd(happiness_clean$`Life evaluation (3-year average)`, na.rm = TRUE)

# GDP
mean(happiness_clean$`Log GDP per capita`, na.rm = TRUE)

median(happiness_clean$`Log GDP per capita`, na.rm = TRUE)

sd(happiness_clean$`Log GDP per capita`, na.rm = TRUE)

# Social support
mean(happiness_clean$`Social support`, na.rm = TRUE)

median(happiness_clean$`Social support`, na.rm = TRUE)

sd(happiness_clean$`Social support`, na.rm = TRUE)
# STEP 7.3 - Summary Statistics

summary(
  happiness_clean[, c(
    "Life evaluation (3-year average)",
    "Explained by: Log GDP per capita",
    "Explained by: Social support",
    "Explained by: Healthy life expectancy",
    "Explained by: Freedom to make life choices",
    "Explained by: Generosity",
    "Explained by: Perceptions of corruption"
  )]
)
# STEP 8 - Correlation Analysis

# Select important numerical variables
cor_data <- happiness_clean[, c(
  "Life evaluation (3-year average)",
  "Explained by: Log GDP per capita",
  "Explained by: Social support",
  "Explained by: Healthy life expectancy",
  "Explained by: Freedom to make life choices",
  "Explained by: Generosity",
  "Explained by: Perceptions of corruption"
)]

# Calculate correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Display correlation matrix
round(cor_matrix, 2)
# STEP 8A - Select numerical columns

cor_data <- happiness_clean[, c(
  "Life evaluation (3-year average)",
  "Explained by: Log GDP per capita",
  "Explained by: Social support",
  "Explained by: Healthy life expectancy",
  "Explained by: Freedom to make life choices",
  "Explained by: Generosity",
  "Explained by: Perceptions of corruption"
)]
# STEP 9 - Visualization

plot(
  happiness_clean$`Explained by: Social support`,
  happiness_clean$`Life evaluation (3-year average)`,
  main = "Life Evaluation vs Social Support",
  xlab = "Social Support",
  ylab = "Life Evaluation",
  pch = 19
)
# Life Evaluation vs GDP

plot(
  happiness_clean$`Explained by: Log GDP per capita`,
  happiness_clean$`Life evaluation (3-year average)`,
  main = "Life Evaluation vs GDP per Capita",
  xlab = "Log GDP per Capita",
  ylab = "Life Evaluation",
  pch = 19
)
# STEP 10 - Hypothesis Testing

# Pearson correlation test

cor_test <- cor.test(
  happiness_clean$`Life evaluation (3-year average)`,
  happiness_clean$`Explained by: Social support`,
  method = "pearson"
)

# Display test results
cor_test
# Check Life Evaluation
head(happiness_clean$`Life evaluation (3-year average)`)

# Check Social Support
head(happiness_clean$`Explained by: Social support`)
cor.test(
  happiness_clean$`Life evaluation (3-year average)`,
  happiness_clean$`Explained by: Social support`,
  method = "pearson"
)
# STEP 11 - Predictive Modeling

# Build multiple linear regression model

model <- lm(
  `Life evaluation (3-year average)` ~
    `Explained by: Log GDP per capita` +
    `Explained by: Social support` +
    `Explained by: Healthy life expectancy` +
    `Explained by: Freedom to make life choices` +
    `Explained by: Generosity` +
    `Explained by: Perceptions of corruption`,
  data = happiness_clean
)

# Display model results
summary(model)
# STEP 11B - Simple regression model

model <- lm(
  `Life evaluation (3-year average)` ~
    `Explained by: Log GDP per capita` +
    `Explained by: Social support`,
  data = happiness_clean
)

summary(model)
# STEP 11 - Predictive Modeling

model <- lm(
  `Life evaluation (3-year average)` ~
    `Explained by: Social support`,
  data = happiness_clean
)

summary(model)
test_model <- lm(
  happiness_clean$`Life evaluation (3-year average)` ~
    happiness_clean$`Explained by: Social support`
)

summary(test_model)
# STEP 13 - Model Predictions

# Generate predictions
predicted_values <- predict(test_model)

# Display first 10 predictions
head(predicted_values, 10)
# STEP 14 - Actual vs Predicted

plot(
  happiness_clean[[4]],
  predicted_values,
  main = "Actual vs Predicted Life Evaluation",
  xlab = "Actual Life Evaluation",
  ylab = "Predicted Life Evaluation",
  pch = 19
)

abline(0, 1)
