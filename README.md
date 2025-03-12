---
title: 'Individual Assignment II '
author: "Group(1) Kyaw Min Khaing"
output:
  word_document: default
  html_document: default
  pdf_document: default
editor_options: 
  markdown: 
    wrap: 72
---

### Load necessary packages

```{r}
library(pacman)
p_load(tidyverse,rstatix,randtests,ggpubr,knitr)
```

#### No-1(a) A physician who specializes in genetic diseases develops a theory which predicts that two-thirds of the people who develop a disease called cyclomeiosis will be males. She randomly selects 300 people who are afflicted with cyclomeiosis and observes that 140 of them are females. Is the physician’s theory supported? Above the median (Males30 Females60) Below the median (Males70 Females60)

## Hypothesis Statements

-   **Null Hypothesis (H0):** The proportion of males afflicted is
    **2/3**.
-   **Alternative Hypothesis (HA):** The proportion of males afflicted
    is **not 2/3**. The Chi-Square test statistic ($\chi^2$) is
    calculated using the formula:

$$
\chi^2 = \sum \frac{(O - E)^2}{E}
$$

where:\
- $O$ = Observed frequency\
- $E$ = Expected frequency

```{r 1(a)define-data}
# Given observed frequencies
observed <- c(Male = 160, Female = 140)

# Expected proportions
expected_prop <- c(Male = 2/3, Female = 1/3)

# Sample size
N <- sum(observed)

# Expected frequencies
expected <- expected_prop * N

# Combine into a tibble
data <- tibble(
  Category = names(observed),
  Observed = observed,
  Expected = expected,
  Chi_Square_Contribution = (Observed - Expected)^2 / Expected
)
kable(data, caption = "chi_square_contribution")
```

Performing the Chi-Square Test

```{r 1(a)chisq-test}
chi_sq_stat <- sum(data$Chi_Square_Contribution)

# Degrees of freedom (df = categories - 1)
df <- length(observed) - 1

# Critical value at alpha = 0.05
critical_value <- qchisq(0.05, df, lower.tail = FALSE)

# Decision Rule
decision <- ifelse(chi_sq_stat >= critical_value, "Reject H0", "Fail to Reject H0")

# Display results
tibble(Chi_Square_Statistic = chi_sq_stat, Critical_Value = critical_value, Decision = decision)
```

```         
Conclusion: there is no supportive predictions of the physician’s theory, stating that the proportion of males among those afflicted is  not 2/3  .
```

#### Visualize observed vs. expected counts

```{r 2(a)Visualize}
library(ggplot2)

ggplot(data, aes(x = Category, y = Observed, fill = Category)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_point(aes(y = Expected), color = "red", size = 3) +
  geom_segment(aes(xend = Category, y = Expected, yend = Observed), color = "red", linetype = "dashed") +
  labs(
    title = "Chi-Square Goodness-of-Fit Test: Cyclomeiosis",
    subtitle = "Comparison of Observed and Expected Frequencies",
    y = "Frequency",
    x = "Category"
  ) +
  theme_minimal()
```

#### Interpretation

Based on the computed Chi-Square statistic and critical value, we
conclude: - If the test statistic is **greater than or equal to** the
critical value, we **reject H0**. - Otherwise, we **fail to reject H0**.

This result indicates whether the physician’s theory is supported or
not.

## No-1(b) A study is conducted to determine whether five-year old females are more likely than five-year old males to score above the population median on a standardized test of eye-hand coordination. One hundred randomly selected females and 100 randomly selected males are © 2000 by Chapman & Hall/CRC administered the test of eye-hand coordination, and categorized with respect to whether they score above or below the overall population median (i.e., the 50th percentile for both males and females). Table 16.11 summarizes the results of the study. Do the data indicate that there are gender differences in performance? Above the median (Males30 Females60) Below the median (Males70 Females60)

```{r 1(b)data-preparation}
# Create a dataframe for the study
study_data <- data.frame(
  Gender = c("Males", "Females"),
  Above_Median = c(30, 60),
  Below_Median = c(70, 40)
)
# Print the data
kable(study_data, caption = "study_data")
```

We use a Chi-Square test for independence to determine if there is a
significant association between gender and test performance.

```{r  1(b)Chi-Square test}
# Create a contingency table
contingency_table <- as.table(rbind(
  c(30, 70),  # Males
  c(60, 40)   # Females
))
rownames(contingency_table) <- c("Males", "Females")
colnames(contingency_table) <- c("Above Median", "Below Median")
# Perform the Chi-Square test
chisq_test <- chisq.test(contingency_table)
# Display the results
chisq_test
```

### Report significance

```{r}

alpha <- 0.05  # Significance level
if (chisq_test$p.value < alpha) {
  cat("The result is significant at α =", alpha, 
      "\n(p-value =", round(chisq_test$p.value, 4), ").",
      "\nThere is evidence of a relationship between gender and performance.\n")
} else {
  cat("The result is not significant at α =", alpha, 
      "\n(p-value =", round(chisq_test$p.value, 4), ").",
      "\nThere is no evidence of a relationship between gender and performance.\n")
}
```

### Visualization

```{r}
# Transform the data for plotting
plot_data <- study_data %>%
  pivot_longer(cols = c(Above_Median, Below_Median),
               names_to = "Score_Category",
               values_to = "Count") %>%
  mutate(Score_Category = factor(Score_Category, levels = c("Above_Median", "Below_Median")))

# Plot
gender_plot <- ggbarplot(
  plot_data, x = "Gender", y = "Count", fill = "Score_Category",
  color = "white", position = position_dodge(),
  xlab = "Gender", ylab = "Count",
  legend.title = "Score Category",
  title = "Performance by Gender in Eye-Hand Coordination Test"
) +
  theme_minimal()
gender_plot
```

## No-2(a) Doctor Radical, a math instructor at Logarithm University, has three classes in advanced calculus. There are five students in each class. The instructor uses a programmed textbook in Class 1, a conventional textbook in Class 2, and his own printed notes in Class 3. At the end of the semester, in order to determine if the type of instruction employed influences student performance, Dr. Radical has another math instructor, Dr. Root, rank the 15 students in the three classes with respect to math ability. The rankings of the students in the three classes follow: Class 1: 9.5, 14.5, 12.5, 14.5, 12.5; Class 2: 6, 9.5, 3, 9.5, 3; and Class 3: 1, 9.5, 6, 3, 6 (assume the lower the rank, the better the student). At α = 0.05, is there a difference three classes in advanced calculus.?

### To input Dataset

The rankings of the students in the three classes are as follows: Class
1 (Programmed Textbook): 9.5, 14.5, 12.5, 14.5, 12.5 Class 2
(Conventional Textbook): 6, 9.5, 3, 9.5, 3 Class 3 (Printed Notes): 1,
9.5, 6, 3, 6

```{r 2(a)data}
data <- tibble(
  Class = rep(c("Class 1", "Class 2", "Class 3"), each = 5),
  Rank = c(
    9.5, 14.5, 12.5, 14.5, 12.5,  # Class 1
    6, 9.5, 3, 9.5, 3,             # Class 2
    1, 9.5, 6, 3, 6               # Class 3
  )
)
print(data)
```

### Kruskal-Wallis Test

```{r  2(a)Kruskal-Wallis Test}
kruskal_result <- data %>%
  kruskal_test(Rank ~ Class)
# Print the result
print(kruskal_result)
```

### Check for significance

```{r}
alpha <- 0.05  # Significance level
if (kruskal_result$p < alpha) {
  cat("The result is significant at α =", alpha, 
      "\n(p-value =", round(kruskal_result$p, 4), ").",
      "\nThere is evidence that at least one group differs significantly.\n")
} else {
  cat("The result is not significant at α =", alpha, 
      "\n(p-value =", round(kruskal_result$p, 4), ").",
      "\nThere is no evidence that the groups differ significantly.\n")
}
```

### Pairwise Comparisons

If the Kruskal-Wallis test is significant, we will perform pairwise
comparisons using the Dunn test with Bonferroni correction.

```{r 2(a)Pairwise Comparisons}
pairwise_result <- data %>%
  dunn_test(Rank ~ Class, p.adjust.method = "bonferroni") %>%
  add_xy_position(x = "Class", step.increase = 0.2) %>%
  mutate(label = paste0("p = ", signif(p.adj, 4))) # Adds x, y positions for annotation
print(pairwise_result)
```

### Visualization

We will create a boxplot to visualize the rank distributions across the
three classes.

```{r 2(a)Visualization}
ggplot(data, aes(x = Class, y = Rank, fill = Class)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + # Boxplot
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "blue") + # Mean point
  labs(
    title = "Rank Distribution by Class",
    subtitle = paste("Kruskal-Wallis p =", round(kruskal_result$p, 4)),
    x = "Class",
    y = "Rank"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

## No-2(b) A quality control study is conducted on a machine that pours milk into containers. The amount of milk (in liters) dispensed by the machine into 21 consecutive containers follows: 1.90, 1.99, 2.00, 1.78, 1.77, 1.76, 1.98, 1.90, 1.65, 1.76, 2.01, 1.78, 1.99, 1,76, 1.94, 1.78, 1.67, 1.87, 1.91, 1.91, 1.89. Are the successive increments and decrements in the amount of milk dispensed random?

### Introduction

In this study, we aim to determine whether the successive increments and
decrements in the amount of milk dispensed by a machine are random. The
dataset contains the amount of milk (in liters) dispensed into 21
consecutive containers.

```{r 2(b)data_preparation}
data <- tibble(
  Container = 1:21,
  Milk_Amount = c(1.90, 1.99, 2.00, 1.78, 1.77, 1.76, 1.98, 1.90, 1.65, 1.76,
                  2.01, 1.78, 1.99, 1.76, 1.94, 1.78, 1.67, 1.87, 1.91, 1.91, 1.89)
)
print(data)
```

### Runs Test for Randomness

To test the randomness of successive increments and decrements in the
amount of milk dispensed, we first create a binary sequence that
represents whether each successive value increases or decreases compared
to the previous value.

```{r 2(b)runs-test}
data <- data %>%
  mutate(Change = ifelse(Milk_Amount > lag(Milk_Amount), 1, 0)) %>%
  drop_na()
# Perform Runs Test
runs_test_result <- runs.test(data$Change)
# Display the result
runs_test_result
```

```         
Conclusion:  The null hypothesis is not rejected, suggesting that the successive increments and decrements in the amount of milk dispensed are randomly distributed. 
```

### Visualization

We can visualize the changes in the amount of milk dispensed to better
understand the pattern of increments and decrements.

```{r 2(b)visualization}
ggplot(data, aes(x = Container, y = Milk_Amount)) +
  geom_line(color = "blue", size = 1) +
  geom_point(aes(color = factor(Change)), size = 3) +
  scale_color_manual(
    values = c("0" = "red", "1" = "green"),
    labels = c("Decrease", "Increase"),
    name = "Change"
  ) +
  labs(
    title = "Milk Dispensed by Container",
    x = "Container Number",
    y = "Milk Amount (liters)"
  ) +
  theme_minimal()
```

## No-3(a)A pediatrician speculates that the length of time an infant is breast fed may be related to how often a child becomes ill. In order to answer the question, the pediatrician obtains the following two scores for five three-year-old children: The number of months the child was breast fed (which represents the X variable) and the number of times the child was brought to the pediatrician’s office during the current year (which represents the Y variable). The scores for the five children follow: Child 1 (20, 7); Child 2 (0, 0); Child 3 (1, 2); Child 4 (12, 5); Child 5 (3, 3). Do the data indicate that the length of time a child is breast fed is related to the number of times a child is brought to the pediatrician?

```{r 3(a)data_preparation}
# Create dataset
data <- tibble(
  Child = 1:5,
  Breastfeeding_Months = c(20, 0, 1, 12, 3),
  Pediatric_Visits = c(7, 0, 2, 5, 3)
)

# Display dataset
kable(data, caption = "Breastfeeding Duration and Pediatric Visits")
```

We calculate the Pearson correlation coefficient to determine the
relationship between the variables.

```{r 3(a)correlation}
ranked_data <- data %>%
  mutate(
    Rank_Breastfeeding = rank(Breastfeeding_Months, ties.method = "average"),
    Rank_Visits = rank(Pediatric_Visits, ties.method = "average"),
    D = Rank_Breastfeeding - Rank_Visits,  
    D_squared = D^2
  )

# Display ranked data
kable(ranked_data, caption = "Ranked Data and D-Squared Values")
```

$$
  r_s = 1 - \frac{6 \sum D^2}{n(n^2 - 1)}
$$

where: - $D$ is the difference between ranks, - $n$ is the number of
observations.

```{r 3(a)scatterplot}
# Create scatter plot
# Compute Spearman's rank correlation coefficient
n <- nrow(ranked_data)
D_squared_sum <- sum(ranked_data$D_squared)

r_s <- 1 - (6 * D_squared_sum) / (n * (n^2 - 1))

# Display Spearman's correlation coefficient
cat("Spearman Rank Correlation Coefficient (ρ):", round(r_s, 4), "\n")
```

```{r}
# Decision Rule
if (abs(r_s) >= 1.000) {
  cat("The correlation is statistically significant. We reject H₀ and conclude there is a significant correlation.\n")
} else {
  cat("The correlation is not statistically significant. We fail to reject H₀.\n")
}

```

reporting

```{r}
cat("A Spearman rank-order correlation was conducted to determine the relationship between breastfeeding duration and pediatric visits. The analysis yielded ρ =", round(r_s, 4), ". Since the obtained ρ is", ifelse(abs(r_s) >= 1.000, "greater", "less"), "than the critical value of 1.000, we", ifelse(abs(r_s) >= 1.000, "reject", "fail to reject"), "the null hypothesis. This suggests that there", ifelse(abs(r_s) >= 1.000, "is", "is no"), "significant correlation between the two variables.")
```

Visualization (Scatterplot)

```{r}
ggplot(data, aes(x = Breastfeeding_Months, y = Pediatric_Visits)) +
  geom_point(size = 4, color = "blue") +  # Data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Trend line
  labs(
    title = "Scatterplot of Breastfeeding Duration vs. Pediatric Visits",
    x = "Breastfeeding Duration (Months)",
    y = "Number of Pediatric Visits"
  ) +
  theme_minimal()

```

## No-3(b) A study is conducted to determine whether there is a correlation between handedness and eye-hand coordination. Five right-handed and five left-handed subjects are administered a test of eye-hand coordination. The test scores of the subjects follow (the higher a subject’s score, the better his or her eye-hand coordination): Right-handers: 11, 1, 0, 2, 0; Left-handers: 11, 11, 5, 8, 4. Is there a statistical relationship between handedness and eye hand coordination?

Hypothesis: H_0: ρ_b=0 (there is no relationship between handedness and
eye-hand coordination; the distributions of eye-hand coordination scores
are the not same for both handedness groups) H_A: ρ_b≠ 0 (there is
relationship between handedness and eye hand coordination; the
distributions are not different)

```{r 3(b)data prepation}
data <- tibble(
  Handedness = c(rep("Right", 5), rep("Left", 5)),
  Score = c(11, 1, 0, 2, 0, 11, 11, 5, 8, 4)
)

# Convert Handedness to a binary variable (1 = Right, 0 = Left)
data <- data %>%
  mutate(Handedness_Binary = ifelse(Handedness == "Right", 1, 0))

# Display dataset
kable(data, caption = "Eye-Hand Coordination Scores by Handedness")
```

$$
r_b = \left[ \frac{\bar{x}_p - \bar{x}_q}{s_x} \right] \sqrt{\frac{P_p P_q}{y}}
$$

```{r}
# Compute means
x_p <- mean(data$Score[data$Handedness_Binary == 1])
x_q <- mean(data$Score[data$Handedness_Binary == 0])

# Compute standard deviation of all scores
s_x <- sd(data$Score)

# Compute proportions
p <- mean(data$Handedness_Binary)
q <- 1 - p
y <- nrow(data)

# Compute Point-Biserial Correlation
r_b <- ((x_p - x_q) / s_x) * sqrt((p * q) / y)

# Display result
cat("Point-Biserial Correlation (r_b):", round(r_b, 4), "\n")
```

Interpret the Results Decision Rule

```{r}
# Decision Rule
# Reporting
cat("A point-biserial correlation was conducted to determine the relationship between handedness and eye-hand coordination. The analysis yielded r_b =", round(r_b, 4), ". Since the obtained r_b is", ifelse(abs(r_b) >= critical_value, "greater", "less"), "than the critical value of 0.632, we", ifelse(abs(r_b) >= critical_value, "reject", "fail to reject"), "the null hypothesis. This suggests that there", ifelse(abs(r_b) >= critical_value, "is", "is no"), "significant correlation between the two variables.")

```

Visualization (Boxplot)

```{r}
ggplot(data, aes(x = Handedness, y = Score, fill = Handedness)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, size = 3, color = "black") +
  labs(
    title = "Boxplot of Eye-Hand Coordination Scores by Handedness",
    x = "Handedness",
    y = "Eye-Hand Coordination Score"
  ) +
  theme_minimal()

```

