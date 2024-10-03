# Load necessary libraries
library(dplyr)    # For data manipulation
library(car)      # For Levene's test
library(FSA)      # For Dunn's post-hoc test

# Filter data for specific groups
# Replace 'data_frame' with your actual data frame
data_filtered <- data_frame %>%
  filter(group_column %in% c("group"))

#### PARAMETRIC TESTS ####

# 1. Shapiro-Wilk test for normality on 'variable'
shapiro_test <- shapiro.test(data_filtered$variable)
print(shapiro_test)

# Check for normality based on Shapiro-Wilk test result for 'variable'
if (shapiro_test$p.value >= 0.05) {
  print("Variable is normally distributed (Shapiro-Wilk test, p > 0.05)")
} else {
  print("Variable is not normally distributed (Shapiro-Wilk test, p <= 0.05)")
}

# Generate Q-Q plot for 'variable'
qqnorm(data_filtered$variable)
qqline(data_filtered$variable)

# 2. Levene's test for homogeneity of variances (variable across groups)
levene_test <- leveneTest(variable ~ group_column, data = data_filtered)
print(levene_test)

# 3. One-way ANOVA for 'variable'
anova_result <- aov(variable ~ group_column, data = data_filtered)
summary(anova_result)

# 4. Post-hoc analysis using Tukey's HSD test (for ANOVA)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 5. Two-sample t-test for 'variable1' between 'group1' and 'group2'
# Subset data for group1 and group2
data_group1 <- subset(data_filtered, group_column == "group1")
data_group2 <- subset(data_filtered, group_column == "group2")

t_test_result <- t.test(data_group1$variable data_group2$variable)
print(t_test_result)


#### NON-PARAMETRIC TESTS ####

# 1. Kruskal-Wallis test for 'variable' (non-parametric alternative to ANOVA)
kruskal_test <- kruskal.test(variable ~ group_column, data = data_filtered)
print(kruskal_test)

# 2. Dunn's post-hoc test for Kruskal-Wallis using PMCMRplus
dunn_test <- dunnTest(variable ~ group_column, data = data_filtered, method = "bonferroni")
print(dunn_test)

# 3. Mann-Whitney U test for 'variable1' between 'group1' and 'group2' (non-parametric alternative to t-test)
mannwhitney_result <- wilcox.test(data_group1$variable, data_group2$variable)
print(mannwhitney_result)
