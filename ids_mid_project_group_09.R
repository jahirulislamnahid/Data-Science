library(dplyr)

file <- read.csv("F:/spring/DataScienceProject/Placement_Data_Full_Class - modified.csv", na.strings = c("", "NA"))

dim(file)
names(file)
head(file)
summary(file)

original_rows <- nrow(file)
file <- unique(file)
new_rows <- nrow(file)
print(paste("Original rows:", original_rows))
print(paste("After removing duplicates:", new_rows))
print(paste("Duplicates removed:", original_rows - new_rows))

missing_values <- colSums(is.na(file))
print("Missing values in each column:")
print(missing_values)

median_salary <- median(file$salary, na.rm = TRUE)
print(paste("Median salary:", median_salary))
file$salary[is.na(file$salary)] <- median_salary

invalid_ssc <- sum(file$ssc_p < 0 | file$ssc_p > 100, na.rm = TRUE)
invalid_hsc <- sum(file$hsc_p < 0 | file$hsc_p > 100, na.rm = TRUE)
invalid_degree <- sum(file$degree_p < 0 | file$degree_p > 100, na.rm = TRUE)
invalid_mba <- sum(file$mba_p < 0 | file$mba_p > 100, na.rm = TRUE)

print(paste("Invalid ssc_p values:", invalid_ssc))
print(paste("Invalid hsc_p values:", invalid_hsc))
print(paste("Invalid degree_p values:", invalid_degree))
print(paste("Invalid mba_p values:", invalid_mba))

ssc_median <- median(file$ssc_p, na.rm = TRUE)
hsc_median <- median(file$hsc_p, na.rm = TRUE)
degree_median <- median(file$degree_p, na.rm = TRUE)
mba_median <- median(file$mba_p, na.rm = TRUE)

file$ssc_p[file$ssc_p < 0 | file$ssc_p > 100] <- ssc_median
file$hsc_p[file$hsc_p < 0 | file$hsc_p > 100] <- hsc_median
file$degree_p[file$degree_p < 0 | file$degree_p > 100] <- degree_median
file$mba_p[file$mba_p < 0 | file$mba_p > 100] <- mba_median



file$ssc_b[file$ssc_b == "Central"] <- 0
file$ssc_b[file$ssc_b == "Others"] <- 1
file$ssc_b <- as.numeric(file$ssc_b)

min_salary <- min(file$salary, na.rm = TRUE)
max_salary <- max(file$salary, na.rm = TRUE)
print(paste("Min salary:", min_salary))
print(paste("Max salary:", max_salary))
file$salary <- (file$salary - min_salary) / (max_salary - min_salary)

placed_count <- sum(file$class == 1)
not_placed_count <- sum(file$class == 0)

if (placed_count > not_placed_count) {
  majority_class <- 1
  minority_class <- 0
  minority_count <- not_placed_count
} else {
  majority_class <- 0
  minority_class <- 1
  minority_count <- placed_count
}

print(paste("Majority class:", majority_class, "with", max(placed_count, not_placed_count), "instances"))
print(paste("Minority class:", minority_class, "with", min(placed_count, not_placed_count), "instances"))


file$workex[file$workex == 0] <- "No"
file$workex[file$workex == 1] <- "Yes"


set.seed(123)
majority_indices <- which(file$class == majority_class)
sampled_indices <- sample(majority_indices, minority_count)
keep_indices <- c(sampled_indices, which(file$class == minority_class))

file <- file[keep_indices, ]

set.seed(123)
total_rows <- nrow(file)
train_size <- round(0.8 * total_rows)
train_indices <- sample(1:total_rows, train_size)

train_data <- file[train_indices, ]
test_data  <- file[-train_indices, ]

print(paste("Total balanced data rows:", total_rows))
print(paste("Training set rows:", nrow(train_data)))
print(paste("Testing set rows:", nrow(test_data)))

print("Training set class distribution:")
print(table(train_data$class))
print("Testing set class distribution:")
print(table(test_data$class))

ssc_mean <- mean(file$ssc_p, na.rm = TRUE)
ssc_median <- median(file$ssc_p, na.rm = TRUE)
ssc_table <- table(file$ssc_p)
ssc_mode <- as.numeric(names(ssc_table)[which.max(ssc_table)])

print("Central tendency for SSC Percentage:")
print(paste("Mean:", round(ssc_mean, 2)))
print(paste("Median:", ssc_median))
print(paste("Mode:", ssc_mode))

ssc_range <- max(file$ssc_p, na.rm = TRUE) - min(file$ssc_p, na.rm = TRUE)
ssc_iqr <- IQR(file$ssc_p, na.rm = TRUE)
ssc_variance <- var(file$ssc_p, na.rm = TRUE)
ssc_sd <- sd(file$ssc_p, na.rm = TRUE)

print("Spread measures for SSC Percentage:")
print(paste("Range:", round(ssc_range, 2)))
print(paste("IQR:", round(ssc_iqr, 2)))
print(paste("Variance:", round(ssc_variance, 2)))
print(paste("Standard Deviation:", round(ssc_sd, 2)))

mba_mean <- mean(file$mba_p, na.rm = TRUE)
mba_median <- median(file$mba_p, na.rm = TRUE)
mba_table <- table(file$mba_p)
mba_mode <- as.numeric(names(mba_table)[which.max(mba_table)])

print("Central tendency for MBA Percentage:")
print(paste("Mean:", round(mba_mean, 2)))
print(paste("Median:", mba_median))
print(paste("Mode:", mba_mode))

mba_range <- max(file$mba_p, na.rm = TRUE) - min(file$mba_p, na.rm = TRUE)
mba_iqr <- IQR(file$mba_p, na.rm = TRUE)
mba_variance <- var(file$mba_p, na.rm = TRUE)
mba_sd <- sd(file$mba_p, na.rm = TRUE)

print("Spread measures for MBA Percentage:")
print(paste("Range:", round(mba_range, 2)))
print(paste("IQR:", round(mba_iqr, 2)))
print(paste("Variance:", round(mba_variance, 2)))
print(paste("Standard Deviation:", round(mba_sd, 2)))


