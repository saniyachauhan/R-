# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Taniya
#  Roll No  : BCA/4/SET4/07
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] Temperature Converter ───────────────────────
cat("===== Q1: Temperature Conversion =====\n")

celsius_to_fahrenheit <- function(c) (c * 9/5) + 32
celsius_to_kelvin     <- function(c) c + 273.15
fahrenheit_to_celsius <- function(f) (f - 32) * 5/9

# Table of common temperatures
cat(sprintf("%-25s %10s %12s %12s\n",
            "Description", "Celsius", "Fahrenheit", "Kelvin"))
cat(strrep("-", 60), "\n")

temps <- list(
  list("Absolute Zero",         -273.15),
  list("Water Freezes",           0.00),
  list("Human Body",             37.00),
  list("Water Boils",           100.00),
  list("Dehradun Summer (avg)",  35.00),
  list("Dehradun Winter (avg)",   8.00)
)
for (t in temps) {
  c  <- t[[2]]
  cat(sprintf("%-25s %10.2f %12.2f %12.2f\n",
              t[[1]], c,
              celsius_to_fahrenheit(c),
              celsius_to_kelvin(c)))
}

# User input simulation
user_c <- 28.5
cat(sprintf("\nYou entered: %.1f°C\n", user_c))
cat(sprintf("Fahrenheit  : %.2f°F\n",  celsius_to_fahrenheit(user_c)))
cat(sprintf("Kelvin      : %.2f K\n\n", celsius_to_kelvin(user_c)))


# ── Q2 [Unit 1] Character Vector Operations ─────────────────
cat("===== Q2: Days of the Week — Vector Operations =====\n")

days <- c("Monday","Tuesday","Wednesday","Thursday",
          "Friday","Saturday","Sunday")

cat(sprintf("Days          : %s\n",       paste(days, collapse = ", ")))
cat(sprintf("Total elements: %d\n",       length(days)))
cat(sprintf("3rd to 5th    : %s\n",       paste(days[3:5], collapse = ", ")))
cat(sprintf("Uppercase     : %s\n",       paste(toupper(days), collapse = ", ")))
cat(sprintf("Reversed      : %s\n",       paste(rev(days), collapse = ", ")))
cat(sprintf("Weekdays only : %s\n",       paste(days[1:5], collapse = ", ")))
cat(sprintf("Weekend       : %s\n",       paste(days[6:7], collapse = ", ")))
cat(sprintf("Contains 'day': %s\n",
            paste(days[grepl("day", days, ignore.case = TRUE)],
                  collapse = ", ")))
cat(sprintf("Sorted alpha  : %s\n\n",
            paste(sort(days), collapse = ", ")))


# ── Q3 [Unit 2] Student Marks — Full Report Card ────────────
cat("===== Q3: Student Report Card Generator =====\n")

students <- data.frame(
  Name    = c("Taniya","Meera","Komal","Ritika","Pooja",
              "Anjali","Shreya","Divya"),
  Math    = c(88, 72, 95, 61, 78, 54, 82, 69),
  English = c(76, 81, 88, 74, 65, 70, 91, 77),
  Science = c(90, 68, 79, 85, 72, 63, 74, 88),
  stringsAsFactors = FALSE
)

students$Total      <- rowSums(students[, 2:4])
students$Percentage <- round(students$Total / 300 * 100, 2)
students$Grade      <- sapply(students$Percentage, function(p) {
  if      (p >= 90) "A+"
  else if (p >= 75) "A"
  else if (p >= 60) "B"
  else if (p >= 45) "C"
  else              "Fail"
})
students$Status     <- ifelse(students$Percentage >= 40, "PASS", "FAIL")
students            <- students[order(-students$Total), ]
students$Rank       <- seq_len(nrow(students))

cat("Report Card:\n")
print(students[, c("Rank","Name","Math","English","Science",
                   "Total","Percentage","Grade","Status")])

topper <- students[1, ]
cat(sprintf("\nClass Topper : %s — %.1f%% (%s)\n",
            topper$Name, topper$Percentage, topper$Grade))
cat(sprintf("Class Average: %.2f%%\n",  mean(students$Percentage)))
cat(sprintf("Pass Rate    : %d/%d\n\n",
            sum(students$Status == "PASS"), nrow(students)))

# Grouped bar chart
bar_data <- t(as.matrix(students[, c("Math","English","Science")]))
colnames(bar_data) <- students$Name
barplot(bar_data,
        beside  = TRUE,
        col     = c("#E57373","#64B5F6","#81C784"),
        main    = "Subject-wise Marks Comparison",
        xlab    = "Students",
        ylab    = "Marks",
        las     = 2,
        cex.names = 0.8,
        legend.text = c("Math","English","Science"),
        args.legend = list(x = "topright"))


# ── Q4 [Unit 3] BMI Calculator Function ─────────────────────
cat("===== Q4: BMI Calculator =====\n")

bmi_calculator <- function(weight_kg, height_m) {
  if (!is.numeric(weight_kg) || !is.numeric(height_m))
    stop("Weight and height must be numeric.")
  if (weight_kg <= 0 || height_m <= 0)
    stop("Weight and height must be positive.")
  if (height_m > 3)
    stop("Height seems to be in cm — please enter in metres.")

  bmi <- weight_kg / (height_m ^ 2)

  category <- if      (bmi < 18.5)              "Underweight"
               else if (bmi < 25.0)              "Normal weight"
               else if (bmi < 30.0)              "Overweight"
               else if (bmi < 35.0)              "Obese (Class I)"
               else if (bmi < 40.0)              "Obese (Class II)"
               else                               "Obese (Class III)"

  recommendation <- switch(category,
    "Underweight"      = "Increase caloric intake; consult a nutritionist.",
    "Normal weight"    = "Maintain your healthy lifestyle!",
    "Overweight"       = "Consider regular exercise and a balanced diet.",
    "Obese (Class I)"  = "Seek medical advice; start a structured diet plan.",
    "Obese (Class II)" = "Medical consultation strongly recommended.",
    "Obese (Class III)"= "Immediate medical attention advised."
  )

  list(bmi = round(bmi, 2), category = category,
       recommendation = recommendation)
}

test_cases <- list(
  list("Taniya",   55,  1.62),
  list("Person B", 80,  1.75),
  list("Person C", 45,  1.70),
  list("Person D", 100, 1.68),
  list("Person E", 72,  1.80)
)

cat(sprintf("%-12s %6s %6s %6s %-20s\n",
            "Name","Weight","Height","BMI","Category"))
cat(strrep("-", 60), "\n")
for (tc in test_cases) {
  result <- bmi_calculator(tc[[2]], tc[[3]])
  cat(sprintf("%-12s %6.1f %6.2f %6.2f %-20s\n",
              tc[[1]], tc[[2]], tc[[3]],
              result$bmi, result$category))
}
cat("\nRecommendations:\n")
for (tc in test_cases) {
  result <- bmi_calculator(tc[[2]], tc[[3]])
  cat(sprintf("  %-12s: %s\n", tc[[1]], result$recommendation))
}
