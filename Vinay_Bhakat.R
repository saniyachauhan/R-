# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Vinay Bhakat
#  Roll No  : BCA/4/SET4/03
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] Arithmetic I/O & Formatted Table ────────────
cat("===== Q1: Calculator with Formatted Output =====\n")

# Using predefined values (readline not supported in batch)
num1 <- 48
num2 <- 7

ops_results <- data.frame(
  Operation  = c("Addition", "Subtraction", "Multiplication",
                 "Division", "Integer Div", "Modulus", "Power"),
  Expression = c(
    sprintf("%g + %g", num1, num2),
    sprintf("%g - %g", num1, num2),
    sprintf("%g × %g", num1, num2),
    sprintf("%g ÷ %g", num1, num2),
    sprintf("%g %/% %g", num1, num2),
    sprintf("%g %% %g",  num1, num2),
    sprintf("%g ^ %g",   num1, num2)
  ),
  Result = c(
    num1 + num2,
    num1 - num2,
    num1 * num2,
    round(num1 / num2, 6),
    num1 %/% num2,
    num1 %% num2,
    num1 ^ num2
  )
)

cat(sprintf("  Numbers entered: a = %g,  b = %g\n\n", num1, num2))
cat(sprintf("  %-18s %-16s %s\n", "Operation", "Expression", "Result"))
cat("  ", strrep("-", 50), "\n", sep = "")
for (i in seq_len(nrow(ops_results))) {
  cat(sprintf("  %-18s %-16s %g\n",
              ops_results$Operation[i],
              ops_results$Expression[i],
              ops_results$Result[i]))
}
cat("\n")


# ── Q2 [Unit 1] Named List & Logical Vectors ────────────────
cat("===== Q2: Student Record Using Named List =====\n")

student_record <- list(
  name      = "Vinay Bhakat",
  roll      = 3L,
  subjects  = c("Data Structures", "DBMS", "OS", "R Programming", "Maths"),
  marks     = c(82, 76, 68, 91, 55),
  cgpa      = 7.9
)

student_record$pass_fail <- student_record$marks >= 40
student_record$percentage <- round(student_record$marks / 100 * 100, 1)

cat("Student Record:\n")
cat(sprintf("  Name    : %s\n",    student_record$name))
cat(sprintf("  Roll No : %d\n",    student_record$roll))
cat(sprintf("  CGPA    : %.1f\n\n", student_record$cgpa))

cat("  Subject-wise Result:\n")
cat(sprintf("  %-20s %5s  %5s  %6s\n", "Subject", "Marks", "Pass?", "Grade"))
cat("  ", strrep("-", 45), "\n", sep = "")
for (i in seq_along(student_record$subjects)) {
  grade <- if      (student_record$marks[i] >= 80) "A"
            else if (student_record$marks[i] >= 65) "B"
            else if (student_record$marks[i] >= 50) "C"
            else if (student_record$marks[i] >= 40) "D"
            else                                    "F"
  cat(sprintf("  %-20s %5d  %5s  %6s\n",
              student_record$subjects[i],
              student_record$marks[i],
              ifelse(student_record$pass_fail[i], "YES", "NO"),
              grade))
}
cat(sprintf("\n  Average: %.1f  |  Passed: %d/%d subjects\n\n",
            mean(student_record$marks),
            sum(student_record$pass_fail),
            length(student_record$marks)))


# ── Q3 [Unit 2] mtcars Dataset — Histogram + Density + Line ─
cat("===== Q3: mtcars MPG Analysis =====\n")

data(mtcars)
mpg       <- mtcars$mpg
mpg_mean  <- mean(mpg)
mpg_med   <- median(mpg)
mpg_sd    <- sd(mpg)

cat(sprintf("MPG Statistics from mtcars dataset (n = %d cars):\n", nrow(mtcars)))
cat(sprintf("  Mean   : %.2f  |  Median : %.2f\n",  mpg_mean, mpg_med))
cat(sprintf("  Std Dev: %.2f  |  Range  : %.1f – %.1f\n",
            mpg_sd, min(mpg), max(mpg)))
cat(sprintf("  Q1     : %.2f  |  Q3     : %.2f\n\n",
            quantile(mpg, 0.25), quantile(mpg, 0.75)))

# Histogram with density overlay
hist(mpg,
     breaks  = 10,
     col     = "#42A5F5",
     border  = "white",
     freq    = FALSE,
     main    = "Distribution of MPG — mtcars Dataset",
     xlab    = "Miles Per Gallon (MPG)",
     ylab    = "Density",
     xlim    = c(5, 45))
lines(density(mpg), col = "#E53935", lwd = 2.5)
abline(v = mpg_mean, col = "#FF6F00", lty = 2, lwd = 2)
abline(v = mpg_med,  col = "#2E7D32", lty = 3, lwd = 2)
legend("topright",
       legend = c("Density Curve", sprintf("Mean (%.1f)", mpg_mean),
                  sprintf("Median (%.1f)", mpg_med)),
       col  = c("#E53935","#FF6F00","#2E7D32"),
       lty  = c(1, 2, 3), lwd = 2, cex = 0.85)

# Cylinder group analysis
cat("MPG grouped by Cylinders:\n")
for (cyl in sort(unique(mtcars$cyl))) {
  grp <- mtcars$mpg[mtcars$cyl == cyl]
  cat(sprintf("  %d-cyl: n=%-3d mean=%-6.2f min=%-5.1f max=%.1f\n",
              cyl, length(grp), mean(grp), min(grp), max(grp)))
}
cat("\n")


# ── Q4 [Unit 3] is_prime + Sieve of Eratosthenes ────────────
cat("===== Q4: Prime Numbers — Two Methods =====\n")

# Method 1: Trial division
is_prime_trial <- function(n) {
  if (n < 2) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  for (i in seq(3, floor(sqrt(n)), by = 2)) {
    if (n %% i == 0) return(FALSE)
  }
  TRUE
}

# Method 2: Sieve of Eratosthenes (efficient for large ranges)
sieve <- function(limit) {
  if (limit < 2) return(integer(0))
  composite <- logical(limit)   # all FALSE = all potentially prime
  composite[1] <- TRUE
  for (i in 2:floor(sqrt(limit))) {
    if (!composite[i]) {
      j <- i * i
      while (j <= limit) {
        composite[j] <- TRUE
        j <- j + i
      }
    }
  }
  which(!composite)
}

# Compare results
test_nums <- c(2, 3, 10, 13, 25, 29, 97, 100, 101)
cat("Trial Division Results:\n")
for (n in test_nums) {
  cat(sprintf("  is_prime(%3d) = %s\n", n, is_prime_trial(n)))
}

cat("\nAll primes ≤ 100 via Sieve of Eratosthenes:\n")
prime_list <- sieve(100)
cat(paste(prime_list, collapse = "  "), "\n")
cat(sprintf("Count: %d primes found below 100\n\n", length(prime_list)))

# Performance check: count primes up to 10,000
cat("Primes up to 10,000:")
big_primes <- sieve(10000)
cat(sprintf("  %d prime numbers found.\n", length(big_primes)))
cat(sprintf("  Largest prime ≤ 10000 : %d\n", max(big_primes)))
