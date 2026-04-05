# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Abhishek Kalura
#  Roll No  : BCA/4/SET4/02
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] Vectors & Arithmetic Operations ─────────────
cat("===== Q1: Vector Arithmetic =====\n")

A <- c(14, 27, 8, 55, 33)
B <- c(6,  13, 4, 20, 17)

cat("Vector A :", A, "\n")
cat("Vector B :", B, "\n\n")

ops <- list(
  Addition       = A + B,
  Subtraction    = A - B,
  Multiplication = A * B,
  Division       = round(A / B, 3),
  Modulus        = A %% B,
  Power          = A ^ 2
)

for (op_name in names(ops)) {
  cat(sprintf("%-16s: %s\n", op_name, paste(ops[[op_name]], collapse = "  ")))
}

cat(sprintf("\nDot product of A and B : %d\n",   sum(A * B)))
cat(sprintf("Euclidean distance     : %.4f\n\n", sqrt(sum((A - B)^2))))


# ── Q2 [Unit 1] Data Types, Class & Type Conversion ─────────
cat("===== Q2: Variables & Data Types =====\n")

full_name  <- "Abhishek Kalura"
roll_no    <- 2L
cgpa       <- 8.45
submitted  <- TRUE
dob        <- as.Date("2004-06-15")

vars <- list(
  full_name = full_name,
  roll_no   = roll_no,
  cgpa      = cgpa,
  submitted = submitted,
  dob       = dob
)

cat(sprintf("%-12s %-20s %-12s\n", "Variable", "Value", "Type"))
cat(strrep("-", 46), "\n")
for (vname in names(vars)) {
  cat(sprintf("%-12s %-20s %-12s\n",
              vname,
              as.character(vars[[vname]]),
              class(vars[[vname]])))
}

# Type conversions demo
cat("\nType Conversion Examples:\n")
cat(sprintf("  cgpa as integer : %d\n",        as.integer(cgpa)))
cat(sprintf("  roll_no as char : '%s'\n",       as.character(roll_no)))
cat(sprintf("  submitted as int: %d\n",         as.integer(submitted)))
cat(sprintf("  Age from DOB    : %d years\n\n",
            as.integer(difftime(Sys.Date(), dob, units = "days")) %/% 365L))


# ── Q3 [Unit 2] Data Frame — City Population Density ────────
cat("===== Q3: City Population Density Analysis =====\n")

cities <- data.frame(
  City       = c("Delhi", "Mumbai", "Bangalore", "Hyderabad", "Chennai",
                 "Kolkata", "Pune", "Ahmedabad", "Jaipur", "Surat"),
  Population = c(320, 205, 130, 100, 87, 148, 67, 80, 38, 72),  # in lakhs
  Area_sqkm  = c(1484, 603, 741, 650, 426, 185, 331, 464, 485, 395),
  stringsAsFactors = FALSE
)

cities$Density   <- round(cities$Population * 100000 / cities$Area_sqkm, 1)
cities$Category  <- cut(cities$Density,
                        breaks = c(0, 500, 1500, 5000, Inf),
                        labels = c("Low", "Medium", "High", "Very High"))

cities <- cities[order(-cities$Density), ]
cities$Rank <- seq_len(nrow(cities))

cat("City-wise Population Density:\n")
print(cities[, c("Rank","City","Population","Area_sqkm","Density","Category")])

cat(sprintf("\nMost Dense City  : %s (%.0f people/km²)\n",
            cities$City[1], cities$Density[1]))
cat(sprintf("Least Dense City : %s (%.0f people/km²)\n",
            cities$City[nrow(cities)], cities$Density[nrow(cities)]))
cat(sprintf("National Average : %.1f people/km²\n\n",
            mean(cities$Density)))

# Horizontal bar chart of density
par(mar = c(4, 8, 4, 2))
barplot(
  rev(cities$Density),
  names.arg = rev(cities$City),
  horiz     = TRUE,
  col       = colorRampPalette(c("#2196F3","#F44336"))(10),
  main      = "Population Density by City (people / km²)",
  xlab      = "Density",
  las       = 1,
  cex.names = 0.85,
  border    = NA
)


# ── Q4 [Unit 3] Loops, Functions & Closure ──────────────────
cat("===== Q4: Loops, Functions & Higher-Order Programming =====\n")

# 4a: Multiplication table with for loop
n <- 7L
cat(sprintf("Multiplication Table of %d:\n", n))
for (i in 1:12) {
  cat(sprintf("  %2d × %2d = %3d\n", n, i, n * i))
}

# 4b: while loop — find first number whose square > 500
cat("\nFirst integer whose square exceeds 500:\n")
x <- 1L
while (x^2 <= 500) x <- x + 1L
cat(sprintf("  %d² = %d  > 500 ✓\n\n", x, x^2))

# 4c: Function factory — make_multiplier returns a closure
make_multiplier <- function(factor) {
  function(x) x * factor
}

double   <- make_multiplier(2)
triple   <- make_multiplier(3)
tenfold  <- make_multiplier(10)

cat("Function Closures Demo:\n")
nums <- c(5, 12, 7, 20)
for (num in nums) {
  cat(sprintf("  %2d  →  ×2: %3d  |  ×3: %3d  |  ×10: %3d\n",
              num, double(num), triple(num), tenfold(num)))
}

# 4d: apply family — sapply + lapply
cat("\nSapply — Square roots of 1:10:\n")
cat(paste(round(sapply(1:10, sqrt), 3), collapse = "  "), "\n")

cat("\nLapply — Cubes of 1:5:\n")
cubes <- lapply(1:5, function(x) x^3)
cat(paste(unlist(cubes), collapse = "  "), "\n")
