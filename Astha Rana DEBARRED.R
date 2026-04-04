# ============================================================
#  BCA 4th Semester | R Programming – Bug Fix Assignment
#  Student  : Astha Rana 
#  Status   : DEBARRED
#  Tasks    : Fix 6 Bugs (lines marked with BUG comment)
#  Deadline : 10 April 2026
#  NOTE     : Each fixed bug must have a comment explaining
#             WHAT was wrong and WHY you fixed it that way.
# ============================================================

# BUG 1 [Unit 1 - Variable Assignment]
# Code should print the square of 7
x <- 7
square = X * X         
cat("Square:", square)
# YOUR FIX below:


# BUG 2 [Unit 1 - Vector Indexing]
# Should print the 3rd element of the vector
fruits <- c("Apple", "Banana", "Cherry", "Date")
cat("Third fruit:", fruits[4])  
# YOUR FIX below:


# BUG 3 [Unit 2 - Data Frame Column Access]
# Should print the Marks column
#Add Grade Column and Percentage column 
students <- data.frame(Name=c("A","B","C"), Marks=c(78,85,90))
print(students$marks)   
# YOUR FIX below:


# BUG 4 [Unit 3 - for Loop Range]
# Loop should print numbers 1 to 55 only

for (i in 1:10) {       
  cat(i, "")
}
# YOUR FIX below:


# BUG 5 
# Function should return the sum of two numbers

Create a vector of 15 random temperatures between 20-45°C. Find mean, median, and count how many days exceeded 38°C.
# YOUR FIX below:


# BUG 6 [Unit 3 - if/else Condition]
# Should print "Pass" when marks >= 40
marks <- 55
if (marks > 60) {       
  cat("Pass")
} else {
  cat("Fail")
}
# YOUR FIX below:
