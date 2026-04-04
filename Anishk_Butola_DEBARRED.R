# ============================================================
#  BCA 4th Semester | R Programming – Bug Fix Assignment
#  Student  : Anishk Butola
#  Status   : DEBARRED
#  Tasks    : Fix 6 Bugs (lines marked with BUG comment)
#  Deadline : 10 April 2026
#  NOTE     : Each fixed bug must have a comment explaining
#             WHAT was wrong and WHY you fixed it that way.
# ============================================================

# BUG 1 [Unit 1 - Variable Assignment]
# Code should print the square of 7
x <- 7
square = X * X          # BUG: wrong variable name (X vs x)
cat("Square:", square)
# YOUR FIX below:
x <- 7
sqare = x **2
cat("Square:", square)



# BUG 2 [Unit 1 - Vector Indexing]
# Should print the 3rd element of the vector
fruits <- c("Apple", "Banana", "Cherry", "Date")
cat("Third fruit:", fruits[4])  # BUG: wrong index (should be 3)
# YOUR FIX below:
cat(" third fruit: ", fruit[3])


# BUG 3 [Unit 2 - Data Frame Column Access]
# Should print the Marks column
students <- data.frame(Name=c("A","B","C"), Marks=c(78,85,90))
print(students$marks)   
# YOUR FIX below;
print(students$Name)
students$Grade=c("A+","B+","C+")
print(students)


# BUG 4 [Unit 3 - for Loop Range]
# Loop should print numbers 1 to 5 only
for (i in 1:10) {       
  cat(i, "")
}
# YOUR FIX below:
for (i in 1:5 ) {
  cat(i, "")
  }


# BUG 5 [Unit 3 - Function Return]
# Function should return the sum of two numbers
add_numbers <- function(a, b) {
  total <- a - b      
  return(total)
}
cat("Sum:", add_numbers(4, 5))
# YOUR FIX below:
add_number <-  function(a ,b) {
  total <- a + b      
  return(total)
}



# BUG 6 [Unit 3 - if/else Condition]
# Should print "Pass" when marks >= 40
marks <- 55
if (marks > 60) {      
  cat("Pass")
} else {
  cat("Fail")
}
# YOUR FIX below:
if (marks >= 40) {      
  cat("Pass")
} else {
  cat("Fail")
}
