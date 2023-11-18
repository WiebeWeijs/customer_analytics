#question 1
p = 0.6
Theta = 0.2
x = 3
n = 5
t= 4
j_max = 5
total_sum <- 0

#left side of formula  
q1 <- (p^x)*((1-p)^(n-x))*((1-Theta)^n) + total_sum

#right sidde formula
##(p^x)*((1-p)^(t-x+j))*Theta*((1-Theta)^(t+j))

# Loop over j from 1 to j_max
for (j in 1:j_max) {
  # Calculate the expression for each value of j
  expression_value <- sum((p^x) * ((1 - p)^(t - x + j)) * Theta * ((1 - Theta)^(t + j)))
  
  # Add the expression value to the total sum
  total_sum <- total_sum + expression_value
}

