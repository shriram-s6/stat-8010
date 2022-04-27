# An Introduction to Statistical Methods and Data-Analysis 6th Edition 
# page no 548

# exercise 10.13

y = 229
n = 500

pi_hat = y / n
print(pi_hat)
# 0.458

alpha = 0.10
z_value = qnorm(alpha / 2, lower.tail=F)
print(z_value)
# 1.644854

# constructing confidence interval

lower_bound = pi_hat - (z_value * sqrt(((pi_hat) * (1 - pi_hat)) / n))
print(lower_bound)
# 0.4213499

upper_bound = pi_hat + (z_value * sqrt(((pi_hat) * (1 - pi_hat)) / n))
print(upper_bound)
# 0.4946501

# exercise 10.19, page 549

y1 = 226
n1 = 473
pi_hat1 = y1 / n1
print(pi_hat1)
# 0.4778013

y2 = 165
n2 = 439
pi_hat2 = y2 / n2
print(pi_hat2)
# 0.3758542

# constructing 95 % confidence interval

sigma_hat = sqrt(((pi_hat1 * (1 - pi_hat1)) / n1) + ((pi_hat2 * (1 - pi_hat2)) / n2))
print(sigma_hat)
# 0.03258632

alpha = 0.05

z_value = qnorm(alpha / 2, lower.tail=FALSE)
print(z_value)
# 1.959964

lower_bound = (pi_hat1 - pi_hat2) - (z_value * sigma_hat)
# 0.03807905

upper_bound = (pi_hat1 - pi_hat2) + (z_value * sigma_hat)
# 0.1658151

# exercise 10.43, page no 556

age = matrix(c(38, 42, 82, 88), ncol=2, byrow=TRUE)
colnames(age) = c('less_than_39', 'greater_or_equal_40')
rownames(age) = c('promoted', 'not_promoted')
age = as.table(age)
print(age)

# null hypothesis, promotion decision is independent of age
# alternative hypothesis, promotion decision is dependent upon age
age_chisquare = chisq.test(age, correct=FALSE)
print(age_chisquare)
# 0.011784

# the test statistic can be calculated manually

expected_values = age_chisquare$expected
print(expected_values)

test_statistic = 0

for (i in 1:4) {
  residual = (age[i] - expected_values[i]) ** 2 / (expected_values[i])
  test_statistic = test_statistic + residual
}

print(test_statistic)
# 0.01178356

alpha = 0.05
df = (nrow(age) - 1) * (ncol(age) - 1) 
chi_square_val = qchisq(1 - alpha, df)
print(chi_square_val)
# 3.841459

# test_statistic is lesser than chi_square_val, so the test_statistic is in fail to reject region. So we fail to reject h0
# there is not enough evidence to conclude that promotion decision is associated with age.
