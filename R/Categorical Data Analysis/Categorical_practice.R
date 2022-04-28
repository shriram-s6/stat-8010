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


# some random questions.

# A random sample of 1500 is drawn from a binomial population. if there are 1200 successes, construct 95% and 90% confidence interval.

n = 1500
y = 1200

pi_hat = y / n
print(pi_hat)
# 0.8

# 95 % confidence interval

alpha = 0.05
z_value = qnorm(alpha / 2, lower.tail = FALSE)
print(z_value)
# 1.959964

sigma_hat = sqrt(((pi_hat) * (1 - pi_hat)) / n)

# lower bound value of CI
lower_bound_ci = pi_hat - (z_value * sigma_hat)
print(lower_bound_ci)
# 0.7797576

# upper bound value of CI
upper_bound_ci = pi_hat + (z_value * sigma_hat)
print(upper_bound_ci)
# 0.8202424

# 90 % confidence interval

alpha = 0.10
z_value = qnorm(alpha / 2, lower.tail = FALSE)
print(z_value)
# 1.644854

sigma_hat = sqrt(((pi_hat) * (1 - pi_hat)) / n)

# lower bound value of CI
lower_bound_ci = pi_hat - (z_value * sigma_hat)
print(lower_bound_ci)
# 0.783012

# upper bound value of CI
upper_bound_ci = pi_hat + (z_value * sigma_hat)
print(upper_bound_ci)
# 0.816988


# Inference about two population proportions

n1 = 200
y1 = 109

pi_hat1 = y1 / n1
print(pi_hat1)
# 0.545

n2 = 200
y2 = 86

pi_hat2 = y2 / n2
print(pi_hat2)
# 0.43

# Null hypothesis, H0: pi_hat1 - pi_hat2 <= 0
# Alternative hypothesis, Ha: pi_hat1 - pi_hat2 > 0

# using prop.test

two_prop_test = prop.test(x=c(109, 86), n=c(200, 200), alternative='greater', conf.level=0.95, correct=FALSE)
print(two_prop_test)
# p-value = 0.0107 is less than 0.05, so we can reject null hypothesis.

# Doing traditional way of testing null hypothesis by calculating test statistic.

sigma_hat = sqrt(((pi_hat1 * (1 - pi_hat1)) / n1) + ((pi_hat2 * (1 - pi_hat2)) / n2))

test_statistic = (pi_hat1 - pi_hat2) / sigma_hat
print(test_statistic)
# 2.316095

# calculating critical value for z
z_val = qnorm(0.05, lower.tail=FALSE)
print(z_val)
# 1.644854

# since test_statistic > z_val, we reject null hypothesis.


# Inference about two population proportions

n1 = 310
pi_hat1 = 0.32

n2 = 309
pi_hat2 = 0.20

# Null hypothesis, H0: pi1 - pi2 = 0 or pi1 = pi2
# Alternative hypothesis, Ha: pi1 - pi2 != 0 or pi1 != pi2

# calculating test statistic

# since the null hypothesis is pi1 = pi2, we should find common proportion

pi_prop = (((pi_hat1 * n1)) + ((pi_hat2 * n2))) / (n1 + n2)
print(pi_prop)

test_statistic = abs(pi_hat1 - pi_hat2) / sqrt((pi_prop * (1 - pi_prop) * ((1 / n1) + (1 / n2))))
print(test_statistic)
# 3.402836

# calculating critical value z
z_val = qnorm(0.05 / 2, lower.tail=FALSE)
print(z_val)
# 1.959964

# since test_statistic > z_val, we will reject null hypothesis.

# test for independence

age_groups = matrix(c(9, 29, 32, 10, 41, 41, 48, 40), ncol=4, byrow=TRUE)
colnames(age_groups) = c('under_30', 'thirty_to_39', 'forty_to_49', 'fifty_and_over')
rownames(age_groups) = c('promoted', 'not_promoted')
age_groups = as.table(age_groups)
print(age_groups)

age_groups_test = chisq.test(age_groups, correct=FALSE)
print(age_groups_test)
# X-squared = 13.025, df = 3, p-value = 0.004582

expected_values = age_groups_test$expected
print(expected_values)

test_statistic = c()

for (i in 1:nrow(age_groups)) {
  
  for (j in 1:ncol(age_groups)) {
    
    residual = (age_groups[i, j] - expected_values[i, j]) ** 2 / (expected_values[i, j])
    
    test_statistic = append(test_statistic, residual)
    
  }
}
print(test_statistic)

test_statistic_val = sum(test_statistic)
print(test_statistic_val)
# 13.02521

# degrees of freedom

df = (nrow(age_groups) - 1) * (ncol(age_groups) - 1)
print(df)
# 3

# calculate critical value for chi-square

chi_square_val = qchisq(1 - 0.05, df)
print(chi_square_val)
# 7.814728

# since test_statistic_val > chi_square_val, we reject null hypothesis.


