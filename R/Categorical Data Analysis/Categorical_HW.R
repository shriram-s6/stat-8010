# Categorical Data Homework 

# Question 1

# A sample of 200 children
n = 200 # sample size

# 83 children had anemia
y = 83  # no of children with anemia

# proportion = no of children with anemia / sample size
pi_hat = y / n
pi_hat
# 0.415

# is it reasonable to conclude that the true proportion of children living in these villages with anemia is equal to 50% ?
# Null hypothesis, pi0 = 0.50
# Alternative hypothesis, pi0 != 0.50

# calculating test statistic 
pi_not = 0.5
sigma_hat = sqrt((pi_not * (1 - pi_not)) / 200)
test_statistic = abs(pi_hat - pi_not) / sigma_hat
print(test_statistic)
# 2.404163

# calculating z for the given significance level. Since we are asked to build a 95% CI, alpha = 0.05
alpha = 0.05
z = qnorm(alpha / 2, lower.tail=F)
print(z)
# 1.959964

# since test_statistic is greater than z, the test_statistic lies in the rejection region. So we reject null hypothesis.
# There is enough evidence to conclude that the true proportion of children living in these villages with anemia is not equal to 50%

# confidence interval
lower_bound = pi_hat - (z * sigma_hat)
print(lower_bound)
# 0.3467135

upper_bound = pi_hat + (z * sigma_hat)
upper_bound
# 0.4832865

# Question 2)

# when the man wakes up early
y1 = 36
n1 = 45
pi_hat1 = y1 / n1
print(pi_hat1)
# 0.8

# when the man wakes up late
y2 = 20
n2 = 35
pi_hat2 = y2 / n2
print(pi_hat2)
# 0.5714286

# claim: pi_hat1 != pi_hat2
# opposite: pi_hat1 = pi_hat2

# Null Hypothesis, H0: pi_hat1 = pi_hat2
# Alternative Hypothesis, Ha: pi_hat1 != pi_hat2

# since the null hypothesis is pi_hat1 - pi_hat2 = 0, we should find common proportion pi_hat

pi_hat = (y1 + y2) / (n1 + n2)
print(pi_hat)
# 0.7

sigma_hat = sqrt((pi_hat) * (1 - pi_hat) * ((1 / n1) + (1 / n2)))
sigma_hat
# 0.1032796

test_statistic = abs(pi_hat1 - pi_hat2) / sigma_hat
print(test_statistic)
# 2.213133

# calculating z for alpha = 0.05. since it's a two tailed test, we should use alpha / 2 to find z.
alpha = 0.05
z = qnorm(alpha / 2, lower.tail=F)
print(z)
# 1.959964

# the test_statistic is greater than z, which means the test_statistic is in the rejection region. So we will reject null hypothesis.
# There is enough evidence to conclude that the proportions are different.

# Test whether the sleeping in is independent on the paper being missing.

newspaper = matrix(c(36, 20, 9, 15), ncol=2, byrow=TRUE)
colnames(newspaper) = c('early', 'late')
rownames(newspaper) = c('found', 'missing')
newspaper = as.table(addmargins(newspaper))
print(newspaper)

# null hypothesis, H0: missing newspaper is independent of waking up early or late
# alternative hypothesis, Ha: missing newspaper is dependent on waking early or late

# doing a chi square test
newspaper_test = chisq.test(newspaper, correct=FALSE)

# getting expected value counts

expected_values = newspaper_test$expected
print(expected_values)

test_statistic = 0

for (i in 1:4) {
  
  residual = (newspaper[i] - expected_values[i]) ** 2 / (expected_values[i])
  test_statistic = test_statistic + residual
}

print(test_statistic)
# 4.897959

# calculating degree of freedom. df = (no of rows - 1) * (no of columns - 1)

df = (nrow(newspaper) - 1) * (ncol(newspaper) - 1)
df
# 1

# calculating chi-square critical value
chi_square_cv = qchisq(1 - alpha, df)
chi_square_cv
# 3.841459

# since test_statistic is greater than chi_square_cv, test_statistic lies in the rejection region. so we reject null hypothesis.
# We can conclude that missing newspaper is associated with the time the man wakes up.

# can you determine that sleeping in late is what's causing the paper to be missing?

# The rejection of null hypothesis indicates only that the apparent association is not reasonably attributable to chance.
# It does not indicate anything about the strength or type of association. So we cannot say that sleeping late is causing the paper to be missing.

# Question 3)

exam = matrix(c(2, 22, 14, 1, 36, 18, 1, 0), ncol=4, byrow=T)
colnames(exam) = c('A', 'B', 'C', 'F')
rownames(exam) = c('taking_exam', 'not_taking_exam')
exam = as.table(exam)

exam_test = chisq.test(exam, correct=FALSE)
print(exam_test)

exam_expected_values = exam_test$expected
print(exam_expected_values)

# the no of expected observations in column F is less than 5, so the test for independence might not be reliable
