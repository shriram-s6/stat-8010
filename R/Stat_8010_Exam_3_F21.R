# stat 8010 practise exam 3

# Question 1)

# calculating tge sensitivity of the swab test

covid_tests = matrix(c(229, 95, 84, 57), ncol=2, byrow=TRUE)
colnames(covid_tests) = c('swab', 'saliva')
rownames(covid_tests) = c('true_positives', 'false_negatives')
covid_tests = as.table(covid_tests)

# sensitivity of slab = 229 / 313
swab_sen = covid_tests[1,1] / sum(covid_tests[, 1])
print(swab_sen)
# 0.7316294

# 95 % confidence interval
n = 313
alpha = 0.05

sigma_hat = sqrt((swab_sen * (1 - swab_sen)) / n)
z_val = qnorm(alpha / 2, lower.tail=FALSE)

lower_bound = swab_sen - ((z_val) * sigma_hat)
lower_bound
# 0.6825399

upper_bound = swab_sen + ((z_val) * sigma_hat)
upper_bound
# 0.7807189

# Question 1b)

# probablity of negative saliva test results = 57 / 152

prob = covid_tests[2, 2] / sum(covid_tests[, 2])
prob
# 0.375

n = 1000
print(paste('For every', n, 'people using the saliva test', prob * n  ,'who would receive negative test results.'))
# For every 1000 people using the saliva test 375 who would receive negative test results.

# Question 1c)

n1 = 313
y1 = 229
pi_swab = y1 / n1
print(pi_swab)
# 0.7316294

n2 = 152
y2 = 95
pi_saliva = y2 / n2
print(pi_saliva)
# 0.625

# claim, pi_swab = pi_saliva
# opposite, pi_swab != pi_saliva

# Null Hypothesis, H0: pi_swab = pi_saliva
# Alternative Hypothesis, Ha: pi_swab != pi_saliva

# calculate test statistic

pi_hat = (y1 + y2) / (n1 + n2)
print(pi_hat)
# 0.6967742

test_statistic = abs(pi_swab - pi_saliva) / (sqrt((pi_hat) * (1 - pi_hat) * ((1 / n1) + (1 / n2))))
print(test_statistic)
# 2.346472

# calculate z value for alpha = 0.05
alpha = 0.05
z_val = qnorm(alpha / 2, lower.tail=FALSE)
print(z_val)
# 1.959964

# since the test_statistic is greater than z_val, the test statistic lies in the rejection region. So, we reject null hypothesis.
# We can conclude that the sensitivity of the two tests are different.

# Question 2)

trials = matrix(c(27, 30, 116, 4413, 5777, 19398), ncol=3, byrow=TRUE)
colnames(trials) = c('Trial_A', 'Trial_B', 'Trial_C')
rownames(trials) = c('cases', 'non_cases')
trials = as.table(trials)

# Question 2a)

# marginal probablity = 173 / 29761
marginal_probablity = sum(trials[1, ]) / sum(trials)
marginal_probablity
# 0.005812977

# Question 2b)

# Null Hypothesis, H0: Infection rate is independent of trial
# Alternative Hypothesis, Ha: Infection rate is dependent on the trial

trials_chi_test = chisq.test(trials, correct=FALSE)
expected_values = trials_chi_test$expected
print(expected_values)

test_statistic = 0

for (i in 1:6) {
  residual = (trials[i] - expected_values[i]) ** 2 / (expected_values[i])
  test_statistic = test_statistic + residual
}

print(test_statistic)
# 0.53395

# calculating z value
alpha = 0.05
df = (nrow(trials) - 1) * (ncol(trials) - 1)

z_val = qchisq(1 - alpha, df)
print(z_val)
# 5.991465

# since the test_statistic is lesser than z_val, the test_statistic lies in the fail to reject region. so, we fail to reject null hypothesis.
# there is not enough evidence to conclude that infection rate is dependent upon/associated with the trial.
