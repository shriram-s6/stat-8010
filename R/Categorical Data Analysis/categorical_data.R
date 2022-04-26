# 1) n = 200, y = 78, construct 95% Confidence Interval

# n - sample size
# y - no of success

# finding population proportion 
n = 200
y = 78

pi.hat = y / n
pi.hat

# 0.39

# since we are building 95% confidence interval, alpha = 0.05
alpha = 0.05
z_value = qnorm(alpha / 2, lower.tail=FALSE)
z_value

# 1.959964

# constructing confidence interval

lower_bound = pi.hat - (z_value * (sqrt(((pi.hat) * (1 - pi.hat)) / n)))
upper_bound = pi.hat + (z_value * (sqrt(((pi.hat) * (1 - pi.hat)) / n)))

lower_bound
# 0.3224025

upper_bound
# 0.4575975

prop_test = prop.test(x=y, n=n, conf.level=0.95, correct=FALSE)

# prop_test$conf.int will give the confidence interval limits

prop_test$conf.int

lower_bound = prop_test$conf.int[1]
# 0.3250834

upper_bound = prop_test$conf.int[2]
# 0.4590625

# calculating confidence interval using binom test

binom_test = binom.test(x=78, n=200, conf.level=0.95)
lower_bound = binom_test$conf.int[1]
# 0.3219911

upper_bound = binom_test$conf.int[2]
# 0.4613212

# 2) Finding sample size for confidence interval

alpha = 0.05
marginal_error = 0.05
pi = 0.5 # plug in 50% when we don't know the population size

z_value = qnorm(alpha / 2, lower.tail=FALSE)

n = (((z_value ** 2) * pi * (1 - pi)) / marginal_error ** 2)
n
# 384.1459

# Hypothesis testing for proportion, pi

# step 1: state null and alternative hypothesis
# step 2: calculate test statistic, z_obs
# step 3: calculate z for the given alpha(level of significance)
# step 4: compare both values of z, check assumptions we made earlier, and make conclusions.

# 3) n = 100, alpha = 0.01

n = 100 # sample size
y = 60 # no of success

pi.hat = y / n

pi.hat
# 0.6

# Null Hypothesis, H0 : pi = 0.5
# Alternative Hypothesis, Ha : pi > 0.5
pi = 0.5 
test_statistic = (pi.hat - pi) / (sqrt((pi * ( 1 - pi)) / 100))
test_statistic
# 2

# calculate z value for the given alpha 
z_val = qnorm(0.01, lower.tail = FALSE)
z_val
# 2.326348

# since test_statistic < z_val, the test_statistic lies in fail to reject region. So we fail to reject H0.

# using prop.test to perform the hypothesis test

prop_test = prop.test(x=60, n=100, conf.level=0.99, p=0.5, alt='greater', correct = FALSE)
prop_test


# using binom.test to perform the hypothesis test
binom_test = binom.test(x=60, n=100, conf.level=0.99, p=0.5, alt='greater')
binom_test

# Inference for two population proportions

n1 = 500
y1 = 75
pi_hat1 = y1 / n1
pi_hat1
# 0.15

n2 = 500
y2 = 25
pi_hat2 = y2 / n2
pi_hat2
# 0.05

# 90% confidence interval, so alpha = 0.10
alpha = 0.10

z_val = qnorm(alpha / 2, lower.tail=FALSE)
z_val
# 1.644854

t_stat1 = (pi_hat1 * (1 - pi_hat1)) / n1
t_stat2 = (pi_hat2 * (1 - pi_hat2)) / n2

t_stat = sqrt(t_stat1 + t_stat2)
t_stat
# 0.01870829

# confidence interval
lower_bound = (pi_hat1 - pi_hat2) - (z_val * t_stat)
lower_bound
# 0.06922761

upper_bound = (pi_hat1 - pi_hat2) + (z_val * t_stat)
upper_bound
# 0.1307724

# Hypothesis testing for two population proportions
n1 = 100
y1 = 50
pi_hat1 = y1 / n1
pi_hat1
# 0.5

n2 = 100
y2 = 25
pi_hat2 = y2 / n2
pi_hat2
# 0.25

# Null Hypothesis, H0: pi1 - pi2 = 0 or pi1 = pi2
# Alternative Hypothesis, Ha : pi1 - pi2 != 0, or pi1 != pi2

# pi_not = 0
pi_hat = (y1 + y2) / (n1 + n2)
pi_hat
# 0.375

test_statistic = (pi_hat1 - pi_hat2) / sqrt((pi_hat * (1 - pi_hat)) * ((1 / n1) + (1 / n2)))
test_statistic
# 3.651484

# calculate z value for alpha = 0.05, since it's a two sided test we should calculate z value for alpha / 2
alpha = 0.05
z = qnorm(alpha / 2, lower.tail=FALSE)
z
# 1.959964

# since test_statistic > z, the test_statistic lies in the rejection region. so we reject null hypothesis.

# Tests for Independence and Homogeneity

persons = matrix(c(16, 21, 11, 24, 17, 13), ncol=3, byrow=TRUE)
colnames(persons) = c('dem', 'rep', 'ind')
rownames(persons) = c('favor', 'nofavor')

# adding Total column to the rows and columns

persons = addmargins(persons)
persons = as.table(persons)

persons_tab = chisq.test(persons)

# we can use $expected to extract expected values
persons_exp_values = persons_tab$expected
persons_exp_values

