# XcYc + confounding ----
# a model of NO CAUSAL relationship, but significant beta


# 1000 students
# true ability: n(50, 10)
# midterm score = true ability + random1, n(0, 10)
# final score = true ability + random2, n(0, 10)

n <- 1000
true_ability <- rnorm(n, 50, 10)
noise_1 <- rnorm(n, 0, 10)
noise_2 <- rnorm(n, 0, 10)
midterm <- true_ability + noise_1
final <- true_ability + noise_2
exams <- data.frame(midterm, final)

plot(exams)
# do a reg 
lm_exam <- lm(final ~ midterm, data = exams)
summary(lm_exam)

# second reg, add confounder - true ability
exams_2 <- data.frame(midterm, final, true_ability)
lm_exam2 <- lm(final ~ true_ability + midterm, data = exams_2)
summary(lm_exam2)
# true ability remains, midterm gone


s