# generate some data without thinking too much about the layers

# types of outcomes: binary, continuous, survival (not now)

# direct sim ----
# (ROS book 5.1) 


# n girls from a population of 400
n_girls <- rbinom(1, size = 400, prob = 0.488)

n_sims <- 1000
n_girls <- rep(NA, n_sims)
for(i in 1:n_sims){
  n_girls[i] <- rbinom(1, 400, 0.488)
}
hist(n_girls)

# or, 
n_girls <- rbinom(n_sims, 500, 0.488)




## mixed discrete/cont ----  
# height of one randomly chosen adult
# adult: 52% women
# M: n(69,1,  2.9)
# F: n(63.7, 2.7)

male <- rbinom(1, 1, 0.48)
height <- ifelse(male == 1, 
                 rnorm(1, 69.1, 2.9), 
                 rnorm(1, 63.7, 2.7))

height




# ___________ ----


# XcYc (reg) ----
# y = b0 + b1x1 + e

x <- 1:20
n <- length(x)
b0 <- 0.2
b1 <- 0.3
sigma <- 0.5
y <- b0 + b1*x + sigma*rnorm(n) 
plot(x,y)



# ___________ ----

# XdYc - normal ----

# group label random only for large samples
# x <- rbinom(200, size = 1, prob = 0.5)
# small sample: fixed size
x <- c(rep(0, 10), rep(1, 10))
y <- rep(NA, length(x))
for(i in 1:length(y)){
  y[i] <- ifelse(x[i] == 0, rnorm(1, mean = 5, sd = 1), 
                 rnorm(1, mean = 10, sd =1))
}
plot(x,y)

summary(lm(y ~ x))


# more X levels ----
# compare the coefficient
x <- factor(c(rep('A', 100), rep('B', 100), rep('C', 100)))
y <- rep(NA, length(x))
for(i in 1:length(y)){
  if(x[i] == 'A'){
    y[i] <- rnorm(1, mean = 5, sd = 1)
  }else if(x[i] == 'B'){
    y[i] <- rnorm(1, mean = 10, sd = 1)
  }else{
    y[i] <- rnorm(1, mean = 50, sd = 1)
  }
}
df <- data.frame(x, y)
df
plot(df)
lmm <- lm(y~x, data = df)
# predict(lmm)
summary(lmm)
# b0 = 4.81
# b1 = 5.36
# b2 = 45.17

# for ONE var:
# beta0: mean(x==A) (reference group)
# beta1: mean(x==B) - beta0
# beta2: mean(x==c) - beta0

mean(y[x == 'B']) - mean(y[x == 'A'])
mean(y[x == 'C']) - mean(y[x == 'A'])
# diff between B and C
mean(y[x == 'C']) - mean(y[x == 'B']) # 39.8



# what about making B the baseline?
df2 <- df
df2$x_bbase <- relevel(df2$x, 'B')
df2$x_bbase # B A C
# lm
lmm2 <- lm(y ~ x_bbase, data = df2)
summary(lmm2)

# beta2: 39.807. it is the difference between B and C

# (multcomp) ----
# https://stats.stackexchange.com/questions/436618/how-to-test-between-level-significance-in-logistic-regression
# use multcomp to systematically compare
library(multcomp)
?multcomp::glht # general linear hypotheses
# set up contrast

amod <- lm(breaks ~ tension, data = warpbreaks)
summary(amod)
# levels: L,M,H
# glht(amod, linfct = mcp(tension = c("M - L = 0", 
#                                     "H - L = 0",
#                                     "H - M = 0")))

contr <- rbind("M - L" = c(-1, 1, 0),
               "H - L" = c(-1, 0, 1), 
               "H - M" = c(0, -1, 1))
comp <- glht(amod, linfct = mcp(tension = contr))
summary(comp)
# this indicates not significant diff bet. H and M

plot(warpbreaks$tension, warpbreaks$breaks)


# now ours revisited
contr <- rbind("B - A" = c(-1, 1, 0), # b1
               "C - A" = c(-1, 0, 1), # b2
               "C - B" = c(0, -1, 1)) # b2-b1
comp <- glht(lmm, linfct = mcp(x = contr))
summary(comp)

b0 = 5.07
b1 = 4.84
b2 = 44.96

# __________ ----
# XcYd - logit ----
# qlogis(p) = log(p/1-p)

qlogis(0.5)
probs <- seq(0.01, 0.99, by = 0.01)
plot(probs, qlogis(probs)) 

# inverse: plogis
# easier to plot the 




# try multicomp
# x <- c(rnorm(50, -2, 1), rnorm(50, 2, 1))
x <- factor(c(rep('white', 25), 
              rep('red', 45), 
              rep('black', 30)))
# make it less obvious
# x <- sample(x, replace = F)

table(x)
# y <- factor(c(rep('0', 50), rep('1', 50)))
y <- rep(NA, length(x))
y[x == 'white'] <- rbinom(length(y[x == 'white']), 1, 0.2)
y[x == 'red'] <- rbinom(length(y[x == 'red']), 1, 0.6)
y[x == 'black'] <- rbinom(length(y[x == 'black']), 1, 0.9)


df <- data.frame(x, y)
table(df)

# do glm 

lr <- glm(y ~ x, family = 'binomial')
summary(lr)

# b0 = 1.872
# b1 = -1.738 (red - black)
# b2 = -2.625 (white - black)
# remember this is beta, not OR

# do multcomp
# black is baseline
contr <- rbind("red - black" = c(-1, 1, 0), # b1, -1.738
               "white - black" = c(-1, 0, 1), # b2, -2.625
               "white - red" = c(0, -1, 1)) # -0.887
comp <- glht(lr, linfct = mcp(x = contr))
summary(comp)

b1 <- -1.7383
b2 <- -2.6256
b2-b1 # -0.887

# df <- data.frame(x, y)
# instead of black, use red as baseline
df$x2 <- relevel(df$x, 'red')
lr2 <- glm(y ~ x2, data = df, family = 'binomial')
summary(lr2)


