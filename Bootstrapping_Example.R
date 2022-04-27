#####################################################################
###              P - V A L U E   F O R   T - T E S T             ####
#####################################################################

# t-Test
t.test(ToothGrowth$len~ToothGrowth$supp)

# Bootstrapping
set.seed(1701)
R <- 1000
p <- vector(length=R)
for(i in 1:R){
  boot.data <- ToothGrowth[sample(1:nrow(ToothGrowth), size = 100, replace=T), ] 
  boot.result  <- t.test(boot.data$len~boot.data$supp)
  p[i] <- boot.result$p.value
}
summary(p)

# 95% CI of p-values
p_sd <- sd(p)
p_mean <- mean(p)
margin <- qt(0.975,df=1000-1)*p_sd/sqrt(1000)
lower <- p_mean - margin # lower boundary
lower
upper <- p_mean + margin # upper boundary
upper

# Density of p-values
plot(density(p), main="Simulated p-value", ylim=c(1,20), xlim=c(0,1))
abline(0,0,0,0.05, col="green")
abline(0,0,0,0.1, col="red")
legend("topright", inset=.01, legend=c("p = 0.05", "p = 0.10"),
       col=c("green", "red"), box.lty=0, lty=1:2, cex=1.0)

#####################################################################
####   B - C O E F F I C I E N T   F O R   R E G R E S S I O N   ####
#####################################################################

# Lineare Regression
lm(mtcars$mpg~mtcars$wt)

# Bootstrapping
set.seed(1701)
R <- 1000
b <- vector(length=R)
for(i in 1:R){
  boot.data <- mtcars[sample(1:nrow(mtcars), size = 100, replace=T), ] 
  boot.result  <- summary(lm(boot.data$mpg~boot.data$wt))
  b[i] <- boot.result$coefficients[2,1]
}
summary(b)

# 95% CI of b-coeffiecient
b_sd <- sd(b)
b_mean <- mean(b)
margin <- qt(0.975,df=1000-1)*b_sd/sqrt(1000)
lower <- b_mean - margin # lower boundary
lower
upper <- b_mean + margin # upper boundary
upper

# Density of b-coefficient
plot(density(b), main="Simulated b-coefficient", ylim=c(0.05,1.25), xlim=c(-7,-3.5))
abline(0,0,0,-5.344, col="red")
legend("topright", inset=.01, legend=c("b-coefficient (original) = -5.344"),
       col=c("red"), box.lty=0, lty=1:2, cex=1.0)

#####################################################################
#### C H I - S Q U A R E   F O R   C H I - S Q U A R E - T E S T ####
#####################################################################

# Chi-Quadrat-Test
mean(mtcars$mpg)
mtcars$binary_mpg <- ifelse(mtcars$mpg<=20,1,0) # run before bootstrapping (!)
chisq.test(mtcars$binary_mpg,mtcars$vs)

# Bootstrapping
set.seed(1701)
R <- 1000
x_square <- vector(length=R)
for(i in 1:R){
  boot.data <- mtcars[sample(1:nrow(mtcars), size = 50, replace=T), ] 
  boot.result  <- chisq.test(boot.data$binary_mpg,boot.data$vs)
  x_square[i] <- boot.result$statistic
}
summary(x_square)

# 95% CI of x_square
x_square_sd <- sd(x_square)
x_square_mean <- mean(x_square)
margin <- qt(0.975,df=1000-1)*x_square_sd/sqrt(1000)
lower <- x_square_mean - margin # lower boundary
lower
upper <- x_square_mean + margin # upper boundary
upper

# Density of x_square
plot(density(x_square), main="Simulated x-square", ylim=c(0.004,0.08), xlim=c(0,40))
abline(0,0,0,9.876, col="red")
legend("topright", inset=.01, legend=c("x_square (original) = 9.876"),
       col=c("red"), box.lty=0, lty=1:2, cex=1.0)
