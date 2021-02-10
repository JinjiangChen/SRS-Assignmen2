set.seed(1)
n = 100
x = rnorm(n, 5, 1)
hist(x, freq = FALSE, main = "KDE vs f_1(x) vs histogram", xlab = 'x', ylab = 'Density')
d = seq(from = min(x), to = max(x), by = 0.1)
lines(x = d, y = dnorm(d, 5, 1))
lines(density(x), lty = 2, col = 2)
legend('topright',legend = c("PDF",'KDE'), lty = c(1,2), col = c(1,2))

x = rexp(n, 10)
hist(x, freq = FALSE, main = "KDE vs f_2(x) vs histogram", xlab = 'x', ylab = 'Density')
d = seq(from = min(x), to = max(x), by = 0.1)
lines(x = d, y = dexp(d, 10))
lines(density(x), lty = 2, col = 2)
legend('topright',legend = c("PDF",'KDE'), lty = c(1,2), col = c(1,2))

par(mfrow = c(2, 2))
R = 1000
n = 100
plot(x = d, y = dnorm(d, 5, 1), type = 'l', ylim = c(0, 0.6), main = "n = 100", xlab = 'x', ylab = 'Density')
for(r in 1:R){
  x = rnorm(n, 5, 1)
  lines(density(x), col = 'skyblue')
}
lines(x = d, y = dnorm(d, 5, 1), type = 'l', col = 'red')

n = 250
plot(x = d, y = dnorm(d, 5, 1), type = 'l',  ylim = c(0, 0.6), main = "n = 250", xlab = 'x', ylab = 'Density')
for(r in 1:R){
  x = rnorm(n, 5, 1)
  lines(density(x), col = 'skyblue')
}
lines(x = d, y = dnorm(d, 5, 1), type = 'l', col = 'red')

n = 500
plot(x = d, y = dnorm(d, 5, 1), type = 'l',  ylim = c(0, 0.6), main = "n = 500", xlab = 'x', ylab = 'Density')
for(r in 1:R){
  x = rnorm(n, 5, 1)
  lines(density(x), col = 'skyblue')
}
lines(x = d, y = dnorm(d, 5, 1), type = 'l', col = 'red')

n = 1000
plot(x = d, y = dnorm(d, 5, 1), type = 'l', ylim = c(0, 0.6), main = "n = 1000", xlab = 'x', ylab = 'Density')
for(r in 1:R){
  x = rnorm(n, 5, 1)
  lines(density(x), col = 'skyblue')
}
lines(x = d, y = dnorm(d, 5, 1), type = 'l', col = 'red')

n = 100
ise100 = numeric(R)
for(r in 1:R){
  x = rnorm(n,5,1)
  a = density(x)
  ise100[r] = sfsmisc::integrate.xy(x = a$x, (a$y - dnorm(a$x, 5,1))^2) 
}
m1 = mean(ise100)

n = 250
ise250 = numeric(R)
for(r in 1:R){
  x = rnorm(n,5,1)
  a = density(x)
  ise250[r] = sfsmisc::integrate.xy(x = a$x, (a$y - dnorm(a$x, 5,1))^2) 
}
m2 = mean(ise250)

n = 500
ise500 = numeric(R)
for(r in 1:R){
  x = rnorm(n,5,1)
  a = density(x)
  ise500[r] = sfsmisc::integrate.xy(x = a$x, (a$y - dnorm(a$x, 5,1))^2) 
}
m3 = mean(ise500)

n = 1000
ise1000 = numeric(R)
for(r in 1:R){
  x = rnorm(n,5,1)
  a = density(x)
  ise1000[r] = sfsmisc::integrate.xy(x = a$x, (a$y - dnorm(a$x, 5,1))^2) 
}
m4 = mean(ise1000)

table = data.frame(sample_size = c('100', '250', '500', '1000'),
                   ISE = c(m1, m2, m3, m4))

plot(ise100, type = 'l', col = 'grey', ylab = 'ISE')
lines(ise250, type = 'l', col = 'green')
lines(ise500, type = 'l', col = 'blue')
lines(ise1000, type = 'l', col = 'red')
legend(800,0.04,legend = c("n=100",'n=250',"n=500",'n=1000'), lty = 1, col = c('grey', 'green','blue','red'))
