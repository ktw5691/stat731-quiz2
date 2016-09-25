# This session deals with special discrete distributions but also provides some
# insights into common probabilistic inequalities and their use in statistical
# theory.
# The first part of this session deals with simulating random variables. In a
# sense, it means experimenting with a computer. We herein simulate most of the
# commonly encountered discrete distributions

# Plot discrete mass functions

r = 5L
p = 0.5
x = r:(r + 10L)
px = dnbinom(x, size = r, prob = p)
plot(x, px, type = 'h', xlim = c(r - 1L, r + 11L), ylim = c(0.0, 1.01 * max(px)),
     lwd = 2L, col = "blue", ylab = "p",
     main = paste0('X ~ NegBinomial(', r, ',', p, ')'))
points(x, px, pch = 9L, cex = 2L, col = "dark red")

# Plot discrete mass functions

m = 5L
p = 0.05
x = 0L:m
px = dnbinom(x, size = m, prob = p)
plot(x, px, type = 'h', xlim = c(-1L, m), ylim = c(0.0, 1.01 * max(px)),
     lwd = 2L, col = "blue", ylab = "p",
     main = paste0('X ~ Binomial(', m, ',', p, ')'))
points(x, px, pch = 9L, cex = 2L, col = "dark red")

# Plot discrete mass functions

m = 20L
lambda = 1.0
x = 0L:m
px = dpois(x, lambda)
plot(x, px, type = 'h', xlim = c(-1L, m), ylim = c(0.0, 1.01 * max(px)),
     lwd = 2L, col = "blue", ylab = "p", main = paste0('X ~ Pois(', lambda, ')'))
points(x, px, pch = 9L, cex = 2L, col = "dark red")

# Plot discrete mass functions

s = 15L
f = 10L
m = 5L
x = 0L:m
px = dhyper(x, m = s, n = f, k = m)

plot(x, px, type = 'h', xlim = c(-1L, m), ylim = c(0.0, 1.01 * max(px)),
     lwd = 2L, col = "blue", ylab = "p",
     main = paste0('X ~ Hypergeo(', s, ',', f, ',', m, ')'))
points(x, px, pch = 9L, cex = 2L, col = "dark red")


# The geometric distribution
# Note that in R the support of the geometric distribution starts at 0
# X ~ Geom(p) where X = Number of failures until success

n = 1000L
p = 0.4
x = rgeom(n, prob = p)

ex = (1.0 - p) / p
xbar = mean(x)

cat('\nX ~ Geo(', p, '): The Theoretical Mean is E[X] =', ex,
    'The Empirical Mean is:', xbar, '\n')

ux = unique(x)
px = prop.table(table(x))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim = c(-1.0, 1.0 + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ Geo(', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

# The Bernoulli distribution
# Despite its apparent simplicity, this is one of the most important
# distributions
# X ~ Ber(p) where Pr[X = 1] = p

n = 1000L
p = .25
x = rbinom(n, size = 1L, prob = p)

ex = p
xbar = mean(x)

cat('\nX ~ Ber(', p, '): The Theoretical Mean is E[X] =', ex,
    'The Empirical Mean is:', xbar, '\n')

ux = unique(x)
px = prop.table(table(x))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim = c(-1.0, 1.0 + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ Ber(', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

# Binomial as a sum of Bernoullis

n = 1000L
m = 5L
p = 0.25
x = apply(matrix(rbinom(m * n, 1L, p), ncol = n), 2, sum)

ex = m * p
xbar = mean(x)
cat('\nX ~ Binomial(', m, ',', p, '): The Theoretical Mean is E[X] =', ex,
    'The Empirical Mean is:', xbar, '\n')

ux = unique(x)
px = prop.table(table(x))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim = c(-1.0, 1.0 + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ Binomial(', m, ',', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

# The Negative Binomial distribution
# This distribution is verily a child of the geometric distribution for. Indeed,
# a realized negative binomial variate is a sum of geometric variates

n = 1000L
p = 0.2
r = 4L
x = rnbinom(n, size = r, prob = p)
x2 = rgeom(n, prob = p)

ex = r * (1.0 - p) / p
xbar = mean(x)

cat('\nX ~ NegBin(', r, ',', p, '): The Theoretical Mean is E[X] =', ex,
    'The Empirical Mean is:', xbar, '\n')

ux = unique(x)
px = prop.table(table(x))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim = c(-1L, 1L + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ NegBin(', r, ',', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

ux = unique(x2)
px = prop.table(table(x2))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim = c(-1L, 1L + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ Geom(', r, ',', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

# The Binomial distribution
# This distribution is verily a child of the Bernoulli distribution for indeed,
# a realized binomial variate is a sum of Bernoulli variates

n = 1000L
p = 0.2
m = 5
x = rbinom(n, size = m, prob = p)

ex = m * p
xbar = mean(x)

cat('\nX ~ Bin(', m, ',', p, '): The Theoretical Mean is E[X]= ', ex,
    'The Empirical Mean is:', xbar, '\n')

ux = unique(x)
px = prop.table(table(x))
plot(ux, px, xlab = 'x', type = 'h', ylab = 'Pr[X = x]',
     xlim=c(-1L, 1L + max(x)), ylim = c(0.0, 1.02 * max(px)), lwd = 2L,
     col = 'blue', main = paste0('X ~ Bin(', m, ',', p, ')'))
points(ux, px, pch = 9L, cex = 1L, col = 'red')

# Generic discrete distribution
# x  in {x1, x2, x3,..., xk}
# px in {p(x1), p(x2), p(x3),..., p(xk)}

# Simulate from the distribution of X defined below

rx = c(-2L, -1L, 0L, 3L, 4L)
px = c(0.125, 0.25, 0.25, 0.25, 0.125)

# Theoretical Average of X
ex = sum(rx * px)

# Second moment of X
ex2 = sum(rx * rx * px)

# Variance of X
vx = ex2 - ex * ex

# Plot the pmf
plot(rx, px, type = 'h', ylim = c(0.0, 1.01 * max(px)), lwd = 2L, col = "blue",
     ylab = "p", main = 'Generic Discrete Distribution')
points(rx, px, pch = 9L, cex = 2L, col = "dark red")

# Simulate n variates from the distribution of X defined below

n = 100000L
d = sample(rx, n, replace = TRUE, prob = px)

# Empirical (sample) average
xbar = mean(d)
varx = var(d)

# Activity for fun: Simulate the discrete uniform in {1,2,...,m}
m = 5L
uni = sample(1L:m, n, replace = TRUE)
mean(uni)

e_uni = (m + 1L) / 2L
uni_bar = mean(uni)

# Exploring the Chebyshev inequality for various distributions

# Poisson distribution
n = 10000L
lambda = 1.0
nu = 2.0
x = rpois(n, lambda)
proba = length(which(abs(x - lambda) < nu * sqrt(lambda))) / n
cat('\nChebyshev holds true here as', proba, 'is >', 1.0 - (1.0 / (nu * nu)),
    '\n')

#  Geometric distribution

n = 10000L
p = 0.25
nu = 2.0
ex = (1.0 - p) / p
vx = (1.0 - p) / (p * p)
x = rgeom(n, p)
proba = length(which(abs(x - ex) < nu * sqrt(vx))) / n
cat('\nChebyshev holds true here as', proba, 'is >', 1.0 - (1.0 / (nu * nu)),
    '\n')

#  Normal distribution
n = 10000L
ex = mu = 9.0
sigma = 2.0
vx = sigma * sigma
nu = 1.96
x = rnorm(n, mean = mu, sd = sigma)
proba = length(which(abs(x - ex) < nu * sqrt(vx))) / n

cat('\nChebyshev holds true here as', proba, 'is >', 1.0 - (1.0 / (nu * nu)),
    '\n')

# Computational demonstration of the LLN (Law of Large Numbers
# This first demonstration uses the normal distribution. However the LLN applies
# to all possible distributions

m = 100L
epsilon = 0.01
#vn = c(100L, 500L, 1600L, 3200L, 6400L, 9600L, 18000L, 25600L, 54000L, 108000L, 256000L, 819200L, 1000000L)
vn = c(100L, 500L, 1600L, 3200L, 6400L, 9600L, 18000L, 25600L, 54000L, 108000L,
       256000L, 819200L)
nn = length(vn)
pgood = numeric(nn)
mu = 9.0
mu = lambda = 3.0

for (j in 1L:nn) {
  n = vn[j]
  xx = matrix(rpois(n * m, mu), ncol = n)
  #xx = matrix(rnorm(n * m, mean = 9.0, sd = 2.0), ncol = n)
  xbar = apply(xx, 1L, mean)
  good = which(abs(xbar - mu) < epsilon)
  pgood[j] = length(good) / m
}

plot(vn, pgood, type = 'b', xlab = 'n', ylab = 'Prob[|Xbar - mu| < epsilon]')

for (j in 1L:nn) {
  n = vn[j]
  xx = matrix(rpois(n * m, mu), ncol = n)
  xbar = apply(xx, 1L, mean)
}

plot(1L:100L, xbar, type = 'b', xlab = 'n', ylab = 'Xbar')

# Activity for fun: Demonstrate the LLN for the above generic discrete distribution

# Approximation of the Binomial
m = 1500L
p = 0.001

binomial = dbinom(0L:m, size = m, prob = p)
poisson = dpois(0L:m, m * p)
error = binomial - poisson
approx1 = data.frame(round(cbind(binomial, poisson, error), 7L))
pname = paste0('X ~ Poisson(', m * p, ')')
bname = paste0('X ~ Binomial(', m, ',', p, ')')
colnames(approx1) = c(bname, pname, 'Error')
rownames(approx1) = paste0('Pr[X = ', 0L:m, ']')
print(head(approx1))
cat('\nSupremum error in the approximation is', max(abs(error)), '\n')

# Neither discrete nor continuous

n = 100000L
x = numeric(n)
alpha = 0.6
lambda = 3.0
mu = 0.0
sigma = 1.0

for(i in 1L:n) {
  ind = rbinom(1L, 1L, prob = alpha)
  x1 = rpois(1L, lambda)
  x2 = rnorm(1L, mean = mu, sd = sigma)
  x[i] = ifelse(ind, x1, x2)
}

hist(x)
mean(x)

# Uncorrelated but not independent

n = 100L
x = runif(n, -1.0, 1.0)
y = abs(x)

cat('\nThe correlation of X and Y is', cor(x, y), '\n')
print(cor.test(x, y))
plot(x, y)
