# *********************************************************************** #
#                ** Statistical Methods for Data Science I **             #
#          ** Lab 10 - Bootstrapping the Correlation / In class **        #
# *********************************************************************** #

#' ---
#' title:  "Statistical Methods for Data Science I"
#' author: "Pierpaolo Brutti"
#' date:   "Lab 10 - Bootstrapping the Correlation / In class"
#' ---

# Data --------------------------------------------------------------------

load("pippo.RData")
dat <- subset(pippoA, select = c("height", "pupils"))

# Filter out missing data
idx <- complete.cases(dat)
dat <- dat[idx, ]

# Correlation -------------------------------------------------------------

# Correlation: point estimate(s) 
rho_hat <- cor(dat$height, dat$pupils)
rho_hat
cor(dat) # correlation matrix

# Bootstrap / Handmade ----------------------------------------------------

# Init
n = nrow(dat)
B = 1000
brep = rep(NA, B)

# Loop
set.seed(1213)  # for reproducibility
for (b in 1:B){
  idx = sample(1:n, replace = T)
  bsamp   = dat[idx, ]        # bootstrap sample
  btheta  = cor(bsamp)[1,2]   # bootstrap replicate
  brep[b] = btheta            # save
}

# Bootstrap ECDF >> G*(t)
Gstar = ecdf(brep)
plot(Gstar, main = "")
hist(brep, breaks = 25, col = "orchid", border = "white",
     xlab = expression(hat(rho)[boot]),
     main = "Bootstrap approximation \n to the sampling distribution")

# Bootstrapped bias
bias <- mean(brep) - rho_hat
bias

# Bootstrapped standard error
se_boot <- sqrt(var(brep))
se_boot

# Boostrapped MSE of the plug-in estimators
mse_boot <- se_boot^2 + bias^2
mse_boot

# Confidence Level
alpha = 0.05

# Bootstrap / Normal CI / 95%
z <- qnorm(1 - alpha/2) # morally: z = 2
c(rho_hat - z*se_boot, rho_hat + z*se_boot)
# (-0.46, -0.05)

# Bootstrap / Percentile CI / 95%
q_boot <- quantile(brep, c(alpha/2, 1 - alpha/2))
q_boot
# (-0.44, -0.04)

# Bootstrap / Pivotal CI / 95%
c(2*rho_hat - q_boot[2], 2*rho_hat - q_boot[1])
# (-0.47, -0.08)

# Bootstrap / <boot> ------------------------------------------------------

# Redo everything via the package <boot>
require(boot, quietly = T)
cor_boot <- function(x, ind) cor( x[ind, 1], x[ind, 2] )
set.seed(123)
res      <- boot(data = dat, statistic = cor_boot, R = 1000)
res

# Get confidence intervals
?boot.ci
boot.ci(res)

?# Asymptotic CI -----------------------------------------------------------

# The function <cor.test> uses Fisher's Z transform to build a CI
# (see the "Details" section in the help file)
?cor.test
cor.test(dat$height, dat$pupils, conf.level = 1 - alpha)$conf.int
# (-0.49, 0.003)

# Recentering

# Fisher Z-transform
# We know that h(hat.rho) is N( h(rho), 1/(n-3))
# So h(hat.rho) - h(rho) ~ N( 0, 1/(n-3) )
h = function(x) 0.5*log((1 + x)/(1 - x))
curve(h(x), -1, 1)

zrep = h(brep) - h(cor(dat)[1,2])
hist(zrep, prob = T, main = "", 
     xlab = expression(h(hat(theta)[boot]) - h(hat(theta)[n])), 
     col = "orchid", border = "white")
curve( dnorm(x, 0, sqrt(1/(n-3)) ), add = TRUE,
       col = "purple", lwd = 4
)
# We compare the asymptotic limit with "recentered" bootstrap
# replicates: good agreement.

## FINAL COMMENT ##
# There's a mild negative correlation between your height and inter-pupils
# distance. This correlation appears to be significantly different from
# zero at level 0.05 although the asymptotic-normal CI is slightly larger
# and contains 0. All the intervals are essentially in agreement.


# From Application to Theory ----------------------------------------------

# The previous quick data-analysis can be relevant to a specific application
# but it cannot be used to compare the *frequentist* theoretical properties
# of statistical procedures like those we used to build our four CI's.
# To go in this direction, we can always resort to a simulation study.
#
# Here, we will set up a Monte Carlo simulation to compare actual coverage and
# expected lenght of the intervals produced by our four procedures, similarly
# to what we did already. See
# Lab 08 / InClass >> url: https://goo.gl/7F66VJ

# 0 # Choose a sample size <n>
# 1 # Sample IID (M) from a specific joint distro
#     having a given corr-coeff <rho>
# 2 # For each of these MC samples evaluate the
#     four procedures & their coverage/lenght

# Step 0 #
# First of all we should pick a bivariate distribution for our population
# that we can sample from, and allows to specify the dependency level in
# terms of the linear correlation coefficient.
# Up to this point, the only option we have is to use a bivariare Normal
# but we are unable to model anything but variables that take value on
# the whole real line. 
# To inject a bit more flexibility we gonna use a *copula model*, which 
# allows use to model marginal and common dependencies separately.
# See the slides for more information.

# Package: https://cran.r-project.org/web/packages/copula/index.html
require(copula)
library(help = copula)

## Simulation setup

# Joint model + marginals: (Claim, Waiting Time) ~ (Exp(rate), Gamma(shape, scale))
# Marginal parameters
set_par <- c("rate" = 0.1, "shape" = 0.9, "scale" = 30)
# Joint model via Gaussian copula
rho_true = 0.7   # true correlation coefficient
cong = mvdc(normalCopula(rho_true), margins = c("exp", "gamma"),
            paramMargins = list(list(rate = set_par["rate"]), 
                                list(shape = set_par["shape"], 
                                     scale = set_par["scale"]) ) )
# Take a look
par.ini <- par()   # save graphical parameters
laymat <- matrix(c(1,2,3,4), ncol = 2, byrow = T)
layout(laymat)
par(mar=c(5, 5, 0.4, 0.4), oma = c(4, 4, 0.4, 0.4))
# 1 #
curve(dexp(x, rate = set_par[1]), 0, 10, 
      lwd = 4, col = "orchid", 
      xlab = expression(x[1]), ylab = expression(F[1]))
# 2 #
curve(dgamma(x, shape = set_par[2], scale = set_par[3]), 0, 10, 
      lwd = 4, col = "purple", 
      xlab = expression(x[2]), ylab = expression(F[2]))
# 3 #
persp(cong, dMvdc,
      xlim = c(0, 10), ylim = c(0, 10),
      theta = 135, phi = 30,
      ltheta = -120, shade = 0.75,
      border = NA, box = FALSE)
# 4 #
contour(cong, dMvdc, 
        xlim = c(0, 10), ylim = c(0, 10) )

n.seq  <- c(10, 50, 100, 200, 500)    # sample-size   --> in the model   
M      <- 1000                        # sim-size      --> not in the model, the large the better!
B      <- 1000                        # Bootstrap replicates
alpha  <- 0.05                        # *Nominal*, not necessarily the *actual*, confidence level

# Output init: array (M x 8 x length(n.seq))
out <- array(NA, dim = c(M, 8, length(n.seq)), 
             dimnames = list(1:M, c("cov_BNor", "cov_BPer", "cov_BPiv", "cov_Asy",
                                    "len_BNor", "len_BPer", "len_BPiv", "len_Asy"),
                             paste("n =", n.seq) ) )

# Loopy-solution / Quite slow, reduce M and/or B.
set.seed(1234)  # for reproducibility
for (i in 1:length(n.seq)){
  n <- n.seq[i]
  for (m in 1:M){
    ## 1a ## Generate the data --> this is a sim, we are in control!
    xx   <- rMvdc(n, cong)
    ## 1b ## Evaluate the plug-estimate for the linear correlation
    rho_hat <- cor(xx)[1,2]

    ## 2 ## Nonparametric Boostrap
    brep = rep(NA, B)
    for (b in 1:B) brep[b] <- cor(xx[sample(1:n, replace = T), ])[1,2]

    ## 3 ## Confidence Intervals
    # Bootstrap / Normal CI
    se_boot <- sd(brep)
    z <- qnorm(1 - alpha/2) # morally: z = 2
    bnor <- c(rho_hat - z*se_boot, rho_hat + z*se_boot)
    # Bootstrap / Percentile CI
    bper <- quantile(brep, c(alpha/2, 1 - alpha/2))
    # Bootstrap / Pivotal CI
    bpiv <- c(2*rho_hat - bper[2], 2*rho_hat - bper[1])
    # Asymptotic
    asy <- cor.test(xx[,1], xx[,2], conf.level = 1 - alpha)$conf.int
    
    ## 4 ## Check for coverage of the true parameter <rho_true>
    cnor <- (rho_true >= bnor[1]) & (rho_true <= bnor[2])
    cper <- (rho_true >= bper[1]) & (rho_true <= bper[2])
    cpiv <- (rho_true >= bpiv[1]) & (rho_true <= bpiv[2])
    casy <- (rho_true >= asy[1]) & (rho_true <= asy[2])
    # Save
    out[m, , i] <- c( cnor, cper, cpiv, casy, 
                      diff(bnor), diff(bper), diff(bpiv), diff(asy) )
  }
}

# Monte Carlo evaluation/approximation of the risk
# Remember that <out> is a 3D array...
?apply
res   <- apply( out, 2:3, mean)     # MC-estimates

# Take a look
round(res, 2)

# Plot with error-bar
require(viridis)
colo = viridis(4)
suppressWarnings( par(par.ini) ) # reset graphical parameters
laymat <- matrix(c(1,2,3,3), ncol = 2, byrow = T)
layout(laymat)
par(mar=c(5, 5, 0.4, 0.4), oma = c(4, 4, 0.4, 0.4))
## 1 ## Observed coverage
matplot(n.seq, t(res[1:4,]), ylim = c(0.8, 1), 
        xlab = "sample size (n)", ylab = "observed coverage",
        type = "p", pch = 21, col = "black", bg = colo)
abline(h = 1-alpha, lwd = 4, lty = 3, col = rgb(1,0,0,.25))
legend("topright", 
       c("Boot-Normal", "Boot-Percentile", "Boot-Pivotal", "Asymptotic"), 
       pch = 19, col = colo, bty = "n", cex = .8)
grid()
## 2 ## Expected Lenght
matplot(n.seq, t(res[5:8,]),
        xlab = "sample size (n)", ylab = "expected lenght",
        type = "p", pch = 21, col = "black", bg = colo)
legend("topright", 
       c("Boot-Normal", "Boot-Percentile", "Boot-Pivotal", "Asymptotic"), 
       pch = 19, col = colo, bty = "n", cex = .8)
grid()
## 3 ## risk
k = 1
matplot(n.seq, t(res[5:8,] + k*abs(res[1:4,] - (1 - alpha))),
        xlab = "sample size (n)", ylab = "risk",
        type = "p", pch = 21, col = "black", bg = colo)
legend("topright", 
       c("Boot-Normal", "Boot-Percentile", "Boot-Pivotal", "Asymptotic"), 
       pch = 19, col = colo, bty = "n", cex = .8)
grid()