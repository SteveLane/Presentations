################################################################################
################################################################################
## Title: Figures for pilot study presentation
## Author: Steve Lane
## Date: Fri 21/03/2014
## Synopsis: Generate figures for the pilot study presentation.
################################################################################
################################################################################
rm(list = ls())
## Plot of power for a varying effect size, with fixed n
png(file = "Power1.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = 10, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col = "grey60")
dev.off()
## Add in differences in shifting the effect size (e.g. for a fixed standard
## deviation, adjust the mean difference)
## png(file = "Power1.png", width = 3600, height = 2700, res = 200)
## par(cex = 2)
## curve(power.t.test(n = 10, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
##       = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col = "grey60")
## segments(1, 0, 1, power.t.test(n = 10, delta = 1, sd = 1)$power, lwd = 2, col =
##          "grey60")
## segments(0.8, 0, 0.8, power.t.test(n = 10, delta = 0.8, sd = 1)$power, lwd = 2,
##          col = "grey60", lty = 2)
## segments(0.7, power.t.test(n = 10, delta = 0.8, sd = 1)$power, 0.7,
##          power.t.test(n = 10, delta = 1, sd = 1)$power, lwd = 2,
##          col = "grey60", lty = 2)
## segments(0.69, power.t.test(n = 10, delta = 0.8, sd = 1)$power, 0.71,
##          power.t.test(n = 10, delta = 0.8, sd = 1)$power, lwd = 2,
##          col = "grey60")
## segments(0.69, power.t.test(n = 10, delta = 1, sd = 1)$power, 0.71,
##          power.t.test(n = 10, delta = 1, sd = 1)$power, lwd = 2,
##          col = "grey60")
## ## text(
## dev.off()

## Add in larger n
png(file = "Power2.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = 10, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey")
curve(power.t.test(n = 15, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col = "grey60", add
      = TRUE)
dev.off()
png(file = "Power3.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = 10, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey")
curve(power.t.test(n = 15, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey", add
      = TRUE)
curve(power.t.test(n = 20, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col = "grey60", add
      = TRUE)
dev.off()
png(file = "Power4.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = 10, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey")
curve(power.t.test(n = 15, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey", add
      = TRUE)
curve(power.t.test(n = 20, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 2, col = "lightgrey", add
      = TRUE)
curve(power.t.test(n = 25, delta = x, sd = 1)$power, from = 0.25, to = 1.5, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col = "grey60", add
      = TRUE)
dev.off()

## Now the issue with estimating d from pilot data is the variance, and the fact
## that power is much more sensitive to d, than it is to n.
png(file = "PowerNComp.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = x/2, delta = 0.5, sd = 1)$power, from = 50, to = 200, bty
      = "l", xlab = "Total sample size (n)", ylab = "Power", lwd = 3, col =
      "grey60")
segments(102, power.t.test(51, 0.5)$power, 102, 0, lwd = 3, lty = 1, col =
         "darkseagreen")
segments(154, power.t.test(77, 0.5)$power, 154, 0, lwd = 3, lty = 1, col =
         "darkseagreen")
segments(128, power.t.test(64, 0.5)$power, 128, 0, lwd = 3, lty = 3, col =
         "darkseagreen")
text(100, power.t.test(64, 0.5)$power/2 + power.t.test(51, 0.5)$power/2,
     expression(paste("|", Delta, plain(power), "| = ", 9.6, "%")), pos = 2)
text(160, power.t.test(77, 0.5)$power/2 + power.t.test(64, 0.5)$power/2,
     expression(paste("|", Delta, plain(power), "| = ", 6.8, "%")), pos = 4)
segments(55, power.t.test(51, 0.5)$power, 102, power.t.test(51, 0.5)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
segments(55, power.t.test(64, 0.5)$power, 200, power.t.test(64, 0.5)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
segments(154, power.t.test(77, 0.5)$power, 200, power.t.test(77, 0.5)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
dev.off()
png(file = "PowerDComp.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(power.t.test(n = 64, delta = x, sd = 1)$power, from = 0.2, to = 0.8, bty
      = "l", xlab = "Effect size (d)", ylab = "Power", lwd = 3, col =
      "grey60")
segments(0.4, power.t.test(64, 0.4)$power, 0.4, 0, lwd = 3, lty = 1, col =
         "darkseagreen")
segments(0.6, power.t.test(64, 0.6)$power, 0.6, 0, lwd = 3, lty = 1, col =
         "darkseagreen")
segments(0.5, power.t.test(64, 0.5)$power, 0.5, 0, lwd = 3, lty = 3, col =
         "darkseagreen")
text(0.3, power.t.test(64, 0.5)$power/2 + power.t.test(64, 0.4)$power/2,
     expression(paste("|", Delta, plain(power), "| = ", 18.9, "%")))
text(0.7, power.t.test(64, 0.6)$power/2 + power.t.test(64, 0.5)$power/2,
     expression(paste("|", Delta, plain(power), "| = ", 11.9, "%")))
segments(0.3, power.t.test(64, 0.4)$power, 0.4, power.t.test(64, 0.4)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
segments(0.3, power.t.test(64, 0.5)$power, 0.7, power.t.test(64, 0.5)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
segments(0.6, power.t.test(64, 0.6)$power, 0.7, power.t.test(64, 0.6)$power, lwd
         = 3, lty = 4, col = "darkseagreen")
dev.off()

## What does a rational decision maker do?
png(file = "Decision1.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(dnorm(x, 0.5, sqrt(2/10)), -1.5, 2.5, xlab = "Effect size (d)", ylab = "f(d)",
      lwd = 3, col = "grey60", bty = "l")
x <- seq(-1.5, 0, length = 101)
y <- dnorm(x, 0.5, sqrt(2/10))
polygon(c(x, x[length(x)]), c(y, y[1]), col = "darkseagreen", border =
        "darkseagreen")
text(-1, 0.3, "Pr(d < 0) = 0.13")
dev.off()
png(file = "Decision2.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(dnorm(x, 0.5, sqrt(2/10)), -1.5, 2.5, xlab = "Effect size (d)", ylab = "f(d)",
      lwd = 3, col = "grey60", bty = "l")
x <- seq(-1.5, 0.1, length = 101)
y <- dnorm(x, 0.5, sqrt(2/10))
polygon(c(x, x[length(x)]), c(y, y[1]), col = "darkseagreen", border =
        "darkseagreen")
text(-1, 0.3, "Pr(d < 0.1) = 0.19")
dev.off()
png(file = "Decision3.png", width = 3600, height = 2700, res = 200)
par(cex = 2, bg = "transparent")
curve(dnorm(x, 0.5, sqrt(2/10)), -1.5, 2.5, xlab = "Effect size (d)", ylab = "f(d)",
      lwd = 3, col = "grey60", bty = "l")
x <- seq(-1.5, 0.2, length = 101)
y <- dnorm(x, 0.5, sqrt(2/10))
polygon(c(x, x[length(x)]), c(y, y[1]), col = "darkseagreen", border =
        "darkseagreen")
text(-1, 0.3, "Pr(d < 0.2) = 0.25")
dev.off()

## Now what about some simulations looking at power using the estimated effect
## size. (Average power should be the same as integrating over the
## distribution).
UncondPower <- function(z, mu.diff, sig, n1, n2){
    se <- sig*sqrt(2/n1)
    se.d <- sqrt(2/n2)
    power.t.test(n = n1, delta = mu.diff + z*se, sd = sig)$power*
        dnorm(z, sd = se.d)
}

UncondPower2 <- function(n.full, n.pilot, sigma, d, alpha){
    tau <- sqrt(2*(sigma^2)/n.full)
    v <- 2/n.pilot
    pnorm((-tau*qnorm(1 - alpha/2) + d)/sqrt(tau^2 + v)) +
        pnorm((-tau*qnorm(1 - alpha/2) - d)/sqrt(tau^2 + v))
}

NPilot <- function(d, n){
    x <- rnorm(n = n)
    y <- rnorm(n = n, mean = d)
    test <- t.test(x, y, var.equal = TRUE)$p.val < 0.05
    d.est <- (mean(y) - mean(x))/sqrt((var(x) + var(y))/2)
    N <- try(ceiling(power.t.test(delta = d.est, power = 0.8)$n))
    if(inherits(N, "try-error")){
        ## browser()
        N <- NA
    }
    pow.est <- try(power.t.test(n = N, delta = d)$power)
    if(inherits(pow.est, "try-error")){
        ## browser()
        pow.est <- NA
    }
    c(test, d.est, N, pow.est)
}
system.time(
out <- replicate(49999, NPilot(0.5, 10))
)
## Rational decision maker wouldn't use estimates below 0
inds <- which(out[2,] > 0)
rowMeans(out[, inds], na.rm = TRUE)
## And may not even use estimates below a certain cutoff
## Cutoff 0.1
inds2 <- which(out[2,] >= 0.1)
rowMeans(out[, inds2], na.rm = TRUE)
## Cutoff 0.2
inds3 <- which(out[2,] >= 0.2)
rowMeans(out[, inds3], na.rm = TRUE)

test <- function(z, np, cutoff){
    sapply(1:length(z), function(x, np, cutoff){
        power.t.test(delta = z[x], sd = 1, power = 0.8)$n*
            dnorm(z[x], 0.5, sd = sqrt(2/np))*I(z[x] >= cutoff)/
                (1 - pnorm(cutoff, 0.5, sqrt(2/np)))
    }, np = np, cutoff = cutoff)
}

## Function to calculate Type I error under a two-stage design
## n is the initial sample size (if not given, it will be calculated), p is the
## proportion in the pilot.
TwoStage <- function(d.pow, d.true, sig, n = NULL, p){
    if(is.null(n)){
        n <- ceiling(power.t.test(delta = d.pow, sd = sig, power = 0.8)$n)
    }
    n1 <- ceiling(p*n)
    x1 <- rnorm(n1, sd = sig)
    x2 <- rnorm(n1, mean = d.true, sd = sig)
    s.pooled <- sqrt(var(x1)/2 + var(x2)/2)
    N <- ceiling(power.t.test(delta = d.pow, sd = s.pooled, power = 0.8)$n)
    n2 <- max(n, N) - n1
    y1 <- rnorm(n2, sd = sig)
    y2 <- rnorm(n2, mean = d.true, sd = sig)
    c(t.test(c(x1, y1), c(x2, y2), var.equal = TRUE)$p.value, n1 + n2)
}
