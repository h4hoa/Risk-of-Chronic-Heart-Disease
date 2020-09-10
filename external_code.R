## ---- first-figure
## Pair plots
pairs(~ (log(chdrisk) - log(1-chdrisk)) + age + totchol + sysbp + diabp + 
        cigpday + bmi + heartrte + glucose + hdlc + ldlc, data = fhs)

## ---- second-figure
## Residual plots for first candidate model
par(mfrow = c(1,2), mar = c(4,4,.1,.1)) # plot frame
pch = 21 # plot character
cex = .8 # size of data point
# plot the residuals vs. fitted value for first model
plot(x = 0, type = "n", # empty plot to get the axis range
     xlim = range(y.hat),
     ylim = range(Resid), cex.lab = cex, cex.axis = cex,
     xlab = 'Predicted CHD risk score', ylab = 'Residuals CHD risk score')
# add dotted lines between residuals to enhance visibility
res.y0 <- apply(Resid, 1, min)
res.y1 <- apply(Resid, 1, max)
segments(x0 = y.hat, y0 = res.y0, y1 = res.y1, lty = 2)
# add points
for(ii in 1:4) {
  points(y.hat, Resid[,ii], pch = pch, cex = cex, bg = clrs[ii])
}
abline(h = 0, col = "grey60") # zero residual line
legend("bottomleft",
       legend = c("Standardized", "Studentized", "PRESS", "DFFITS"),
       pch = 21, pt.cex = cex, cex = cex,
       pt.bg = c("black", "blue", "red", "orange"),
       title = "Residual Type:")

# histogram for studentized residuals
hist(stud.res, breaks = 50, freq = FALSE, cex.axis = cex,
     xlab = "Studentized Residual CHD risk score", main = "")
curve(dnorm(x), col = "red", add = TRUE) # fit the normal curve

## ---- third-figure
## Residual plots for second candidate model
par(mfrow = c(1,2), mar = c(4,4,.1,.1)) # plot frame
pch = 21 # plot character
cex = .8 # size of data point
# plot the residual vs. fitted value for first model
plot(x = 0, type = "n", # empty plot to get the axis range
     xlim = range(y.hat1),
     ylim = range(Resid1), cex.lab = cex, cex.axis = cex,
     xlab = 'Predicted CHD risk score', ylab = 'Residuals CHD risk score')
# add dotted lines between residuals to enhance visibility
res.y01 <- apply(Resid1, 1, min)
res.y11 <- apply(Resid1, 1, max)
segments(x0 = y.hat1, y0 = res.y01, y1 = res.y11, lty = 2)
# add points
for(ii in 1:4) {
  points(y.hat1, Resid1[,ii], pch = pch, cex = cex, bg = clrs[ii])
}
abline(h = 0, col = "grey60") # zero residual line
legend("bottomleft",
       legend = c("Standardized", "Studentized", "PRESS", "DFFITS"),
       pch = 21, pt.cex = cex, cex = cex,
       pt.bg = c("black", "blue", "red", "orange"),
       title = "Residual Type:")

# histogram for studentized residuals
hist(stud.res1, breaks = 50, freq = FALSE, cex.axis = cex,
     xlab = "Studentized Residual Risk scores", main = "")
curve(dnorm(x), col = "red", add = TRUE) # fit the normal curve

## ---- fourth-figure
## Leverage vs. Influence plot
# Cook's Distance vs. Leverage first model
clrs[lev.ind] <- 'blue' # color for high leverage index
clrs[infl.ind] <- 'red' # color for high influence index
# plot Cook's Distance measure against leverage
plot(h, D, xlab = 'Leverage', ylab = "Cook's Distance", 
     pch = pch, bg = clrs, cex = cex, cex.axis = cex)
# limit for high leverage points
abline(v = 2*hbar, col = 'grey60', lty=2) 
# specifying high influence and leverage points
legend("topleft", legend = c("High Leverage", "High Influence"), 
       pch = pch, pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)

## ---- fifth-figure
# Cook's Distance vs. Leverage second model
clrs[lev.ind1] <- 'blue' # color for high leverage index
clrs[infl.ind1] <- 'red' # color for high influence index
# plot Cook's Distance measure against leverage
plot(h1, D1, xlab = 'Leverage', ylab = "Cook's Distance Measure", 
     pch = pch, bg = clrs, cex = cex, cex.axis = cex)
# limit for high leverage points
abline(v = 2*hbar1, col = 'grey60', lty=2)
# specifying high influence and leverage points
legend("topleft", legend = c("High Leverage", "High Influence"), 
       pch = pch, pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)

## ---- sixth-figure
## Boxplot and histogram of out-of-sample log likelihood ratio statistics
par(mfrow = c(1,2), mar = c(5.1, 5.1, 3, 1.1)*0.6) # plot frame
cex <- .8 # size of data points

# boxplot
boxplot(x = list(sqrt(mspe1), sqrt(mspe2)), names = Mnames,
        main = "rMSPE",
        ylab = "rMSPE",
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)

# out-of-sample log likelihood ratio statistics
lambda <- lambda1 - lambda2
# histogram of out-of-sample log likelihood ratio statistics
hist(log(lambda), breaks = 50, freq = FALSE,
     main = "Log-Likelihood Ratio Statistic",
     ylab = "Density",
     xlab = "log-likelihood ratio test",
     cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
abline(v = mean(log(lambda)), col = "red", lwd = 2) # mean of lambda


## ---- Others

## Summary Statistics
summary(fhs)


## Variance inflation factor
X <- model.matrix(lm(log(chdrisk) - log(1 - chdrisk)~ . - 1 - sex - 
                       cursmoke - diabetes - bpmeds - prevmi - prevstrk - prevhyp, 
                     data = fhs)) # model matrix without catergorial data
C <- cor(X) # correlation matrix
vif <- diag(solve(C)) # diagonal lines of the model matrix 
vif


## First Candidate Model
# intercept only
M0 <- lm(log(chdrisk) - log(1 - chdrisk) ~ 1, data = fhs) 

# all main effects and interactions
Mmax <- lm(log(chdrisk) - log(1 - chdrisk) ~ (.)^2, data = fhs) 

beta.max <- coef(Mmax)
length(beta.max) # number of coefficients
names(beta.max)[is.na(beta.max)] # coefficients that couldn't be estimated
table(fhs[c("cursmoke", "cigpday")]) 

Mmax <- lm(log(chdrisk) - log(1 - chdrisk) ~ (.)^2 - cursmoke:cigpday - 
             bpmeds:prevhyp, data = fhs)
anyNA(coef(Mmax)) # make sure there are no coefficients with NA in the model

# starting point model: main effects only
Mstart <- lm(log(chdrisk) - log(1 - chdrisk) ~ ., data = fhs) 

# forward
system.time({
  Mfwd <- step(object = M0, # base model
               scope = list(lower = M0, upper = Mmax), # smallest and largest model
               direction = "forward",
               trace = FALSE) # trace prints out information
})

# backward
system.time({
  Mback <- step(object = Mmax, # base model
                scope = list(lower = M0, upper = Mmax),
                direction = "backward", trace = FALSE)
})

# stepwise 
system.time({
  Mstep <- step(object = Mstart,
                scope = list(lower = M0, upper = Mmax),
                direction = "both", trace = FALSE)
})

# F-statistics for first model
anova(Mfwd, Mstep)
anova(Mstep, Mback)
anova(Mback, Mfwd)


## Second Candidate Model
# basic linear model
M1 <- lm(log(chdrisk) - log(1 - chdrisk) ~ ., data = fhs) #basic linear model
# modified linear model
M2 <- lm(log(chdrisk) - log(1 - chdrisk) ~ . - totchol - ldlc, data = fhs) 

# F-statistics for second model
anova(M2, M1)


## Residuals for first candidate model
# predicted values
y.hat <- predict(Mstep) 
sigma.hat <- sigma(Mstep)

# ordinary residuals
res <- resid(Mstep) 

# standardized residuals
stan.res <- res/sigma.hat 

# computing leverages for finding the studentized residuals
X <- model.matrix(Mstep)
H <- X%*%solve(crossprod(X), t(X)) # HAT matrix
h <- hatvalues(Mstep)

# studentized residuals
stud.res <- stan.res/sqrt(1-h)

# PRESS residuals
press.res <- res/(1-h)

# DFFITS residuals
dfts.res <- dffits(Mstep)

# collect all residuals
Resid <- data.frame(stan = stan.res,
                    stud = stud.res,
                    press = press.res,
                    dffits = dfts.res)

hbar  <- ncol(model.matrix(Mstep))/nobs(Mstep) # 
# standardize residuals by making them all equal at average leverage
Resid <- within(Resid, {
  stud <- stud.res * sqrt(1-hbar)
  press <- press.res * (1-hbar)/sigma.hat
  dffits <- dfts.res * (1-hbar)/sqrt(hbar)
})


## Residualss for second candidate model
# predicted values
y.hat1 <- predict(M2) 
sigma.hat1 <- sigma(M2)

# ordinary residuals
res1 <- resid(M2) 

# standardized residuals
stan.res1 <- res1/sigma.hat 

# computing leverages for finding the studentized residuals
X1 <- model.matrix(M2)
H1 <- X1%*%solve(crossprod(X1), t(X1)) # HAT matrix
h1 <- hatvalues(M2)

# studentized residuals
stud.res1 <- stan.res1/sqrt(1-h1)

# PRESS residuals
press.res1 <- res1/(1-h1)

# DFFITS residuals
dfts.res1 <- dffits(M2)

# collect all residuals
Resid1 <- data.frame(stan = stan.res1,
                     stud = stud.res1,
                     press = press.res1,
                     dffits = dfts.res1)

hbar1  <- ncol(model.matrix(M2))/nobs(M2) # average leverage
# standardize residuals by making them all equal at average leverage
Resid1 <- within(Resid1, {
  stud <- stud.res1 * sqrt(1-hbar1)
  press <- press.res1 * (1-hbar1)/sigma.hat1
  dffits <- dfts.res1 * (1-hbar1)/sqrt(hbar1)
})


## Leverage and Influence measures for first model
clrs <- c("black", "blue", "red", "orange") # plot frame
pch = 21 # plot character
cex = .8 # size of data points
# Cook's distance for the first model
D <- cooks.distance(Mstep) # calculate Cook's Distance Measures

infl.ind <- which.max(D) # top influence point
lev.ind <- h > 2*hbar # leverage more than twice average 


## Leverage and Influence measures for second model

# Cook's distance for the second model
D1 <- cooks.distance(M2) # calculate Cook's Distance Measures

infl.ind1 <- which.max(D1) # top influence point
lev.ind1 <- h1 > 2*hbar1 # leverage more than twice average 



## Outliers
# Outliers from the first model
omit.ind <- c(infl.ind, # most influential
              which.max(h)) # highest leverage
# naming the row of most influential and highest leverage points
names(omit.ind) <- c("infl", "lev")
# details of most influential and highest leverage points
fhs[omit.ind,]

# Outliers from the second model
omit.ind1 <- c(infl.ind1, # most influential
               which.max(h1)) # highest leverage
# naming the row of most influential and highest leverage points
names(omit.ind1) <- c("infl", "lev")
# details of most influential and highest leverage points
fhs[omit.ind1,]

## Cross-Validation Process
require(statmod)

# logitnorm_mean function
logitnorm_mean <- function(mu, sigma){
  v <- 1/(1+exp(-mu))
  alpha1 <- 1/((sigma^2)*(1-v))
  alpha2 <- 1/(v*(sigma^2))
  gqp <- gauss.quad.prob(n=10,dist="beta",alpha=alpha1,beta=alpha2)
  x <- gqp$nodes
  y <- gqp$weights
  g <- dnorm((log(x/(1-x))),mean=mu,sd=sigma,log = TRUE) - log(1-x) - 
    dbeta(x, shape1=alpha1,shape2 = alpha2, log = TRUE)
  sum(y*(exp(g)))
}


# compare Mstep to M2
M10 <- Mstep
M20 <- M2
Mnames <- expression(M[STEP], M[MLM])

# number of cross-validation replications
nreps <- 2e3 # number of replications
ntot <- nrow(fhs) # total number of observations
ntrain <- 1500 # for fitting MLE's
ntest <- ntot-ntrain # for out-of-sample prediction

# storage space
mspe1 <- rep(NA, nreps) # mean-squared prediction errorsfor M10
mspe2 <- rep(NA, nreps) # mean-squared prediction errors for M20
lambda1 <- rep(NA, nreps) # out-of-sample log-likelihood for M1
lambda2 <- rep(NA, nreps) # out-of-sample log-likelihood for M2

# cross-validation
system.time({
  for(ii in 1:nreps) {
    if(ii%%100 == 0) message("ii = ", ii)
    train.ind <- sample(ntot, ntrain) # training observations
    
    # fit the models on the subset of training data
    M10.cv <- update(M10, subset = train.ind)
    M20.cv <- update(M20, subset = train.ind)
    
    # out-of-sample log-likelihoods
    M10.sigma <- sqrt(sum(resid(M10.cv)^2)/ntrain) # MLE of sigma
    M20.sigma <- sqrt(sum(resid(M20.cv)^2)/ntrain)
    
    mu_1 <- predict(M10.cv, newdata = fhs[-train.ind,])
    mu_2 <- predict(M20.cv, newdata = fhs[-train.ind,])
    
    # out-of-sample residuals
    M10.res <- fhs$chdrisk[-train.ind] - 
      sapply(1:ntest, function(ii) logitnorm_mean(mu_1[ii],M10.sigma))
    M20.res <- fhs$chdrisk[-train.ind] - 
      sapply(1:ntest, function(ii) logitnorm_mean(mu_2[ii],M20.sigma))
    
    # mean-square prediction errors for each model
    mspe1[ii] <- mean(M10.res^2)
    mspe2[ii] <- mean(M20.res^2)
    
    # since res = y - pred, dnorm(y, pred, sd) = dnorm(res, 0, sd)
    lambda1[ii] <- sum(dnorm(M10.res, sd = M10.sigma, log = TRUE))
    lambda2[ii] <- sum(dnorm(M20.res, sd = M20.sigma, log = TRUE))
  }
})



## Final model
summary(Mstep)
