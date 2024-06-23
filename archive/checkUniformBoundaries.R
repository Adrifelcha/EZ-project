source("../code/functions/default_priors.R")
priors <- default_priors("FALSE", "ttest")

## Boundary_mean
b <- seq(0.1,3,0.01)
plot(b,dnorm(b,priors$bound_mean_mean,priors$bound_mean_sdev), type="l")
# 95% C.I
qnorm(c(0.025,0.975),1.5,0.2)
## Implied Bound distribution
plot(b,dnorm(b,qnorm(0.025,1.5,0.2),priors$bound_sdev_lower*1.15))
plot(b,dnorm(b,qnorm(0.025,1.5,0.2),priors$bound_sdev_upper*0.85))
plot(b,dnorm(b,qnorm(0.975,1.5,0.2),priors$bound_sdev_lower*1.15))
plot(b,dnorm(b,qnorm(0.975,1.5,0.2),priors$bound_sdev_upper*0.85))
plot(b,dnorm(b,qnorm(0.975,1.5,0.2),0.3))
plot(b,dnorm(b,qnorm(0.025,1.5,0.2),0.3))

## Drift_mean
d <- seq(-3,3,0.01)
plot(d,dnorm(d,priors$drift_mean_mean,priors$drift_mean_sdev))
# 95% C.I
qnorm(c(0.025,0.975),0,0.5)
#Implied drift distribution
plot(d,dnorm(d,qnorm(0.025,0,0.5),priors$drift_sdev_lower*1.1))
plot(d,dnorm(d,qnorm(0.025,0,0.5),priors$drift_sdev_upper*0.9))
plot(d,dnorm(d,qnorm(0.975,0,0.5),priors$drift_sdev_lower*1.1))
plot(d,dnorm(d,qnorm(0.975,0,0.5),priors$drift_sdev_upper*0.9))


## Nondecision_mean
n <- seq(0.05,2,0.01)
plot(n, dnorm(n,priors$nondt_mean_mean,priors$nondt_mean_sdev))
# 95% C.I
qnorm(c(0.025,0.975),0.3,0.06)
plot(n,dnorm(n,0.15,priors$nondt_sdev_lower*1.1))
plot(n,dnorm(n,0.15,priors$nondt_sdev_upper*0.9))
plot(n,dnorm(n,0.45,priors$nondt_sdev_lower*1.1))
plot(n,dnorm(n,0.45,priors$nondt_sdev_upper*0.9))
plot(n,dnorm(n,0.18,0.05))
plot(n,dnorm(n,0.41,0.05))
plot(n,dnorm(n,0.18,0.2))
plot(n,dnorm(n,0.41,0.2))
abline(v=0.1)

plot(seq(0.05,2,0.01), 
     dnorm(seq(0.05,2,0.01),0.1,0.1))