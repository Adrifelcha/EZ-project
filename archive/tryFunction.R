a <- c(1:10, NA)

mean(a)

t <- try(samples <- jags(data=data_toJAGS,
                         parameters.to.save=parameters,
                         model="./model_perception.bug",
                         n.chains=n.chains,  n.iter=n.iter,
                         n.burnin=n.burnin,  n.thin=n.thin,
                         DIC=T, inits=myinits))

t <- try(samples <- 2+2)

samples


if("try-error" %in% class(t)) alternativeFunction()


if (inherits(t, "try-error")){ next }