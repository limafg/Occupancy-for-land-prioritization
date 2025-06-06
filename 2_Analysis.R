# Occupancy models

library(rjags)


# i = site -> ncol
# j = detection history -> nrow
# k = species -> length(data[,,k])


occupancy <- "model{
for(k in 1:nspecies){
for(i in 1:nsite){
for(j in 1:nhistory){
n[i,j,k]~dbin(theta[k]*Z[i,k], 1)
}
Z[i,k]~dbin(psi[k], 1)
}
logit(theta[k]) <- b0[k]

# priors 
b0[k]~dnorm(b0m, 1/sigmatm^2)
logit(psi[k]) <- c0[k]
c0[k]~dnorm(c0m, 1/sigmapm^2)
}

# hyperpriors
b0m~dnorm(0,1/100^2)
sigmatm~dunif(0, 100)

c0m~dnorm(0,1/100^2)
sigmapm~dunif(0, 100)
}"

writeLines(occupancy, "JAGS/occup.txt")

data <- list(n = occ.spp, nspecies = length(occ.spp[,,k]), nsites = nrow(occ.spp), nhistory = ncol(occ.spp))

z.init <- ifelse(rowSums(occ.spp,na.rm=TRUE)>0,1,0) # z initial
init <- list(z = z.init)

m.occ <- jags.model("JAGS","occupancy.txt", data = data, n.chains = 3, n.adapt = 500, inits = init)
samples.occ <- coda.samples(m.occ, variable.names = c("psi", "theta"), n.iter = 5000)

plot(samples.occ)