# Occupancy models

occupancy <- "model{
for(k in 1:length(data[,,k])){
for(i in 1:nrow){
for(j in 1:ncol){
n[i,j,k]~dbin(theta[k]*Z[i,k], 1)
}
Z[i,k]~dbin(psi[k], 1)
}
}

}"