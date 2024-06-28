# Data processing

library(here)
recordings <- read.csv2(here("Data/processed/Registros.csv"))
View(recordings)
head(recordings)

# Camera trap functioning

## presences (1), absences and true NA (NA) per observed period for each sampling station
cam.on <- ifelse(is.na(tapply(rep(1,nrow(recordings)),list(recordings$ID_ponto,recordings$Quinz),sum)),NA,1)

## species presence (1) and absences including NAs (0)
occ <- tapply(rep(1,nrow(recordings)),list(recordings$ID_ponto,recordings$Quinz,recordings$Especie),max,default = 0)

dims <- dim(occ) # check the matrix dimensions

## multiplicating the to matrices in order to get the presences, and true absences and NAs
for(i in 1:dims[3]){
  occ[,,i]<-occ[,,i]*cam.on
}

# selecting the target species: maned wolf, gray brocket deer, and giant armadillo
occ.spp <- occ[,,c("Chrysocyon_brachyurus", "Subulo_gouazoubira", "Priodontes_maximus")]
head(occ.spp)