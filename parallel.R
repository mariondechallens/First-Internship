##trying parallel computing with R

library(foreach)
library(doParallel)
library(doSNOW)

detectCores() #how many cores are available for parallel execution
# Create cluster with desired number of cores
cl <- makeCluster(3)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used for parallel execution
getDoParWorkers()


x <- foreach::foreach(i = 1:3, .combine = c) %dopar% sqrt(i)
y <- foreach(i = 1:3, .combine = "+") %dopar% sqrt(i)  ##somme 
z <- foreach(i = 1:3, .combine = "*") %dopar% sqrt(i)

parLapply(cl, list(1, 2, 3), sqrt)

##example 1
#Generate fake tree data set with 100 observations for 100 species
tree.df <- data.frame(species = rep(c(1:100), each = 100), girth = runif(10000,7, 40))
tree.df$volume <- tree.df$species/10 + 5 * tree.df$girth + rnorm(10000, 0, 3)
# Extract species IDs to iterate over
species <- unique(tree.df$species)

# modèle de regression sur chaque espèce
fits <- foreach(i = species, .combine = rbind) %dopar% {
sp <- subset(tree.df, subset = species == i)
fit <- lm(volume ~ girth, data = sp)
return(c(i, fit$coefficients))
}

head(fits)

#What if we want all of the info from the lm object? Change .combine
fullfits <- foreach(i = species) %dopar% {
sp <- subset(tree.df, subset = species == i)
fit <- lm(volume ~ girth, data = sp)
return(fit)
}
attributes(fullfits[[100]])

###example 2
data<-cars
vit<-unique(data$speed)
data$speed2<-foreach(i = 1:nrow(data), .combine = c) %dopar% data$speed[i]^2
data$temps<-foreach(i = 1:nrow(data), .combine = rbind) %dopar% {round(data$dist[i]/data$speed[i],digit=2)}

fits <- foreach(i = vit, .combine = rbind) %dopar% {
  sp <- subset(data, subset = vit == i)
  fit <- lm(speed2 ~ temps, data = data)
  return(c(i, fit$coefficients))
}

head(fits)


for (i in 1:5){
  e<-data[i,]
  print(d)
}


foreach::foreach(i=1:5)%dopar%{
  f<-data[i,]
  print(f)
}
