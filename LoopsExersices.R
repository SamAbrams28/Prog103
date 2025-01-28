# simple loop
for (i in 4:8){
  print(i)
}

# two types of iterators
animal <- c('fish','octopus', 'fish', 'fish', 'octopus')
count <- c(3, 1, 6, 7, 2)
# first type of iterator
for (a in animal){
  print(a)
}
# second type of iterator
for (i in i:length(animal)){
  print(rep(animal[i], count[i]))
}

# allocate result
octopus_mass_g <- 135
fish_mass_g <- 60
total_mass <- rep(0, length(animal))
for (i in 1:length(animal)){
  if(animal[i] == 'octopus'){
    total_mass[i] <- count[i] * octopus_mass_g
  } else if (animal[i] == 'fish'){
    total_mass[i] <- count[i] * fish_mass_g
  }
}
total_mass
animal
count
