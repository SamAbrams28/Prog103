i <- -1
if (i == 1) {
  print("first")
} else if (i == 2){
  print("second")
} else{
  print("other")
}

socal <- c('Los Angeles', 'Orange','San Diego')
county1 <- 'San Fransisco'
county2 <- 'Mendocino'
if (county1 %in% socal & county2 %in% socal) {
  print("both counties in socal")
} else if(county1 %in% socal){
  print("county1 in socal")
} else if(county2 %in% socal){
  print("county2 in socal")
} else{
  print("no counties in socal")
}
