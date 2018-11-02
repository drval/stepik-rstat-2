df <- read.csv('https://stepic.org/media/attachments/lesson/11481/evals.csv')

#if
a <- 1
if (a > 0) {
  print(' > 0')
} else {
  print('<= 0')
}

a <- c(-1, 1)
ifelse(a > 0, '> 0', '<= 0')

#for
for (i in 1:10) {
  print(i)
}

#mtcars
str(mtcars)
ifelse(mtcars$carb >=4 | mtcars$cyl > 6, 1, 0)

mv <- c(1, 25, 7, 1, 2)
ifelse(mean(mv) > 20, 'mean > 20', 'mean <= 20')

#AirPassengers 
?AirPassengers
str(AirPassengers)

av <- as.vector(AirPassengers)
avn <- c(av[2:length(av)], 0)
avn[avn > av]
av

mavg <- rep(0, length(av) - 10)
for(z in 1:(length(av) - 9)) {
  mavg[z] <- mean(av[z:(z + 9)])
}
