myvar <- 42
v2 <- 35.25
v3 <- myvar^2 + v2^2
v3 > 200

1 : 67
v1 <- 1 : 67
v2 <- c(-32, 45, 67, 12.78, 129, 0, -65)
v2[1]

#обращение к неск. элементам
v2[c(1,2,3)]
v2[1:3]

v2[c(1,2,3, 10)]

the_best_vector <- c(c(1:5000),c(7000:10000))

v1 + 10
v2 == 0
#индексация по логическому условию
v2[v2 > 0]
v1[v1 > 20 & v1 < 30]


#выборка всех больше средней
v1  <- c(165, 178, 180, 181, 167, 178, 187, 167, 187)
mean_v1 <- mean(v1)
gtm <- v1[v1 > mean_v1]


#list
age <- c(16, 18, 22, 27)
is_married <- c(F, F, T, T)
name <- c('Olga', 'Maria', 'Nastya', 'Polina')
data <- list(age, is_married, name)
data[[1]][1]
data[[2]][3]

#dataframe
df <- data.frame(Name=name, Age = age, Status = is_married)

