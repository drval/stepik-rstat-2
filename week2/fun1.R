d1 <- rnorm(100)
d1[1:30] <- NA
hist(d1)
d1[is.na(d1)] <- mean(d1, na.rm = T)

my_na_rm <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = T)
  x
}

d1 <- my_na_rm(d1)

#Напишите функцию, которая выводит номера позиций пропущенных наблюдений 
#в векторе.
#На вход функция получает числовой вектор с пропущенными значениями. 
#Функция возвращает новый вектор с номерами позиций пропущенных значений.
NA.position <- function (v) {
  which(is.na(v1))
  #v2
  # (1:length(v1))[is.na(v1)]
}
v1 <- c(1, 2, NA, 3, NA, 6)
NA.position(v1)


#Напишите функцию NA.counter для подсчета пропущенных значений в векторе.

#На вход функция  NA.counter должна принимать один аргумент - числовой 
#вектор. Функция должна возвращать количество пропущенных значений.

#> my_vector <- c(1, 2, 3, NA, NA)
#> NA.counter(my_vector)
#[1] 2 
NA.counter <- function(v) {
  length(NA.position(v))
}
NA.counter(v1)