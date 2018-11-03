?mtcars
df <- mtcars
str(df)
#vs - тип двигателя, am-тип коробки - преобразуем в фактор.
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

#Рассчитайте среднее значение времени разгона (qsec) для автомобилей, 
#число цилиндров (cyl) у которых не равняется 3 и показатель количества миль на галлон топлива (mpg) больше 20.
mean(subset(df, cyl != 3 & mpg > 20)$qsec)

#aggregate
mean_hp_vs <- aggregate(x = df$hp,  by = list(df$vs), FUN = mean)
colnames(mean_hp_vs) <- c("VS", "Mean HP")
#more pretty...
aggregate(hp ~ vs, df, mean)

aggregate(hp ~vs + am, df, mean)

#медиана всех кол. переменных сгруп. по типа к/п
aggregate(x = df[, -c(8,9)], by= list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)
#pretty
aggregate(cbind(mpg, disp) ~ am + vs, df, sd)


#рассчитайте стандартное отклонение переменной hp (лошадиные силы) и 
#переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
aggregate(cbind(hp, disp) ~ am, df, sd)

#описательная статистика по всему датасету одним махом
library(psych)
describe(df)
describe(df[,-c(8,9)])

#describeBy: статистика с группировкой.
# mat=T - выводить в виде датафрейма(матрица). иначе list
# fast=T - быстрый вариант. базовые статистики
dfd <- describeBy(df[,-c(8,9)], group = df$vs, mat = T, digits = 2, fast=T)
describeBy(df$qsec, list(df$vs, df$am), mat=T, digits=1, fast=T)

##
#
#
#NA
sum(is.na(df))
#подпортим df
df$mpg[1:10] <- NA
mean(df$mpg) #NA
mean(df$mpg, na.rm = T)

#Воспользуемся встроенными данными airquality. В новую переменную сохраните subset исходных данных, 
#оставив наблюдения только для месяцев 7, 8 и 9.
m789 <- subset(airquality, Month == 7 | Month == 8 | Month == 9)
m789 <- subset(airquality, Month %in% c(7, 8, 9) )
#При помощи функции aggregate рассчитайте количество непропущенных наблюдений 
#по переменной Ozone в 7, 8 и 9 месяце. Для определения количества наблюдений используйте функцию length(). 
str(m789)
aggregate(Ozone~Month, data=m789, FUN=length)

aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length)

#Примените функцию describeBy к количественным переменным данных airquality, 
#группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии (skew) 
#переменной Wind в восьмом месяце?
str(airquality)
rs <- describeBy(airquality, group= list(airquality$Month), mat=T)
rownames(rs)

str(iris)
describe(iris)[,c(1, 5)]


my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
#замена пропущенных значений на средние
replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

