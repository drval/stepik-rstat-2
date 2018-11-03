#https://stepic.org/media/attachments/lesson/11502/grants.csv
df <- read.csv('https://stepic.org/media/attachments/lesson/11502/grants.csv')
str(df)
head(df)

#скорректируем датасет - изменим переменную как факторную и 
#установим уровни фактора
df$status <- as.factor(df$status)
levels(df$status) <- c('Not funded', 'Funded')
# v.2
df$status <- factor(df$status, labels = c('Not funded', 'Funded'))

#table - подсчет комбинаций у каждого уровня фактора в переменной
t1 <- table(df$status)
#размерность
dim(t1)

#группировка по status, потом по field
t2 <- table(df$status, df$field)
#название колонок.строк
t2 <- table(status=df$status, field = df$field)
dim(t2)

#% по кажому значению staus/field от общего числа заявок
prop.table(t2)
#% по кажому значению field/status от общего числа заявок в status
prop.table(t2, 1)
#% по кажому значению field/status от общего числа заявок в field
prop.table(t2, 2)

t3 <- table(Years <- df$years_in_uni, field = df$field, status=df$status)


#tasks
#% рыжеволосых от общего голубоглазых мужчин
HairEyeColor
#v.1
male <- HairEyeColor[,,'Male']
sum(male[,'Blue'])
sum(male['Red','Blue']) / sum(male[,'Blue'])
#v.2
prop.table(male, 2)['Red', 'Blue']
#v.3
as.numeric( prop.table(HairEyeColor[,'Blue','Male'])['Red'])
#число зеленоглазых женщин
sum(HairEyeColor[,,'Female'][,'Green'])

#plots

barplot(t2)
barplot(t2, legend.text = T, args.legend = list(x = 'topright'))
barplot(t2, legend.text = T, args.legend = list(x = 'topright'), beside = T)
mosaicplot(t2)


#Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из 
#таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать 
#цвет глаз. По оси Y - количество наблюдений.
mydata <- as.data.frame(HairEyeColor)

# С помощью scale_fill_manual мы говорим графику, что мы хотим, 
#чтобы он использовал указанные нами цвета.

obj <- ggplot(data = subset(mydata, Sex == 'Female'), aes(x = Hair, y = Freq, fill=Eye)) + 
geom_bar(stat="identity", position = position_dodge() ) + 
scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

#binom test
#биноминальный тест - показывает вероятность получить такой же или более откл. от среднего
# результат при условии что H0 верна. 
binom.test(x=3, n=20, p=0.5)
#мы можем применить б-тест и к таблице
binom.test(t1)

# Chi-Square - Хи-квадрат-тест
chi <- chisq.test(t1)
chi$expected

chi2 <- chisq.test(t2)
chi2$observed
chi2$expected

# точный критерий Фишера - работает устойчивее чем тест Хи-квадрат - 
# может работать с пропусками
ft <- fisher.test(t2)

#На основе таблицы HairEyeColor создайте ещё одну таблицу, в которой хранится информация 
#о распределении цвета глаз у женщин-шатенок (Hair = 'Brown'). Проведите тест равномерности 
#распределения цвета глаз у шатенок и выведите значение хи-квадрата для этого теста.

chisq.test(HairEyeColor[,,'Female']['Brown',])


#Воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат 
#проверьте гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color).
#В переменную main_stat сохраните значение статистики критерия Хи - квадрат. 
#Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом).
str(diamonds)
tdiam <- table(diamonds$cut, diamonds$color)
dchi <- chisq.test(tdiam)
as.vector( dchi$statistic)


#Опять воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат 
#проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов. 
#Для этого сначала нужно перевести эти количественные переменные в формат пригодный 
#для Хи - квадрат

# Создайте две новые переменные в данных diamonds:
#factor_price - где будет 1, если значение цены больше либо равно чем среднее, и 0, 
#если значение цены ниже среднего цены по выборке.

diamonds$factor_price <- as.factor( ifelse(diamonds$price >= mean(diamonds$price), 1, 0))

#factor_carat - где будет 1, если число карат больше либо равно чем среднее,  и 0, 
#если ниже среднего числа карат по выборке.
diamonds$factor_carat <- as.factor( ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))
chisq.test(diamonds$factor_price, diamonds$factor_carat)$sta

#При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) 
#и типа двигателя (vs) в данных mtcars. 
fisher.test(mtcars$am, mtcars$vs)
