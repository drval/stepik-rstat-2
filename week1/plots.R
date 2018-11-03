df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$mpg, breaks = 20, xlab = "MPG")
boxplot(mpg ~ am, df)
plot(df$mpg, df$hp)
#а теперь - фишшечки
hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)
plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)
boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)
boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)
plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)
plot(~ mpg + hp, df) 

library(ggplot2)

ggplot(df, aes(x = mpg)) + 
  geom_histogram(fill="white", col="black", binwidth = 2)

#простой dotplot - отображает кол-во авто с соотв. mpg
ggplot(df, aes(x = mpg)) + 
  geom_dotplot()

#не простой dotplot - отображает кол-во авто с соотв. mpg и типу авто
ggplot(df, aes(x = mpg, fill=am)) + 
  geom_dotplot()

#плотность распределения mpg
ggplot(df, aes(x = mpg)) + 
  geom_density(fill="red")

ggplot(df, aes(x = mpg)) + 
  geom_density(aes(fill=am), alpha=0.2)

ggplot(df, aes(x = mpg, fill=am)) + 
  geom_density(alpha = 0.2)

#не красиво
ggplot(df, aes(x = am, y = hp)) + 
  geom_point()
#красиво - boxplot
ggplot(df, aes(x = am, y = hp)) + 
  geom_boxplot()

#boxplot - красим - по факту группировка еще и по типу двигателя
ggplot(df, aes(x = am, y = hp, col = vs)) + 
  geom_boxplot()

#scatterplot
ggplot(df, aes(x = mpg, y = hp)) + 
  geom_point(size=6)

#цветом выделяем тип двигателя
ggplot(df, aes(x = mpg, y = hp, col=vs)) + 
  geom_point(size=6)
 
#размер - время разгона
last_plot <- ggplot(df, aes(x = mpg, y = hp, col=vs, size=qsec)) + 
  geom_point()
last_plot

#base plot
plotbase <- ggplot(df, aes(x = am, y = hp, col=vs))
plotbase + geom_boxplot()

#tasks
#При помощи функции ggplot() или boxplot() постройте график boxplot, используя встроенные 
#в R данные airquality. По оси x отложите номер месяца, по оси y — значения переменной Ozone.
str(airquality)
ggplot(airquality, aes(x=as.factor(Month), y = Ozone)) + 
  geom_boxplot()


#Используем знакомые нам данные mtcars. 
#Нужно построить scatterplot с помощью ggplot из ggplot2, по оси x которого будет mpg, 
#по оси y - disp, а цветом отобразить переменную (hp).
ggplot(mtcars, aes(x = mpg, y = disp, col = hp))+
  geom_point()


ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(aes(size = Petal.Length))