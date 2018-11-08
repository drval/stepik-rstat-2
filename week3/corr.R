df <- mtcars

#кор. анализ
fit <- cor.test(df$mpg, df$hp)
# в виде формулы
fit <- cor.test(~ mpg + hp, df)

plot(df$mpg, df$hp)

library(ggplot2)

ggplot(df, aes(x=mpg, y = hp, col=factor(cyl))) + 
  geom_point(size=3)

#многофакт. корр. анализ
#выберем все кол. переменные
dfn <- df[, c(1, 3:7)]
#график попарной корреляцци
pairs(dfn)
#многофак. корреляция - расчет
cor(dfn)
#многофак. корреляция - расчет более подробный
library(psych)
fit <- corr.test(dfn)
fit$r
#p-уровень значимости
fit$p



#Напишите функцию corr.calc, которая на вход получает data.frame 
#с двумя количественными переменными, рассчитывает коэффициент корреляции 
#Пирсона и возвращает вектор из двух значений: коэффициент корреляции 
#и p - уровень значимости.

corr.calc <- function(df) {
  tr <- cor.test( df[,c(1)], df[,c(2)] )
  c(as.numeric(tr$est), tr$p.value)
}

dfv <-  df[,c(1,3)]
corr.calc(dfv)
corr.calc(iris[,1:2])


#линейная регрессия
fit <- lm(mpg ~ hp, df)
summary(fit)


ggplot(df, aes(x=hp, y = mpg)) + 
  geom_point(size=3)+
  geom_smooth(method = 'lm')+
  #geom_smooth()
  facet_grid(.~cyl)



#разлчия при применении aes к графику в целом и конкретному geom
ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth(se = F)


ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))


#предсказанные значения
fitted_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

#предсказанные значения для новых переменных
new_hp <- data.frame(hp = c(100, 150, 129, 200))
new_hp$mpg <- predict(fit, new_hp)

#независимая факторная переменная
#подаем cyl как кол. переменную
fit <- lm(mpg ~ cyl, df)
summary(fit)
#строим график - не OK
ggplot(df, aes(cyl, mpg)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

#представляем cyl как фактор
df$cyl <- factor(df$cyl, labels=c("four", "six", "eight"))
fit <- lm(mpg ~ cyl, df)
summary(fit)
#теперь в статистике присутствуют группы 6 и 8 цил. Intercept - 4 цил.
aggregate(mpg ~ cyl, df, mean)
#заметим что Intercept = средней по групп 4 ц.
# значения 6 и 8 групп - отклонение средних от Intercept

#непарам. тест не сможет предсказать точное p-значение
#если в данных есть одинаковые наблюдения
cor.test(mtcars$mpg, mtcars$disp, method = "spearman")

# тут поможет spearman_test
install.packages('coin')
library(coin)
spearman_test(~ mpg + disp, mtcars)
