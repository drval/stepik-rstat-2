?swiss
df <- swiss
str(df)

hist(df$Fertility, col='green')

#строим модель
fit <- lm(Fertility ~ Examination + Catholic ,data = df)
summary(fit)

fit_2 <- lm(Fertility ~ Examination * Catholic ,data = df)
summary(fit_2)

#дов. интервалы для коэф. модели
confint(fit_2)

#категориальные предикторы
hist(df$Catholic, col = 'red')
#мы види что переменная распределена неравномерно
# создадим категориальную 
df$confession <- ifelse(df$Catholic < 60, 'Protestant', 'Catholic')
df$confession <- as.factor(df$confession)
str(df$confession)
fit_3 <- lm(Fertility ~ Examination + confession, data = df)
summary(fit_3)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)           87.0221     3.1165  27.923   <2e-16 ***
#   Examination           -0.6858     0.2222  -3.086   0.0035 ** 
#   confessionProtestant  -8.4469     3.7016  -2.282   0.0274 * 
#Анализ модели:
#в модели с кат. переменной Intercept не может означать среднее при 
# 0-ых значениях всех переменных. для кат. переменной такого не может быть!

#В нашем случае Intercept - среднее значение предсказанноой переменной 
# при значени кат. переменной 1-го уровня - Catholic, при том что Examination = 0
# т.е. среднее значение рождаемости при условии что уровень здоровья = 0
# в провинциях с преобл. конфессией - католицизм
# Examination показывает насколько изменяется значение Intercept при условии
# что comfession = 'Catholic'
# confessionProtestant - покажет, как поменяется рождаемость при 
# переходе от провинции с конфесс. = 'Catholic' к конф. = 'Protestat'

fit_4 <- lm(Fertility ~ Examination * confession, data = df)
summary(fit_4)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       79.1545     5.4001  14.658   <2e-16 ***
#   Examination                        0.1479     0.5201   0.284   0.7775    
# confessionProtestant               2.9615     7.4096   0.400   0.6914    
# Examination:confessionProtestant  -1.0096     0.5723  -1.764   0.0848 . 

# Examination:confessionProtestant показывает как влияет Examination на 
# Fertility при переходе от ConfessionCatholic к confessionProtestant

#plots
library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


ggplot(df, aes(x = Examination, y = Fertility, col=confession)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

df$confession <- ifelse(df$Catholic > 60, 'Cath', '_Prot')
df$confession <- as.factor(df$confession)
str(df$confession)
fit_5 <- lm(Fertility ~ confession * Infant.Mortality * Examination, data = df)
summary(fit_5)


################

# Памятка по интерпретации результатов регрессионного анализа с категориальными и непрерывными переменными
# Модель для примера: 
#   
#   DV ~ IV_numeric * IV_categorical
# 
# IV_categorical - фактор с двумя уровнями (Level1 и Level2)
# 
# Коэффициенты:
#   
#   Intercept — предсказанное значение DV для первого уровня IV_categorical с учётом того, что IV_numeric равна нулю.
# 
# IV_numeric — насколько изменяется предсказанное значение DV при увеличении IV_numeric на одну единицу в группе, соответствующей первому уровню IV_categorical
# 
# IV_categoricalLevel2 — насколько изменяется предсказанное значение DV при переходе от первого уровня IV_categorical ко второму уровню. С учётом того, что IV_numeric равна нулю. 
# 
# IV_numeric:IV_categoricalLevel2 — насколько сильнее (или слабее) изменяется предсказанное значение DV при увеличении IV_numeric на одну единицу в группе, соответствующей второму уровню IV_categorical, по сравнению с первым уровнем. 
# 
# Как предсказывать значения в новом датасете на основе полученных коэффициентов
# 
# 1). Предположим у нас есть новый объект, про который мы знаем, что он принадлежит к группе, соответствующей IV_categorical (Level1) и измеренный у него IV_numeric составил 10:
#   
#   Предсказанное значение DV = Intercept + 10 * IV_numeric
#   
#   2). Предположим у нас есть новый объект, про который мы знаем, что он принадлежит к группе, соответствующей IV_categorical (Level2) и измеренный у него IV_numeric составил 6:
#   
#   Предсказанное значение DV = Intercept + IV_categoricalLevel2 + 6 * (IV_numeric + IV_numeric:IV_categoricalLevel2)
###############


df <- mtcars
df$am <- factor(df$am, labels = c('Automatic', 'Manual'))
str(df$am)
fit_am <- lm(mpg ~ wt*am, df)
summary(fit_am)

my_plot <- ggplot(df, aes(x = wt, y = mpg, col=am)) + 
  geom_smooth(method = 'lm')

#отбор моделей
# от всех предикторов
fit_full <- lm(Fertility ~ ., data = swiss )
summary(fit_full)

#урезаная без agriculture
f_r1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(f_r1)

#сравниваем
#H0 - модели одинаково объясняют зависимость и различие в R^2 стат. не значимо
anova(fit_full, f_r1)
#отвергаем и берем лучший R^2 - т.е. полную модель

f_r2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic + Education, data = swiss)
summary(f_r2)
anova(fit_full, f_r2)
#различия в моделях не значимы. - можно брать вторую модель

#вручную отбирать предикторы долго - использум step
#выбираем направление и получаем опт. модель!
opt_fit <- step(fit_full, direction = 'backward')
summary(opt_fit)


model_full <- lm(rating ~ ., data = ?attitude) 
model_null <- lm(rating ~ 1, data = attitude)
summary(model_full)
summary(model_null)

scope = list(lower = model_null, upper = model_full)
m1 <- step(scope = scope, object = model_null, direction = 'forward')
m2 <- step(scope = scope, object = model_full, direction = 'backward')
summary(m2)

anova(model_full, m1)

#диагностика данных для регрессии
str(swiss)
#взаимосвязь всех переменных - pairs
pairs(swiss)
#взаимосвязь двух переменных
library(ggplot2)

#визуальное определение выбросов
ggplot(swiss, aes(Examination, Education))+
  geom_point() + 
  geom_smooth(method = 'lm')

#визуальная проверка на нормальность
ggplot(swiss, aes(Examination)) + 
  geom_histogram(fill='yellow', col='black')

#распределение скошено вправо, применяем выравнивнивание. log
ggplot(swiss, aes(log(Education))) + 
  geom_histogram(fill='yellow', col='black')


#определяем какая функция согласно Shapiro-test подходит к нам для Н. распределения
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

shapiro.test(my_vector)$p
shapiro.test(sqrt(my_vector))$p

vfun <- c( function (x) list('sqrt', sqrt(x)), 
           function (x) list('1/x',1/x), 
           function(x) list('log',log(x)));



sapply(vfun, function(x) {
  f <- x(my_vector);
  df <- as.data.frame(f[[2]])
  colnames(df) <- c('Y')
  print(ggplot(df, aes(Y)) + geom_histogram(fill='yellow', col='black'));
  c(shapiro.test(f[[2]])$p, f[[1]])
} )


#линейная модель vs полиномиальная
ggplot(swiss, aes(Examination, Education))+
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_squared <- (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)
#R^2 второй модели > R^2 первой, т.е. она лучше описывает данные
#сравнение моделей
anova(lm2, lm1)
#различия в моделях значимы

#добавим предсказанные значения и остатки
swiss$lm1_fitted <- lm1$fitted.values
swiss$lm2_fitted <- lm2$fitted.values
swiss$lm1_resid <- lm1$residuals
swiss$lm2_resid <- lm2$residuals
swiss$obs_number <- 1:nrow(swiss)

#отображаем линии тренда на графике
ggplot(swiss, aes(Examination, Education))+
  geom_point(size=3)+
  geom_line(aes(Examination, lm1_fitted), col='red', lwd=1)+
  geom_line(aes(Examination, lm2_fitted), col='blue', lwd=1)

#отобразим распределенеи остатков
ggplot(swiss, aes(lm1_fitted, lm1_resid))+
  geom_point(size=1) + geom_hline(yintercept = 0, col='red', lwd=1)

ggplot(swiss, aes(lm2_fitted, lm2_resid))+
  geom_point(size=3) + geom_hline(yintercept = 0, col='red', lwd=1)

#проверка допущения о независимости остатков (чтоб остатки не были сгруппированы)
ggplot(swiss, aes(obs_number, lm1_resid))+
  geom_point(size=1) + geom_smooth()

ggplot(swiss, aes(obs_number, lm2_resid))+
  geom_point(size=1) + geom_smooth()

#гомоскедастичность - изменчивость остатков постоянна на всем протяжении модели

ggplot(swiss, aes(lm1_fitted, lm1_resid))+
  geom_point(size=1)

ggplot(swiss, aes(lm2_fitted, lm2_resid))+
  geom_point(size=1)

library(gvlma)
x <- gvlma(lm2)
summary(x)

ds <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
str(ds)
lmds <- lm(DV ~ IV, ds)
ds$fitted <- lmds$fitted.values
ds$resid <- lmds$residuals
ds$num <- 1:nrow(ds)

ggplot(ds, aes(IV, DV)) + 
  geom_point(size=1)+
  geom_line(aes(IV, fitted), col='red', lwd=1)

ggplot(ds, aes(fitted, resid))+
  geom_point(size=1) + geom_hline(yintercept = 0, col='red', lwd=1)

ggplot(ds, aes(num, resid))+
  geom_point(size=1) + geom_smooth()

summary(gvlma(lmds))

#нормальность распределения остатков
ggplot(swiss, aes(lm1_resid))+
  geom_histogram(binwidth = 4, fill='white', col='black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

############################
cor_swiss <- cor(swiss)
dgm <- diag(nrow = nrow(cor_swiss), ncol = ncol(cor_swiss))
diag(cor_swiss) <- 0
cor_swiss_abs <- abs(cor_swiss - dgm)
rownames(which(cor_swiss_abs == max(cor_swiss_abs), arr.ind = T))

high.corr <- function(x) {
  cor_ds <- cor(x)
  dgm <- diag(nrow = nrow(cor_ds), ncol = ncol(cor_ds))
  cor_ds_abs <- abs(cor_ds - dgm)
  rownames(which(cor_ds_abs == max(cor_ds_abs), arr.ind = T))
}

high.corr(swiss)
high.corr(iris[,-5])

x1 <- rnorm(30)
x2 <- rnorm(30)
x3  <- x1 + 5
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
high.corr(my_df)