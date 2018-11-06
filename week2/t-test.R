df <- iris
df1 <- subset(df, Species != 'setosa')
table(df1$Species)

hist(df1$Sepal.Length)
library(ggplot2)
#графики для оценки различий двух групп
#гистограммы
ggplot(df1, aes(x = Sepal.Length)) + 
  geom_histogram(fill='white', col='black', binwidth = 0.4) + 
  facet_grid(Species ~ .)

#плотность распределения
ggplot(df1, aes(Sepal.Length, fill= Species))+
  geom_density(alpha = 0.5)

#boxplot
ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

#проверка на нормальность распределения и гомогенности дисперсий

#Shapiro-Wilk test
shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == 'versicolor'])
shapiro.test(df1$Sepal.Length[df1$Species == 'virginica'])

#Barlett test гомогенности дисперсий
bartlett.test(Sepal.Length ~ Species, df1)

t.test(Sepal.Length ~ Species, df1)
# p-value сильно меньше 0.05
# дов. 95% интервал разницы средних не включает 0


#Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth. Данные позволяют 
#исследовать рост зубов у морских свинок в зависимости от дозировки витамина C и типа 
#потребляемых продуктов.
#Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) 
#с дозировкой 0.5 миллиграмм, со средним значением длины зубов свинок, которые потребляли 
#аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 

head(ToothGrowth)
str(ToothGrowth)
tg_sb <- subset(ToothGrowth, (supp=='OJ' & dose == 0.5) | (supp=='VC' & dose == 2))
tg_ttest <- t.test(len ~ supp, tg_sb)
table(tg_sb$supp)
tg_ttest$statistic


ggplot(tg_sb, aes(x = len)) + 
  geom_histogram(fill='white', col='black', binwidth = 3) + 
  facet_grid(supp ~ .)

ggplot(tg_sb, aes(len, fill= supp))+
  geom_density(alpha = 0.5)

ggplot(tg_sb, aes(supp, len))+
  geom_boxplot()

shapiro.test(tg_sb$len)
bartlett.test(len ~ supp, tg_sb)

#По всем испытуемым сравните показатель давления до начала лечения (Pressure_before) 
#с показателем давления после лечения (Pressure_after) при помощи t - 
#критерия для зависимых выборок. 
pressure <- read.csv('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv')
str(pressure)
t.test(pressure$Pressure_before, pressure$Pressure_after, paired = T)


#визуализация сравнения средних
# отобразить и средние и границы Д.И. по двум выборкам

ggplot(df1, aes(Species, Sepal.Length)) + 
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width=0.1)+
  stat_summary(fun.y = mean, geom='point')

ggplot(df1, aes(Species, Sepal.Length)) + 
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', size=1)

#функция расчета средней и Д.И.
mean_cl_normal(df$Sepal.Length)


#непараметрический аналог критерия t-Стьюдента: Wilcoxon test
#тест Вилкоксона, известный также как Мана-Витни тест
wilcox.test(Petal.Length ~ Species, df1)


#tasks
dft <- read.table('dataset_11504_16.txt')
str(dft)
bartlett.test(dft)
t.test(dft, var.equal = T)

#для построения графиков преобразуем table в dataframe с группировкой
f1 <- as.data.frame(list(dft$V1, c('T1')), col.names  =  c('v', 't'))
f2 <- as.data.frame(list(dft$V2, c('T2')), col.names  =  c('v', 't'))
dft2 <- rbind(f1, f2)

ggplot(dft2, aes(t, v))+
  geom_boxplot()