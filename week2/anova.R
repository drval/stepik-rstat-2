#формулы
# DV ~ IV - зависиимая / независимая переменная
# DV ~ IV1 + IV2 - зависиимая / две независимые переменные
# DV ~ IV1:IV2 - взаимодействие нез. переменных:
  #когда влияние одной переменной на другую зависит от третьей

# DV ~ IV1 + IV2 + IV1:IV2 - главные эффекты + взаимодействие
# DV ~ IV1*IV2 - главные эффекты + взаимодействие (то же, но короче)

# Dv ~ (IV1 + IV2 + IV3)^2 - главные эффекты + взаимодействия до 2-го уровня
# т.е. IV1 + IV2 + IV3 + IV1:IV2 + IV1:IV3 + IV1:IV3

# DV ~ IV1 + Error(subject/IV1) - повторные измерения

dfs <- read.csv('https://stepic.org/media/attachments/lesson/11505/shops.csv')
str(dfs)

boxplot(price ~ origin, dfs)
#v.2
ggplot(dfs, aes(x = origin, y = price)) + 
  geom_boxplot()

#дисперсионный анализ
fit <- aov(price ~ origin, data = dfs)
summary(fit)

#двухфакторный Д.А. (two-way ANOVA)
fit <- aov(price ~ origin + store, data = dfs)
summary(fit)

#доп. информация. напр. среднее по группам
model.tables(fit, 'means')


pd = position_dodge(0.1)
ggplot(dfs, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()
#из графика видно, что влияние импорта распростроняется только
#на минимаркеты. строим модель с взаимодействиями 2-ух переменных

fit_i <- aov(price ~ origin + store + origin:store, data = dfs)
summary(fit_i)

#Воспользуемся встроенными данными npk, иллюстрирующими влияние 
#применения различных удобрений на урожайность гороха (yield). 
#Нашей задачей будет выяснить, существенно ли одновременное применение 
#азота (фактор N) и фосфата (фактор P). Примените дисперсионный анализ, 
#где будет проверяться влияние фактора применения азота (N), влияние 
#фактора применения фосфата (P) и их взаимодействие.
#В ответе укажите p-value для взаимодействия факторов N и P.

df <- npk
str(df)
f1 <- aov(yield ~ N + P + N:P, data = df)
summary(f1)

f2 <- aov(yield ~ N + P + K, data = df)
summary(f2)


ggplot(dfs, aes(x = food, y = price)) + 
  geom_boxplot()

fit <- aov(price ~ food, data = dfs)
summary(fit)
#попрарное сравнение групп с поправкой Тьюки
TukeyHSD(fit)

#Проведите однофакторный дисперсионный анализ на встроенных данных iris. 
#Зависимая переменная - ширина чашелистика (Sepal.Width), независимая 
#переменная - вид (Species). Затем проведите попарные сравнения видов. 
#Какие виды статистически значимо различаются по ширине чашелистика 
#(p < 0.05)?

df <- iris
str(df)
fit <- aov(Sepal.Width ~ Species, data = df)
summary(fit)
TukeyHSD(fit)

df <- read.csv('https://stepic.org/media/attachments/lesson/11505/therapy_data.csv')
str(df)
df$subject <- as.factor(df$subject)
#модель без учета повторных измерений 
f1 <- aov(well_being ~ therapy, data = df)
summary(f1)
#модель с учетом повторных измерений
f2 <- aov(well_being ~ therapy + Error(subject/therapy), data = df )
summary(f2)

#модель без учета повторных измерений - с учетом 2-ух нез. переменных
#выдает что цена оказывает значимое влияние на самочувствие
f3 <- aov(well_being ~ therapy * price, data = df)
summary(f3)

ggplot(df, aes(x = price, y = well_being))+geom_boxplot()

#поправка на множественные испытания - цена уже не является
#стат. значимым фактором!
f4 <- aov(well_being ~ therapy * price + Error(subject/(therapy*price)), data = df)
summary(f4)

#это показывает график - для 2-ух из 5 испытуемых цена явилась знач. фактором
ggplot(df, aes(x = price, y = well_being)) +
  geom_boxplot() + 
  facet_grid(~subject)

#учет межгрупповых факторов - без учета множ. сравнений
f5 <- aov(well_being ~ therapy * price * sex, data = df)
summary(f5)
#учет межгрупповых факторов - с учетом множ. сравнений
#пол не учитывается в поправке - т.к. является межгрупповым фактором - 
#его нельзя сгруппировать внтури испытуемого
f6 <- aov(well_being ~ therapy * price * sex + Error(subject/(therapy*price)), data = df)
summary(f6)


#Проведите однофакторный дисперсионный анализ с повторными измерениями: 
#влияние типа таблетки (pill) на температуру (temperature) с учётом 
#испытуемого (patient). Каково p-value для влияния типа таблеток 
#на температуру?

df <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
str(df)
df$patient <- as.factor(df$patient)

fit <- aov(temperature ~ pill + Error(patient/(pill)), data = df)
summary(fit)

ggplot(df, aes(x = pill, y = temperature)) +
  geom_boxplot() + 
  facet_grid(~patient)


fit2 <- aov(temperature ~ doctor*pill + Error(patient/(doctor*pill)), data = df)
summary(fit2)