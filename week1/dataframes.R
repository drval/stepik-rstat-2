df <- read.csv('https://stepic.org/media/attachments/lesson/11481/evals.csv')
head(df, 3)
tail(df)

#data viewer
View(df)
#structure
str(df)

#summary
summary(df)
#column names
names(df)

df$score
summary(df$score)

df$double_score <- df$score * 2 
nrow(df)
ncol(df)
1:nrow(df)

#subsetting 
df$score[1:10]

#df[row, col]
df[2,3]
df[c(2, 193, 225), 1]
df[5, ]
df[, 1]
head(df[,2:5])
#subsetting with condition
nrow(df[df$gender == 'male', ])
# the same as 
nrow(subset(df, gender == 'male'))
subset(df, score > 4.9)[,1:5]
subset(df, score > mean(df$score))[,1]

#rbind, cbind
df_male <- subset(df, gender == 'male')
df_female <- subset(df, gender == 'female')
df_mf <- rbind(df_male, df_female)
nrow(df_mf)

df_5 <- df[, 5]
df_6 <- df[, 6]
df_56 <- cbind(df_5, df_6)
##mtcars
mtcars$even_gears <- 1 - mtcars$gear %% 2
head(mtcars)

mtcars[, c("mpg")]

#all except ...
mtcars[, -c(1, 2)]
#delete var
mtcars$z <- NULL
mpg_4 <- subset(mtcars, cyl == 4)$mpg
mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)),]
