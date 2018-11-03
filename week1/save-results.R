library(psych)
df <- mtcars
summary <- describe(df, -c(8,9))
#save data
write.csv(df, "mtcars.csv")
write.csv(summary, "summary.csv")

x <- 20
save(x, file = 'x.RData')
