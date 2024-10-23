library(ISwR)

print(table(stroke2$dead))

print(summary(glm(dead ~ sex+age+factor(entry), poisson, offset=log(exit-entry), data=stroke2)))