1.
plot(mpg$cty, mpg$hwy)

2.
library(ggplot2)
library(dplyr)
head(midwest)

midwest %>%
    filter(poptotal<=500000 &popasian<=10000) %>%
    ggplot(aes(x=poptotal, y= popasian, 
               col=midwest$continent)) + 
    geom_point() 

3. # barplot 실행 안 됨
head(mpg)
mpg %>%
    filter(class=="suv") %>%
    select(manufacturer,cty) %>%
    arrange(desc(cty)) %>%
    head()
barplot(x$cty, names.arg=x$manufacturer)


4. # 코드 변형해서...
head(mpg)
mpg %>%
    filter(class==compact, class==subcompact, class==suv) %>%
    ggplot(aes(x=class,y=n) +
               geom_bar(stat='identity') +
               coord_flip()
           
           
5.
head(economics)
ggplot(economics, aes(x=date, y=psavert)) +
geom_point(alpha=0.5)
           
6.
head(mpg)
boxplot(class$compact, class$subcompact, class$suv)
           
           
           head(iris)
           boxplot(seto$Sepal.Length, seto$Sepal.Width,
                   seto$Petal.Length, seto$Petal.Width,
                   col=c('red','yellow','green','blue'),
                   names=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
                   main='Setosa')
           

           
           
           