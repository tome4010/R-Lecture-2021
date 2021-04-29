---
title: "Galtons dataset"
author: "Hyunjung Song"
date: '2021 4 29 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### 아버지와 아들의 키의 회귀식을 구해보고 의미를 파악해 보시오.
library(ggplot2)

Galtons <- read.csv("http://www.randomservices.org/random/data/Galton.txt", sep='\t')
head(Galtons)

Father <- Galtons[,2] * 2.54
Kids <- Galtons[,6] * 2.54
Galtons %>%
    filter(Gender=="M") %>%
    select(Father, Kids, Gender) 
   
ggplot(aes(Father, Kids), data=Galtons) +
    geom_point() +
    geom_abline()
    
m <- lm(Kids ~ Father, data=Galtons)
coef(m)
summary(m)






