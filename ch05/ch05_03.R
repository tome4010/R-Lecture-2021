# Avocado 사례
library(dplyr)

avocado <- read.csv('data/avocado.csv')
glimpse(avocado)

# 지역별 평균 판매량과 가격
avocado %>%
    group_by(region) %>%
    summarize(P_avg=mean(AveragePrice), V_avg=mean(Total.Volume))
# 지역별 연도별 평균 판매량과 가격
avocado %>%
    group_by(region, year, type) %>%
    summarize(P_avg=mean(AveragePrice), V_avg=mean(Total.Volume))
# 지역별 연도별 유기농여부에 따른
avocado %>%
    group_by(region, year) %>%
    summarize(P_avg=mean(AveragePrice), V_avg=mean(Total.Volume))

# 그룹 단위 통계를 시각화
library(ggplot2)
avocade %>%
    group_by(region, year, type) %>%\
    summarize(P_avg=mean(AveragePrice), V_avg=mean(Total.Volume)) %>% 
    filter(region != "TotalUS") %>%  
    ggplot(aes(year, V_avg, col=type)) + 
    geom_line() + 
    facet_wrap(~region)

x_avg <- avocado %>%
    group_by((region, year, type)) %>%
    summarize(p_avg mean(AveragePrice), V_avg=mean(Total.Volume))
arrange(x_avg, desc)) %>% head(10)

x_avg <- x_avg >%>
    filter(region!='TotalUS')
arrange(x_avg,desc(V_avg)) >%> head(10)

avocado %>%
    filter(region=='Califonia' &year==2018) %>%
    select()
