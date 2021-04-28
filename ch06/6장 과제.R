str(gapminder)

# 1
x <- gapminder %>%
    filter(year==1952) %>%
    select(country, pop) %>%
    arrange(desc(pop)) %>%
    head() +
    pie(x$pop, x$country)

# 1-(2)


pop_compo <- function(x) {
    for (i in n) {
        x <- gapminder %>%
            filter(year>=1952 &year<=2007)
            select(country, pop) %>%
            arrange(desc(pop)) %>%
            head() +
            pie(x$pop, x$country)
    }
    return(x)
}


