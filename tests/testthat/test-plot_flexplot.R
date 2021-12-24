flexplot(y~1, data=small)
flexplot(a~1, data=small)
flexplot(a~1, data=small %>% mutate(a = factor(a, ordered=T)))

flexplot(y~a, data=small[-27,], related=T)
