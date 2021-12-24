#univariate plots
flexplot(y~1, data=small)
flexplot(a~1, data=small)
flexplot(a~1, data=small %>% mutate(a = factor(a, ordered=T)))

#related plots
flexplot(y~a, data=small[-27,], related=T)
expect_error(flexplot(y~a, data=small, related=T))
expect_error(flexplot(y~b, data=small[-27,], related=T))

#logistic plots
flexplot(a~x, data=small)
flexplot(y_bin~x, data=small)

#association plots

