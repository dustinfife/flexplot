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
flexplot(a~b, data=small)
flexplot(y_bin~b, data=small)

## beeswarm plots
flexplot(y~a, data=small)
flexplot(y~b, data=small, spread="stdev")
flexplot(y~y_bin, data=small)

# scatterplot
flexplot(y~x, data=small)

# panels
flexplot(y~x | b, data=small)
flexplot(y~a | b, data=small)
flexplot(a~x | b, data=small)
flexplot(a~b | y_bin, data=small)
flexplot(y~1 | b, data=small)
