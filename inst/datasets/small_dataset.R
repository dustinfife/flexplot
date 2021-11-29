n = 27
a = sample(c("a", "b"), size=n, T)
b = sample(c("x", "y", "z"), size=n, T)
x = rnorm(n)
z = .4*x + rnorm(n, 0, sqrt(1-.4^2))
y = model.matrix(~a + b + x + z + x:a) %*% c(0, .3, .1, .3, .4, .2, -.2) + rnorm(n, 0, .5)
y_bin = as.numeric(as.character(cut(y, breaks = c(-Inf,.3, Inf), labels=c(0,1))))
small = data.frame(y=y, a=a, b=b, z=z, x=x, y_bin=y_bin)
usethis::use_data(small, overwrite=T)
