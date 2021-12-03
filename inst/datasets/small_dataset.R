set.seed(2323)
n = 27
a = sample(c("a", "b"), size=n, T)
b = sample(c("x", "y", "z"), size=n, T)
x = rnorm(n)
z = .4*x + rnorm(n, 0, sqrt(1-.4^2))
w_a = .4*x + rnorm(n, 0, sqrt(1-.4^2))
y = model.matrix(~a + b + x + z + x:a + w_a) %*% c(0, .3, .1, .3, .4, .2, -.2, .3) + rnorm(n, 0, .5)
y_bin = as.numeric(as.character(cut(y, breaks = c(-Inf,.3, Inf), labels=c(0,1))))
small = data.frame(y=y, a=factor(a), b=factor(b), z=z, x=x, y_bin=y_bin, w_a = round(rescale(w_a, 10, 3)))
usethis::use_data(small, overwrite=T)

# create random forest model
small_rf = party::cforest(y~., data=small)
usethis::use_data(small_rf, overwrite = T)
small_randomForest = randomForest::randomForest(y~., data=small)
usethis::use_data(small_randomForest, overwrite = T)


require(bluepill)
# simulate data where depression = stress + life_events + parental_depression + ses
fixed = c(0, .2, .5, .3, .2)
random = c(.1, .1, 0, .2, .1)
vars = list(
  y = c(0, 1, 3),
  x = c(0, 1, 3),
  a = c("no", "yes"),
  b = c("no", "mild", "moderate", "severe"),
  z = c(0, 1, 3),
  id = paste0("Dr. ", LETTERS[1:5])
)
small_mixed = mixed_model(fixed, random, sigma = .3, clusters=5, n_per = c(10,1), vars=vars)
usethis::use_data(small_mixed, overwrite = T)
small_mixed_mod = lme4::lmer(y~x + a + (1 | id), data=small_mixed)
usethis::use_data(small_mixed_mod, overwrite = T)
