set.seed(2323)
n = 27
a = sample(c("a", "b"), size=n, T)
b = sample(c("x", "y", "z"), size=n, T)
x = rnorm(n)
z = .4*x + rnorm(n, 0, sqrt(1-.4^2))
y = model.matrix(~a + b + x + z + x:a) %*% c(0, .3, .1, .3, .4, .2, -.2) + rnorm(n, 0, .5)
y_bin = as.numeric(as.character(cut(y, breaks = c(-Inf,.3, Inf), labels=c(0,1))))
y_pois = round(y^2); y_pois
y_gam = (y^(2))
small = data.frame(y=y, a=factor(a), b=factor(b), z=z, x=x, y_bin=y_bin, y_pois = y_pois, y_gam = y_gam)
usethis::use_data(small, overwrite=T)

# create random forest model
small_rf = party::cforest(y~., data=small)
usethis::use_data(small_rf, overwrite = T)
small_randomForest = randomForest::randomForest(y~., data=small)
usethis::use_data(small_randomForest, overwrite = T)

# create other fitted objects
logistic_fit = glm(y_bin~x + z, data=small, family="binomial")
  usethis::use_data(logistic_fit, overwrite=T)
logistic_fit_interaction = glm(y_bin~x*z, data=small, family="binomial")
  usethis::use_data(logistic_fit_interaction, overwrite=T)  
poisson_fit = glm(y_pois~x + z, data=small, family="poisson")
  usethis::use_data(poisson_fit, overwrite=T)
gamma_fit = glm(y_gam~x + z, data=small, family=Gamma)
  usethis::use_data(gamma_fit, overwrite=T)
gaussian_fit = glm(y_bin~x + z, data=small)
  usethis::use_data(gaussian_fit, overwrite=T)  
  
  
  # predict(full, newdata=list(gender=c("female", "male"), safety = rep(mean(tablesaw.injury$safety), times=2)), 
  #         type="response")


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
small_mixed = bluepill::mixed_model(fixed, random, sigma = .3, clusters=5, n_per = c(10,1), vars=vars)
small_mixed$y_binary = cut(small_mixed$y, breaks = c(-Inf, 0, Inf))
usethis::use_data(small_mixed, overwrite = T)
small_mixed_mod = lme4::lmer(y~x + a + (1 | id), data=small_mixed)
usethis::use_data(small_mixed_mod, overwrite = T)
mixed_logistic = lme4::glmer(y_binary~x + a + (x | id), data=small_mixed, family="binomial")
mixed_logistic_2f = lme4::glmer(y_binary~x + a + b + (x | id), data=small_mixed, family="binomial")
  usethis::use_data(mixed_logistic, overwrite=T)  
  usethis::use_data(mixed_logistic_2f, overwrite=T)  


x = rnorm(100)
y = .5*x + rnorm(length(x), 0, sqrt(1-.5^2))
y_char = cut(y, breaks = c(-Inf, 0, Inf), labels=c("1-no", "0-yes")) %>% 
  as.character
y_numb = cut(y, breaks = c(-Inf, 0, Inf), labels=c(0, 1)) %>% 
  as.character %>%
  as.numeric
y_ord = cut(y, breaks = c(-Inf, 0, Inf), labels=c("no", "yes")) %>% 
  factor(levels=c("no", "yes"), ordered=T)
y_ord_rev = cut(y, breaks = c(-Inf, 0, Inf), labels=c("no", "yes")) %>% 
  factor(levels=c("yes", "no"), ordered=T)
small_logistic = data.frame(x=x, y=y, y_char=y_char, y_numb=y_numb, y_ord=y_ord, y_ord_rev)
usethis::use_data(small_logistic, overwrite=T)  



