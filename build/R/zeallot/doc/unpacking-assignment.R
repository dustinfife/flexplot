## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

library(zeallot)

## ------------------------------------------------------------------------
c(lat, lng) %<-% list(38.061944, -122.643889)

## ------------------------------------------------------------------------
lat
lng

## ------------------------------------------------------------------------
c(lat, lng) %<-% c(38.061944, -122.643889)
lat
lng

## ------------------------------------------------------------------------
c(min_wt, q1_wt, med_wt, mean_wt, q3_wt, max_wt) %<-% summary(mtcars$wt)
min_wt
q1_wt
med_wt
mean_wt
q3_wt
max_wt

## ---- error=TRUE---------------------------------------------------------
c(stg1, stg2, stg3) %<-% list("Moe", "Donald")

## ---- error=TRUE---------------------------------------------------------
c(stg1, stg2, stg3) %<-% list("Moe", "Larry", "Curley", "Donald")

## ------------------------------------------------------------------------
#
# A function which returns a list of 2 numeric values.
# 
coords_list <- function() {
  list(38.061944, -122.643889)
}

c(lat, lng) %<-% coords_list()
lat
lng

## ------------------------------------------------------------------------
#
# Convert cartesian coordinates to polar
#
to_polar = function(x, y) {
  c(sqrt(x^2 + y^2), atan(y / x))
}

c(radius, angle) %<-% to_polar(12, 5)
radius
angle

## ------------------------------------------------------------------------
c(inter, slope) %<-% coef(lm(mpg ~ cyl, data = mtcars))
inter
slope

## ---- eval = require("purrr")--------------------------------------------
safe_log <- purrr::safely(log)

## ---- eval = require("purrr")--------------------------------------------
pair <- safe_log(10)
pair$result
pair$error

## ---- eval = require("purrr")--------------------------------------------
pair <- safe_log("donald")
pair$result
pair$error

## ---- eval = require("purrr")--------------------------------------------
c(res, err) %<-% safe_log(10)
res
err

## ------------------------------------------------------------------------
c(mpg, cyl, disp, hp) %<-% mtcars[, 1:4]

head(mpg)

head(cyl)

head(disp)

head(hp)

## ------------------------------------------------------------------------
quartet <- lapply(1:4, function(i) anscombe[, c(i, i + 4)])

c(an1, an2, an3, an4) %<-% lapply(quartet, head, n = 3)

an1

an2

an3

an4

## ------------------------------------------------------------------------
c(a, c(b, d), e) %<-% list("begin", list("middle1", "middle2"), "end")
a
b
d
e

## ---- error=TRUE---------------------------------------------------------
c(a, c(b, d, e), f) %<-% list("begin", list("middle1", "middle2"), "end")

## ------------------------------------------------------------------------
c(ch1, ch2, ch3) %<-% "abc"
ch1
ch2
ch3

## ------------------------------------------------------------------------
c(y, m, d) %<-% Sys.Date()
y
m
d

## ------------------------------------------------------------------------
f <- lm(mpg ~ cyl, data = mtcars)

c(fcall, fterms, resids, ...rest) %<-% summary(f)

fcall

fterms

head(resids)

## ------------------------------------------------------------------------
str(rest)

## ---- error = TRUE-------------------------------------------------------
c(fcall, fterms, resids, rest) %<-% summary(f)

## ------------------------------------------------------------------------
c(...skip, e, f) %<-% list(1, 2, 3, 4, 5)
skip
e
f

## ------------------------------------------------------------------------
c(begin, ...middle, end) %<-% list(1, 2, 3, 4, 5)
begin
middle
end

## ------------------------------------------------------------------------
c(min_wt, ., ., mean_wt, ., max_wt) %<-% summary(mtcars$wt)
min_wt
mean_wt
max_wt

## ------------------------------------------------------------------------
c(begin, ..., end) %<-% list("hello", "blah", list("blah"), "blah", "world!")
begin
end

## ------------------------------------------------------------------------
c(begin, ., ...middle, end) %<-% as.list(1:5)
begin
middle
end

## ------------------------------------------------------------------------
nums <- 1:2
c(x, y) %<-% tail(nums, 2)
x
y

## ---- error = TRUE-------------------------------------------------------
c(x, y, z) %<-% tail(nums, 3)

## ------------------------------------------------------------------------
c(x, y, z = NULL) %<-% tail(nums, 3)
x
y
z

## ------------------------------------------------------------------------
c(first, last) %<-% c("Ai", "Genly")
first
last

c(first, last) %<-% c(last, first)
first
last

## ------------------------------------------------------------------------
cat <- "meow"
dog <- "bark"

c(cat, dog, fish) %<-% c(dog, cat, dog)
cat
dog
fish

## ---- eval = require("magrittr")-----------------------------------------
library(magrittr)

mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean() %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %->% 
  c(cyl, mpg, ...rest)

cyl
mpg
rest

