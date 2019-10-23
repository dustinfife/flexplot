data(exercise_data)
require(vdiffr)
data("relationship_satisfaction")
d = exercise_data
k = d
deleteme = which(k$rewards == "no rewards")
k = k[-(deleteme[1:2]), ]
set.seed(1212)
test_that("ghost line plots work", {
  
  ### ghost lines
  a=flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red")	   
  expect_doppelganger("just ghost line", a)         
  b = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red",
               breaks = list(income = c(95000, 100000, 105000)),
               labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	
  expect_doppelganger("ghost with breaks/labels", b)          	
  c = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	
  expect_doppelganger("ghost with choosing panel", c)          	
  e = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31))
  expect_doppelganger("ghost with choosing ONE panel", e)          	
  
  f = flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", 
               ghost.reference=list("health"=31, "income"=90000, gender="female"))	
  expect_doppelganger("ghost with choosing second slot", f)          	
  g = flexplot(weight.loss~motivation + therapy.type | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000))		
  expect_doppelganger("ghost with using both second slots", g)   
  h = flexplot(gender~motivation | income + health, data=d, se=FALSE, method="logistic", ghost.line="gray")		
  expect_doppelganger("ghost with logistic regression", h)
  i = flexplot(conscientiousness ~ honesty + separated | gender, data=relationship_satisfaction, ghost.line="green")
  expect_doppelganger("ghost with previous bug", i)
})