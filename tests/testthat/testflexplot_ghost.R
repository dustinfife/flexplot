context("ghost line plots")
options(warn=-1)
data(exercise_data)
data("relationship_satisfaction")
d = exercise_data
k = d
deleteme = which(k$rewards == "no rewards")
k = k[-(deleteme[1:2]), ]
set.seed(1212)


test_that("ghost line plots work", {
  ### ghost lines
  a=flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red")	 %>%suppressMessages()  
  expect_flexplot_snapshot("testflexplot_ghost/just-ghost-line.svg", "just ghost line", a)
  b = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red",
               breaks = list(income = c(95000, 100000, 105000)),
               labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	%>%suppressMessages()
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-breaks-labels.svg", "ghost with breaks/labels", b)
  c = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	%>%suppressMessages()
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-choosing-panel.svg", "ghost with choosing panel", c)
  e = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31))%>%suppressMessages()
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-choosing-one-panel.svg", "ghost with choosing ONE panel", e)
  
  f = flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", 
               ghost.reference=list("health"=31, "income"=90000, gender="female"))	
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-choosing-second-slot.svg", "ghost with choosing second slot", f)
  g = flexplot(weight.loss~motivation + therapy.type | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000))%>%suppressMessages()		
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-using-both-second-slots.svg", "ghost with using both second slots", g)
  h = flexplot(gender~motivation | income + health, data=d, se=FALSE, method="logistic", ghost.line="gray")	%>%suppressMessages()	
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-logistic-regression.svg", "ghost with logistic regression", h)
  i = flexplot(conscientiousness ~ honesty + separated | gender, data=relationship_satisfaction, ghost.line="green")%>%suppressMessages()
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-previous-bug.svg", "ghost with previous bug", i)
  j = flexplot(y~x + a | b + y_bin, data=small, ghost.reference=list(a="a", b="z", y_bin=1))
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-previous-bug-binning.svg", "ghost with previous bug binning", j)
  expect_flexplot_snapshot("testflexplot_ghost/ghost-with-numeric-of-two-levels-previous-bug.svg", "ghost with numeric of two levels (previous bug)",
                              flexplot(z~x | y_bin, data=small, ghost.line="red")%>%suppressMessages())
})
options(warn=0)

