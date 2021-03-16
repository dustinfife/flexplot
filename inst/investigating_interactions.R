require(flexplot)
require(tidyverse)

covmat = matrix(.3, nrow=5, ncol=5)
diag(covmat) = 1
d = MASS::mvrnorm(3000, mu=c(0,0,0,0,0), Sigma=covmat) %>% 
  data.frame %>% 
  set_names(nm=c("y", paste0("x", 1:4))) %>% 
  mutate(y2 = y + .5*scale(x3)*scale(x4) + -.3*I(x2^2), y=y2)

## two way interaction, but both are paneled
# data-generating model: y = b0 + b1 X1 +                   # main effect of x1
#                                 b2 X2 + b3 X2^2           # nonlinear x2
#                                 b4 X3 + b5 X4 + b6 X3*X4  # interaction between x3/x4

# Basic strategy
# 1. Plot all variables at once, looking for interactions or nonlinear effects
#       - make sure to alternate having each variable on the x axis
p = flexplot(y~x3 + x2 | x1 + x4, data=d, method="lm", alpha=.2,
             labels=list("x1" = c("low", "mid", "high"), 
                         "x4" = c("low", "mid", "high")))
flexplot(weight.loss~motivation | therapy.type, data=exercise_data, 
         method="lm")
# use marginal plots to see the row/column trends
marginal_plot(p)

# 2. Use AVPs to visually subtract out interactions/nonlinear effects
added.plot(y~x3 | x1 + x4, data=d, lm_formula = y~x3*x4, method="lm")
added.plot(y~x3 | x2 + x4, data=d, lm_formula = y~x3*x4, method="lm")
  # interaction has been removed

added.plot(y~x2 | x1 + x4, data=d, lm_formula = y~x3*x4 + x2, method="quadratic")
  # we can see the quadratic relationship, now let's remove it
added.plot(y~x2 | x1 + x4, data=d, lm_formula = y~x3*x4 + x2 + I(x2^2), method="quadratic")
  # polynomial has been removed


# anything with x1?
added.plot(y~x1 | x2 + x3, data=d, lm_formula = y~x3*x4 + x2 + I(x2^2), method="quadratic")
  # small parallel slopes, so let's model the slope
added.plot(y~x1 | x2 + x3, data=d, lm_formula = y~x3*x4 + x2 + x1 + I(x2^2), method="quadratic")
  # there's nothing left

# visual conditioning instead of visual partitioning
a = added.plot(y~x1, data=d, lm_formula = y~x3*x4 + x2 + I(x2^2), method="lm")
b = added.plot(y~x2, data=d, lm_formula = y~x3*x4 + x1, method="quadratic")
c = added.plot(y~x3 | x4, data=d, lm_formula = y~ x2 + x1 + I(x2^2), method="lm")
require(patchwork)
a+b+c

# flexplot(y2~x1 + x2 | x3+x4, data=d, method="lm", alpha=.1)
#   # no interaction visible
# flexplot(y2~x1 + x3 | x2+x4, data=d, method="lm", alpha=.1, ghost.line="black")
#   # no interaction visible
# flexplot(y2~x3 + x2 | x1+x4, data=d, method="lm", alpha=.1, ghost.line="black")
#   # we can see interactions! Slope raises as we go up the rows
# flexplot(y2~x4 + x2 | x3+x1, data=d, method="lm", alpha=0)
#   # conclusion: one of the variables doing the interacting has to be on X axis!
# flexplot(y2~x4 + x3 | x2+x1, data=d, method="lm", alpha=0)
#   # do lines deslopify across row panels? column panels? Or across color?


# plot ghost line as a function of marginal effects
# probably best to do as the prediction matrix
mod1 = lm(y~x1+x2+x3+x4, data=d)
mod2 = lm(y~x1*x2*x3*x4, data=d)
compare.fits(y~x3 | x1 + x4, data=d, mod1, mod2) +
  scale_color_manual(values=c("gray", "red"))

d$residuals = residuals(lm(y~x3*x4, data=d))
flexplot(residuals~x3 | x2 + x4, data=d, method="lm")
marginal_plot(added_plot(y~x3|x1 + x4, data=d, lm_formula = y~x3*x4, method="lm"))


mod1 = lm(y~x1+x2+x3+x4, data=d)
mod2 = lm(y~x1*x2*x3*x4, data=d)
compare.fits(y~x3 | x1 + x4, data=d, mod1, mod2, raw.data=F) +
  scale_color_manual(values=c("gray", "red"))

# once we detect the interaction, let's residualize it, and see if there's anything left besides that
d$residuals_int = residuals(lm(y~x3*x4, data=d))
mod1 = lm(residuals_int~x1+x2, data=d)
mod2 = lm(residuals_int~x1*x2, data=d)
compare.fits(residuals_int~x1 | x2, data=d, mod1, mod2, sample = 100) 
  # it works!!!!!


# now add another interaction and see if we do detect that
d2 = d %>% mutate(y2 = y + .2*scale(x1)*scale(x2))
d2$residuals_int = residuals(lm(y2~x3*x4, data=d2))
mod1 = lm(residuals_int~x1+x2, data=d2)
mod2 = lm(residuals_int~x1*x2, data=d2)
compare.fits(residuals_int~x1 | x2, data=d2, mod1, mod2, sample = 100)

# now see how to detect a 3-way interaction
d3 = d %>% mutate(y2 = y + .4*scale(x1)*scale(x2)*scale(x3))
d3$residuals_int = residuals(lm(y2~x3*x4, data=d3))
mod1 = lm(residuals_int~x2*x3, data=d3)
mod2 = lm(residuals_int~x2*x3*x1*x4, data=d3)
compare.fits(residuals_int~x2 | x3, data=d3, mod1, mod2, sample = 100)
  # this AVP removes the two way, but not the three way. By placing the variables in the two way, we see nonparallel lines

# 0. Compare full model to main effects model
# .5. Alternatate variables on the x axis. 
# 1. Identify where slopes are changing (across panels, rows, or colors)
# 2. Model that interaction and residualize
# 3. Do AVP, after removing the interaction, placing the main effects of residualized variables in plot. If parallel, no interactions remain between those vars and other vars
# 4. Repeat with variables not included in interaction. 
# What if there's a 3-way???

# create a plot that averages rows/columns. Maybe ghost.line = c("row_mean", "col_mean", "grand_mean")
mod1 = lm(y~x3*x4, data=d)
dnew = flexplot(y~x3 | x1 + x4, data=d, method="lm", return_data = T)

layers = list(geom_smooth(method="lm", formula = y~x),
              coord_fixed(),   
              theme_bw(),
              theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                    axis.text.y = element_blank(), axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(), 
                    axis.ticks.y = element_blank()))
              
a = ggplot(data=dnew, aes(x=x3, y=y)) + 
  facet_grid(x4_binned~.)  + 
  layers
b = ggplot(data=dnew, aes(x=x3, y=y)) + 
  facet_grid(~x1_binned)  + 
  layers
gap = ggplot(data=dnew, aes(x=x3, y=y)) + 
  layers +
  theme(plot.background = element_rect(fill = 'yellow', linetype = 'solid', colour = 'black'),
  plot.margin = unit(c(0,0,0,0), "cm"))
c = flexplot(y~x3 | x1 + x4, data=d, method="lm")
require(patchwork)
(b + gap + plot_layout(widths=c(5,1)))/(c + a + plot_layout(widths=c(5,1))) +
  plot_layout(heights = c(1,5))  


# 1. Compare full model to main effects model (see if lines are different)
# 2. Alternate variables on the x axis
# 3. Identify where slopes are changing (across panels, rows, or colors) using marginal_plots
# 4. Model that interaction and residualize
# 5. Do AVP, after removing the interaction.
    # for each variable in interaction, place on x-axis to make sure it doesn't interact anymore as a three-way
    # if all variables are parallel, you have extracted the interaction
# 6. Repeat with variables not included in interaction. 

