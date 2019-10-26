## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(knitr)
# set global chunk options
#opts_chunk$set(prompt=TRUE)
#options(replace.assign=TRUE, width = 90, prompt = "Rzan> ")

## ---- smiley, out.width = "400px", fig.cap = "A table of numbers on the left, and a color-coded table on the right, where the 2's have been highlighted in yellow. With the color, a pattern emerges that was not easy to see without the graphic. Figure used with permission from Correll (2015). \\label{fig:smiley}", message=FALSE, warning=FALSE,fig.align='center'----
require(ggplot2)
require(flexplot)
data(exercise_data)
knitr::include_graphics("images/smiley.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.align = 'center'-----
require(ggplot2)
require(flexplot)
data(exercise_data)

## ---- message=FALSE, echo=TRUE, warning=FALSE, options(prompt=" ", continue=" ")----
plot = ggplot(data = exercise_data, aes(x=therapy.type, y=weight.loss)) +
  geom_jitter(width = .2, alpha = .4) +
  stat_summary(fun.y = 'mean', geom = 'point', 
        size = 3, position = position_dodge(width = .2)) +
  stat_summary(geom = 'errorbar', fun.ymin = function(z){mean(z)-1.96*sd(z)}, 
        fun.ymax=function(z) {mean(z)+1.96*sd(z)}, 
        size = 1.25, width = .2, position = position_dodge(width = .2)) 

## ---- message=FALSE, echo=TRUE, warning=FALSE----------------------------
plot = flexplot(weight.loss ~ therapy.type, data = exercise_data)

## ---- flexplotgrammar, out.width= "500px", fig.cap = "A diagram showing how elements of Flexplot's graphics are represented in a plot. $X1$ is shown on the $X$ axis, $X2$ shows up as different colors/symbols/lines, $X3$ panels in columns, and $X4$ panels in rows. \\label{fig:flexplotgrammar}", message=FALSE, warning=FALSE, echo=FALSE, fig.align='center'----
require(ggplot2)
require(flexplot)
data(exercise_data)
knitr::include_graphics("images/flexplot_diagram.jpg")

## ---- message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "A histogram (left) and barchart (right) produced within \\pkg{flexplot}.", fig.width = 6, fig.height = 3, out.width = "90%", fig.align = 'center'----
require(flexplot)
data(exercise_data) #### these are simulated data available 
                    #### in flexplot. Please don't base your 
                    #### weight loss program on this dataset
a = flexplot(weight.loss ~ 1, data = exercise_data)
b = flexplot(gender ~ 1, data = exercise_data)
cowplot::plot_grid(a , b)

## ---- raw, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Violin plots with 15,000 versus 15 datapoints. The outlines look the same in the left image, but the right image overlays the raw data, which makes the differing sample sizes much more apparent. \\label{fig:raw}", fig.width = 6, fig.height = 3, out.width = "90%", fig.align = 'center'----
group1 = c(0,1,2,2,3,3,3,3,3,3,4,4,5,6)
group2 = rnorm(10000,3,1)
d = data.frame(score = c(group1, group2), 
               group = c(rep("group 1", times = length(group1)), 
                       rep("group 2", times = length(group2))))
a = ggplot2::ggplot(data = d, aes(x = group, y = score)) +  
  geom_violin() + theme_bw()
b = flexplot(score ~ group, data = d)
cowplot::plot_grid(a, b)


## ---- jitterit, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Dot plots with the interquartile range and no jittering (left), JD plot with mean + standard errors and jittering on $X$ and $Y$ (middle), and JD plot with mean + standard deviation with jittering on only $X$ (right).", fig.width = 6, fig.height = 2.5, out.width = "90%", fig.align = 'center'----
a = flexplot(weight.loss ~ therapy.type, data = exercise_data, 
             jitter = F, spread = "quartile")
b = flexplot(weight.loss ~ therapy.type, data = exercise_data, 
             jitter = c(.4,.5), spread = "sterr")
c = flexplot(weight.loss ~ therapy.type, data = exercise_data, 
             jitter = .2, spread = "stdev")
cowplot::plot_grid(a, b, c, nrow = 1)

## ---- message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Scatterplot with different options of fit: \\code{loess} (default), \\code{lm} (regression), \\code{polynomial}. Also, the data in the far right plot has been jittered. ", fig.width = 6, fig.height = 2.5, out.width = "90%", fig.align = 'center'----
a = flexplot(weight.loss ~ satisfaction, data = exercise_data) + 
  theme_minimal() ### using layering to change theme
b = flexplot(weight.loss ~ satisfaction, data = exercise_data, 
             method = "lm", se = F)
c = flexplot(weight.loss ~ satisfaction, data = exercise_data, 
             method = "polynomial", jitter = .4)
cowplot::plot_grid(a, b, c, nrow = 1)

## ---- logistic, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Example of a logistic plot in the \\pkg{flexplot} package.\\label{fig:logistic}", out.width="40%", fig.width = 3, fig.height = 3----
data("tablesaw.injury") ### also simulated data available 
                        ### in flexplot package
                        ### always remember to be safe 
                        ### and attentive when woodworking
flexplot(injury ~ attention, data = tablesaw.injury, 
             method = "logistic", jitter = c(0, .05))

## ---- association, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Example of an association plot for categorical predictor/categorical outcome.\\label{fig:association}" , out.width="40%", fig.width = 3.5, fig.height = 2.5----
tablesaw.injury$injury = factor(tablesaw.injury$injury, 
                  levels=c(0, 1), labels=c("all good", "ouch"))
flexplot(injury ~ gender, data = tablesaw.injury)


## ---- plant, message=FALSE, echo=TRUE, warning=FALSE,out.width="40%", fig.width = 3, fig.height = 3, fig.cap = "A plot of repeated measures data.\\label{fig:plant}"----
data("plant_growth")
flexplot(Diameter ~ Soil.Type, data = plant_growth, related=T)

## ---- sample, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Four graphics showing different ways to handle overlapping datapoints. The top-left image does nothing. The top-right does omits raw data. The bottom-left reduces the opacity of the points. The bottom-right samples datapoints. \\label{fig:sample}"----
data("nsduh")

nrow(nsduh)
a = flexplot(distress ~ major.dep, data = nsduh)
b = flexplot(distress ~ major.dep, data = nsduh, raw.data = F)
c = flexplot(distress ~ major.dep, data = nsduh, alpha = .005)
d = flexplot(distress ~ major.dep, data = nsduh, sample = 200)
cowplot::plot_grid(a, b, c, d, nrow = 2)

## ---- avp, message=FALSE, echo=TRUE, warning=FALSE, fig.width = 3, fig.height = 2, out.width = "50%", fig.cap = "An Added Variable Plot (AVP) showing the relationship between \\code{therapy.type} and \\code{weight.loss}, after controlling for \\code{motivation}. \\label{fig:avp}"----
added.plot(weight.loss ~ motivation + therapy.type, data = exercise_data)

## ---- symbols, message=FALSE, echo=TRUE, warning=FALSE, fig.width = 6, fig.height = 3, out.width = "90%", fig.align = 'center', fig.cap = "Two multivariate graphs illustrating how the second slot in a \\pkg{flexplot} formula controls the visualization. The left image demonstrates what happens to the second slot variable ($X2$) when a numeric predictor is on the X-axis, while the right image demonstrates what happens to the second slot variable when a categorical predictor is on the X-axis.\\label{fig:symbols}"----
a = flexplot(weight.loss ~ motivation + gender, 
    data = exercise_data, se = F, alpha = .3)
b = flexplot(weight.loss ~ therapy.type + gender, 
    data = exercise_data, se = F, alpha = .3)
cowplot::plot_grid(a, b)

## ---- message=FALSE, echo=TRUE, warning=FALSE----------------------------
a = flexplot(weight.loss ~ motivation | gender, 
             data = exercise_data)
b = flexplot(weight.loss ~ therapy.type | gender, 
             data = exercise_data)
c = flexplot(weight.loss ~ motivation |  gender + therapy.type, 
             data = exercise_data) +
      ggplot2::facet_grid(therapy.type ~ gender, 
              labeller = ggplot2::labeller(therapy.type = label_value)) 
#### edit point size
c = ggplot2::ggplot_build(c)
c$data[[1]]$size = .25
c = ggplot2::ggplot_gtable(c)


## ---- panels, message=FALSE, echo=FALSE, warning=FALSE, fig.align = "center", out.width = "90%", fig.cap = "A multivariate plot where \\code{therapy.type} and \\code{gender} are now shown in panels. \\label{fig:panels}", fig.width = 5, fig.height = 5----
		top.row = cowplot::plot_grid(a, b, ncol = 2)
		bottom.row = cowplot::plot_grid(NULL, c, NULL, ncol = 3, rel_widths=c(.25, .5, .25))
		cowplot::plot_grid(top.row, bottom.row, nrow = 2, rel_heights=c(.55, .45))
  

## ---- message=FALSE, echo=TRUE, warning=FALSE----------------------------
a = flexplot(weight.loss ~ motivation | satisfaction, 
             data = exercise_data, 
             breaks = list(satisfaction=c(3,7))) +
  ggplot2::facet_grid( ~ therapy.type, 
              labeller = ggplot2::labeller(therapy.type = label_value))
b = flexplot(weight.loss ~ motivation + satisfaction, 
             data = exercise_data, 
             breaks = list(satisfaction=c(3,7)), 
             labels = list(satisfaction=c("low", "medium", "high")))
c = flexplot(weight.loss ~ motivation + satisfaction, 
             data = exercise_data, 
             bins = 2) 


## ---- panels2, fig.align = "center", out.width = "90%",fig.height = 6, fig.width = 8,  fig.cap = "Plots reflecting choices of different break points (left-most plot), labels (right plot), and bins (bottom plot). \\label{fig:panels2}",message=FALSE, echo=FALSE, warning=FALSE----
  top.row = cowplot::plot_grid(a, b, ncol = 2, rel_widths=c(1,1.5))
  bottom.row = cowplot::plot_grid(NULL, c, NULL, ncol = 3, rel_widths=c(.2, .6,.2))
  cowplot::plot_grid(top.row, bottom.row, nrow = 2,rel_heights = c(.55, .45))

## ---- ghost, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Ghost lines repeat the pattern from one panel to the others, making it easier to compare across panels. \\label{fig:ghost}", out.width = "80%", fig.width = 5, fig.height = 3, fig.align = 'center'----
flexplot(weight.loss ~ motivation | satisfaction, 
             data = exercise_data, method = "lm", 
             bins = 3, ghost.line = "red")

## ---- threeway, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Multivariate relationship between five variables. Each \\pkg{flexplot} slot is occupied and it is difficult to interpret what is going on in the top figure, though the use of regression lines instead of loess lines, removing standard errors, reducing transparency of the datapoints, adding ghost lines, and reducing the number of bins have made it  easier to interpret (bottom image).  \\label{fig:threeway}", fig.width = 6, fig.height = 8----


a = flexplot(weight.loss ~ motivation + gender | satisfaction + health, 
    data = exercise_data) 
b = flexplot(weight.loss ~ motivation + gender | satisfaction + health, 
    data = exercise_data, 
    method = "lm", se = F, bins = 2, ghost.line = "black", alpha = .2,
    ghost.reference = list(satisfaction = 0, health = 10, gender = "male"))
cowplot::plot_grid(a, b, ncol = 1)

## ---- ancova, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Demonstration of the \\code{visualize} function on a \\code{lm} object. The top row shows two different representations of the statistical model. The bottom row shows diagnostic plots.  \\label{fig:ancova}", out.width = '80%', fig.width=8, fig.height=8----

model = lm(weight.loss ~ motivation + therapy.type, 
           data = exercise_data)
visualize(model)

## ---- ancova2, message=FALSE, echo=TRUE, warning=FALSE, fig.cap = "Demonstration of the visualize function on a \\code{lm} object, but with \\pkg{flexplot} arguments controlling the output (as well as suppressing residuals).  \\label{fig:ancova2}"----
model = lm(weight.loss ~ motivation + therapy.type, data = exercise_data)
visualize(model, formula = weight.loss ~ motivation | therapy.type, 
          ghost.line = "red", se = F, method = "lm", plot = "model")

