library(jaspTools)
setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
options <- analysisOptions("~/Downloads/math_debug.jasp")
results <- runAnalysis("mixedmod_jasp", "~/Downloads/math.csv", options)
# 
# 
# library(jaspTools)
# setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
# options <- analysisOptions("~/Downloads/avengers.jasp")
# results <- runAnalysis("glinmod_jasp", "~/Downloads/avengers.csv", options)


library(jaspTools)
setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
options <- analysisOptions("~/Downloads/debug5.jasp")
results <- runAnalysis("linmod_jasp", "~/Downloads/debug.csv", options)
