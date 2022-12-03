# library(jaspTools)
# setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
# options <- analysisOptions("~/Downloads/avengers.jasp")
# results <- runAnalysis("flexplot_jasp2", "~/Downloads/avengers.csv", options)
# 
# 
# library(jaspTools)
# setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
# options <- analysisOptions("~/Downloads/avengers.jasp")
# results <- runAnalysis("glinmod_jasp", "~/Downloads/avengers.csv", options)


library(jaspTools)
setPkgOption("module.dirs", "~/Documents/RPackages/flexplot")
options <- analysisOptions("~/Downloads/avengers.jasp")
results <- runAnalysis("mixedmod_jasp", "~/Downloads/avengers.csv", options)