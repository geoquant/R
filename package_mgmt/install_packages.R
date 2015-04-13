install.packages(c("AER",
                   "boot",
                   "car",
                   "copula",
                   "corpcor",
                   "corrplot",
                   "devtools",
                   "dtw",
                   "Ecdat",
                   "ellipse",
                   "evir",
                   "faraway",
                   "fCopulae",
                   "fEcofin",
                   "fGarch",
                   "FinTS",
                   "FitAR",
                   "foreach",
                   "forecast",
                   "ggplot2",
                   "lattice",
                   "leaps",
                   "lmtest",
                   "MASS",
                   "mnormt",
                   "nor1mix",
                   "numDeriv",
                   "pastecs",
                   "pcaPP",
                   "PerformanceAnalytics",
                   "plyr",
                   "quantmod",
                   "reshape",
                   "reshape2",
                   "rJava",
                   "robust",
                   "rugarch",
                   "tseries",
                   "urca",
                   "vars",
                   "xtable",
                   "xts",                 
                   "zoo"))

# R connection to Bloomberg
install.packages("Rbbg", repos="http://r.findata.org/", dependencies = TRUE)

# Install an old version of the sn package for CFRM 542 homework (Winter 2015)
install.packages("http://cran.r-project.org/src/contrib/Archive/sn/sn_1.0-0.tar.gz", 
                 repos = NULL, type = "source")


# Install Packages for CFRM 543
install.packages(c("ROI","ROI.plugin.glpk","ROI.plugin.quadprog"))
install.packages("mpo", repos = "http://r-forge.r-project.org")
install.packages("factorAnalytics", repos = "http://r-forge.r-project.org")
install.packages("PerformanceAnalytics", repos = "http://r-forge.r-project.org")

# Source:
# https://rmkrug.wordpress.com/2014/11/10/update-all-user-installed-r-packages-again/
#install.packages( 
#  lib  = lib <- .libPaths()[1],
#  pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package,
#  type = 'source'
#)