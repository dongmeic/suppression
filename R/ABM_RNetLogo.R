#install.packages('rJava')
Sys.setenv(NOAWT=1)
library(rJava)
library(RNetLogo)
nl_path <- "/Applications/NetLogo 6.0.1"
NLStart(file.path(nl_path, 'Java'), gui = TRUE, nl.jarname='netlogo-6.0.1.jar')
model_path <- file.path("models", "Sample Models", "Biology", "Wolf Sheep Predation.nlogo")
NLLoadModel(file.path(nl_path, model_path))
