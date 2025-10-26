pkgname <- "hdps"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('hdps')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dx")
### * dx

flush(stderr()); flush(stdout())

### Name: dx
### Title: Example Diagnosis Data
### Aliases: dx
### Keywords: datasets

### ** Examples

data(dx)
head(dx)
str(dx)



cleanEx()
nameEx("master")
### * master

flush(stderr()); flush(stdout())

### Name: master
### Title: Example Master Table with Exposure and Outcome
### Aliases: master
### Keywords: datasets

### ** Examples

data(master)
head(master)
str(master)
table(master$exposure)
table(master$outcome)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
