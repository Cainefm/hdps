library(usethis)
#create pakcage from Rstudio
# add github
# use usethis to create lincense
# use renv to control the package versions
# renv::init()
# renv::snapshot()
# renv::install()
# renv::update()
use_mit_license()
devtools::check()


# devtools::build()


# create manuals
# usethis::use_vignette("my-vignette")
usethis::use_readme_rmd()

usethis::use_readme_rmd()

icd9dx <- readRDS("inst/extdata//icd9.rds")

head(icd9dx)

sample(1:1000)
sample()


#Prevalence(how many people are with this code within all participants, range 0-1)



# 升级版本号
usethis::use_version()




library(data.table)
dx <- read.csv("/Users/fanmin/Desktop/new job/to office PC/MND dummy dataset/seperated files/dummy_dx.csv")
setDT(dx)
dx <- dx[,.(id,value=all.diagnosis.code.icd9.)]
dx[,.(prec=uniqueN(id)/.N),value]
dx[,.(count=.N),.(id,value)]

dx[,.(count=.N),.(id,value)][,.(Q2=quantile(count,0.5),Q3=quantile(count,probs = 0.75)),value][,Q3:=ifelse(Q2==Q3,NA,Q3)][]


dcast(melt(merge(dx[,.(count=.N),.(id,value)],
                 dx[,.(count=.N),.(id,value)][,.(Q2=quantile(count,0.5),Q3=quantile(count,probs = 0.75)),value][,Q3:=ifelse(Q2==Q3,NA,Q3)],
                 by="value")[,.(once=ifelse(count>=1,1,0),
                                spor=ifelse(!is.na(Q2) & count>=Q2,1,0),
                                freq=ifelse(!is.na(Q3) & count>=Q3,1,0)),.(id,value)],id.vars = c("id","value"))[,.(id,value_type=paste(value,variable,sep="_"),value=value.1)],
      id~value_type)

