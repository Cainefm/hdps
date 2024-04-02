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


roxygen2::roxygenize()
# 升级版本号
usethis::use_version()

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








library(data.table)
library(hdps)
dx <- read.csv("/Users/fanmin/Desktop/new job/to office PC/MND dummy dataset/seperated files/dummy_dx.csv")
dx <- read.csv("D:/OneDrive - connect.hku.hk/Projects/MND Project/9.dummy data for trial/dummy_dx.csv")
ip <- as.data.table(read.csv("D:/OneDrive - connect.hku.hk/Projects/MND Project/9.dummy data for trial/dummy_ip.csv"))

setDT(dx)
dx <- dx[,.(id=id,icd9code=all.diagnosis.code.icd9.)][,pid:=.GRP,id][,.(pid,icd9code)]
master <- dx[,.(pid=unique(pid),outcome=sample(c(0,1),491,replace = T),exposure=sample(c(0,1),491,replace = T))]

hdpsCohort <- rec_assess(dx,"pid",code = "icd9code",type = "dx")
hdpsCohort[1:5,1:6]

hdpsCohort<- merge(master,hdpsCohort,by="pid")
hdpsCohort[1:5,1:5]

hdpsResult <- prioritize(hdpsCohort,
                         cova_exc = c("id","exposure","outcome"),
                         expo = "exposure",outc = "outcome")

library(ggplot2)
ggplot(data=hdpsResult)+
    geom_point(aes(rrCE,rrCD))+
    theme_classic()

library(plotly)
p <- ggplot(data=hdpsResult,
            aes(text=code, x = rrCD, y = rrCE)) +
    geom_point() +
    theme_bw()

ggplotly(p)

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

library(matchit)
m.ps <- glm(exposure~.,data = hdpsCohort, family="binomial")
test <- copy(hdpsCohort)
test$pr_score <- predict(m.ps,type="response")
ggplot(test)+
  geom_histogram(aes(x=pr_score, y=..density..,), color="white", bins=30)+
  scale_x_continuous(limits = c(0.1,0.9))
  geom_density(aes(x=pr_score))+
  facet_wrap(~exposure, ncol=1)

ggplot(test)+
  geom_histogram(aes(x=pr_score, y=..density.., fill=exposure),
                 color="white", bins=30, position="identity", alpha=.3)+
  scale_x_continuous(limits = c(0.1,0.9))+
  geom_density(aes(x=pr_score, color=exposure))

plot(density(df$pscore[df$Near==1]))
plot(density(df$pscore[df$Near==0]))


library(data.table)
library(hdps)
# dx <- read.csv("/Users/fanmin/Desktop/new job/to office PC/MND dummy dataset/seperated files/dummy_dx.csv")
# dx <- read.csv("D:/OneDrive - connect.hku.hk/Projects/MND Project/9.dummy data for trial/dummy_dx.csv")
dx <- readRDS("M:/Personal/Edmund Cheung/HDPS/cohort_all_rec_arb.rds")
# ip <- as.data.table(read.csv("D:/OneDrive - connect.hku.hk/Projects/MND Project/9.dummy data for trial/dummy_ip.csv"))

setDT(dx)
# dx <- dx[,.(id=id,icd9code=all.diagnosis.code.icd9.)][,pid:=.GRP,id][,.(pid,icd9code)]
# master <- dx[,.(pid=unique(pid),outcome=sample(c(0,1),491,replace = T),exposure=sample(c(0,1),491,replace = T))]
#
# hdpsCohort <- rec_assess(dx,"patient_pssn",code = "icpc",type = "dx")
# hdpsCohort[1:5,1:6]
#
# # remove the stupid columsn in pacakge
dx <- dx[,`:=`(class=NULL,fu=NULL)]

prioritize(dx,pid="patient_pssn",expo = "exposed",outc = "outcome")


