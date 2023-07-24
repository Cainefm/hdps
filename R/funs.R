
feature_filter <- function(dt,id,code,type,rank=NA){
    dt <- copy(dt)
    setDT(dt)
    setnames(dt,c(id,code),c("pid","code"))
    output <- dcast(
        melt(merge(dt[,.(count=.N),.(pid,code)],
                   dt[,.(count=.N),.(pid,code)][,.(Q2=quantile(count,0.5),Q3=quantile(count,probs = 0.75)),code][,Q3:=ifelse(Q2==Q3,NA,Q3)],
                   by="code")[,.(once=ifelse(count>=1,1,0),
                                 spor=ifelse(!is.na(Q2) & count>=Q2,1,0),
                                 freq=ifelse(!is.na(Q3) & count>=Q3,1,0)),.(pid,code)],id.vars = c("pid","code"))[,.(pid,code_type=paste(type,code,variable,sep="_"),count=value)]
        ,pid~code_type)
    output[is.na(output)] <- 0
    return(output)
}


estBiasTable<-function(dt,var1,cova,...){
    temp<-dt[, .(count = .N), by = c(var1,cova)]
    setnames(temp,c(var1,cova),c("e","c"))
    temp <- merge(temp,
                  CJ(e=c(0,1),c=c(0,1)),by=c("e","c"),
                  all.y=T)
    temp[is.na(count), count := 0]
    return(temp)
}

estBias <- function(hdpsCohort,cova,expo,outc,...){
    setDT(hdpsCohort)
    e1 <- hdpsCohort[get(expo)==1,.N]
    e0 <- hdpsCohort[get(expo)==0,.N]
    d1 <- hdpsCohort[get(outc)==1,.N]
    d0 <- hdpsCohort[get(outc)==0,.N]
    n <- hdpsCohort[,.N]

    temp <- estBiasTable(hdpsCohort,expo,cova)
    c1 <- temp[c==1,sum(count)]
    c0 <- n - c1
    e0c1 <- temp[e == 0 & c == 1, count]
    e1c1 <- temp[e == 1 & c == 1, count]
    e0c0 <- temp[e == 0 & c == 0, count]
    e1c0 <- temp[e == 1 & c == 0, count]

    temp <- estBiasTable(hdpsCohort,outc,cova)
    d0c1 <- temp[e == 0 & c == 1, count]
    d1c1 <- temp[e == 1 & c == 1, count]
    d0c0 <- temp[e == 0 & c == 0, count]
    d1c0 <- temp[e == 1 & c == 0, count]

    pc1 <- e1c1 / e1
    pc0 <- ifelse(e0c1 / e0==0,0.1,e0c1 / e0)
    rrCE <- ifelse(pc1/pc0==0,NA,pc1/pc0)

    if(c1==0){c1<-0.1}
    rrCD <- ifelse((d1c1/c1)/(d1c0/c0)==0,NA,(d1c1/c1)/(d1c0/c0))

    bias <- (pc1*(rrCD - 1) + 1)/ (pc0*(rrCD - 1) + 1)
    absLogBias <- abs(log(bias))

    ce_strength <- abs(rrCE-1)
    cd_strength <- abs(rrCD-1)
    temp <- data.table(code = paste(cova),
                       e1,e0,d1,d0,c1,c0,
                       e1c1,e0c1,e1c0,e0c0,
                       d1c1,d0c1,d1c0,d0c0,
                       pc1,pc0,
                       rrCE,rrCD,
                       bias,absLogBias,
                       ce_strength,cd_strength)
    return(temp)
}
# estBias(hdpsCohort,"dx_335.20_once","exposed","outcome")
# estBias(hdpsCohort,"dx_000_freq","exposed","outcome")

prioritize <- function(dt,type="dx",expo,outc){
    return(rbindlist(pbapply::pblapply(grep(type,colnames(dt),value = T),
                                       function(x) estBias(dt,cova = x,
                                                           expo=expo,
                                                           outc=outc))))
}

#prioritize(hdpsCohort,"dx","exposed","outcome")

#
# feature_filter(dx,"id","all.diagnosis.code.icd9.","dx")
#
# saveRDS(feature_filter(dx,"id","all.diagnosis.code.icd9.","dx"),"~/Documents/abc.rds")
#
# prioritation <- function(hdps){
#
#     hdpsCohort <- hdpsCohort %>%
#         replace(is.na(.), 0)
#
#     vars <- colnames(hdpsCohort[, !names(hdpsCohort) %in% c("patid", "indexdate", "exposed", "outcome")])
#
#     e1 <- nrow(hdpsCohort %>% filter(exposed == 1 ))
#     e0 <- nrow(hdpsCohort %>% filter(exposed == 0))
#     d1 <- nrow(hdpsCohort %>% filter(outcome == 1))
#     d0 <- nrow(hdpsCohort %>% filter(outcome == 0 ))
#     n <- nrow(hdpsCohort)
#
#     biasInfo <- data.frame(
#         code = as.character(),
#         e1 =  as.numeric(),
#         e0 =  as.numeric(),
#         d1 =  as.numeric(),
#         d0 =  as.numeric(),
#         c1 =  as.numeric(),
#         c0 =  as.numeric(),
#         e1c1 =  as.numeric(),
#         e0c1 =  as.numeric(),
#         e1c0 =  as.numeric(),
#         e0c0 =  as.numeric(),
#         d1c1 =  as.numeric(),
#         d0c1 =  as.numeric(),
#         d1c0 =  as.numeric(),
#         d0c0 =  as.numeric(),
#         pc1 =  as.numeric(),
#         pc0 =  as.numeric(),
#         rrCE =  as.numeric(),
#         rrCD =  as.numeric(),
#         bias =  as.numeric(),
#         absLogBias =  as.numeric(),
#         ceStrength =  as.numeric(),
#         cdStrength =  as.numeric()
#     )
#
#     counter <- 0
#     for(v in vars){
#         counter <- counter + 1
#         if (counter %% 50 == 0) {
#             print(paste0("Started generation of bias information for code ", counter, ", out of ", length(vars) ))
#         }
#
#         invisible(capture.output (
#             tempFrameEx <- hdpsCohort %>%
#                 dplyr::select(exposed, outcome,all_of(v) ) %>%
#                 rename(code := paste(all_of(v))) %>%
#                 group_by(exposed) %>%
#                 summarise(sum = sum(code)) %>%
#                 ungroup()
#         ))
#         c1 <- as.numeric(tempFrameEx[2,2])  + as.numeric(tempFrameEx[1,2])
#         c0 <- n - c1
#         e1c1 <- as.numeric(tempFrameEx[2, 2])
#         e0c1 <- as.numeric(tempFrameEx[1, 2])
#         e1c0 <- e1 - e1c1
#         e0c0 <- e0 - e0c1
#
#         invisible(capture.output(
#             tempFrameOut <- hdpsCohort %>%
#                 dplyr::select(exposed, outcome,all_of(v) ) %>%
#                 rename(code := paste(all_of(v))) %>%
#                 group_by(outcome) %>%
#                 summarise(sum = sum(code)) %>%
#                 ungroup()
#         ))
#
#
#         d1c1 <- as.numeric(tempFrameOut[2, 2])
#         d0c1 <- as.numeric(tempFrameOut[1, 2])
#         d1c0 <- d1 - d1c1
#         d0c0 <- d0 - d0c1
#
#         pc1 <- e1c1 / e1
#         pc0 <- e0c1 / e0
#
#         rrCE <- pc1/pc0
#
#         if (rrCE == 0) {
#             rrCE <- NA
#         }
#
#
#         rrCD <- (d1c1/c1)/(d1c0/c0)
#
#         if (rrCD == 0) {
#             rrCD <- NA
#         }
#
#         bias <- (pc1*(rrCD - 1) + 1)/ (pc0*(rrCD - 1) + 1)
#
#         absLogBias <- abs(log(bias))
#
#         ce_strength <- abs(rrCE-1)
#         cd_strength <- abs(rrCD-1)
#
#
#         biasInfo <- biasInfo %>%
#             add_row(
#                 code := paste(all_of(v)),
#                 e1 =  e1,
#                 e0 =  e0,
#                 d1 =  d1,
#                 d0 =  d0,
#                 c1 =  c1,
#                 c0 =  c0,
#                 e1c1 =  e1c1,
#                 e0c1 = e0c1,
#                 e1c0 = e1c0,
#                 e0c0 = e0c0,
#                 d1c1 = d1c1,
#                 d0c1 = d0c1,
#                 d1c0 = d1c0,
#                 d0c0 = d0c0,
#                 pc1 =  pc1,
#                 pc0 =  pc0,
#                 rrCE = rrCE,
#                 rrCD = rrCD,
#                 bias = bias ,
#                 absLogBias =  absLogBias,
#                 ceStrength =  ce_strength,
#                 cdStrength =  cd_strength
#             )
#
#
#     }
# }

