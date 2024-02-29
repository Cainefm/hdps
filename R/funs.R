
#' The recurrence of features is assessed in a pre-exposure period, creating binary covariates based on a set of frequency-based cut-offs.The standard implementation of the HDPS defines three indicators for each patient capturing whether a feature was recorded:larger than once,larger than the median, and larger the75th percentile.
#'
#' @title covariate assessment
#' @param dt A database including the pid and covariates for assessment
#' @param id The column name of patient identification
#' @param code The column name of codes, eg. ICD-9, ICD-19, Drug Codes, or Lab results
#' @param type The prefix for output table, indicating type of data dimensions, eg. dx for diagnoses, px for procedures
#' @param rank The desired number of covariates to include in the model. eg. 500, 1000, Inf, etc
#'
#' @return A data.table including the recurrent assessment of the codes
#' @export
#'
rec_assess <- function(dt,id,code,type,rank=Inf){
    if (missing(dt) || !is.data.table(dt) || !is.data.frame(dt)) {
        stop("'dt' must be provided as a data.frame or data.table")
    }
    if (missing(id) || !is.character(id)) {
        stop("'id' must be the column name of identifier and provided as a character")
    }
    if (missing(code) || !is.character(code)) {
        stop("'code' must be the column name of codes and provided as a character")
    }
    if (missing(type) || !is.character(type)) {
        stop("'type' must be provided as a character")
    }
    if (!is.numeric(rank) || !is.infinite(rank)) {
        stop("'rank' must be provided as a number or Inf")
    }
    dt <- copy(dt)
    setDT(dt)
    setnames(dt,c(id,code),c("pid","code"))
    # catch
    if(!rank==Inf){
        count_cutoff <- dt[,.(count=.N),.(pid,code)][order(count,decreasing = T)][rank,count]
    }else(
        count_cutoff <- Inf
    )

    output <- dcast(
        melt(merge(dt[,.(count=.N),.(pid,code)][count <= count_cutoff],
                   dt[,.(count=.N),.(pid,code)][,.(Q1=quantile(count,0.25),
                                                   Q2=quantile(count,0.5),
                                                   Q3=quantile(count,0.75)),
                                                code
                                                ][,.(code,Q1,Q2=ifelse(Q1==Q2,NA,Q2),
                                                     Q3=ifelse(Q2==Q3,NA,Q3))],
                   by="code",
                   all.x=T)[,.(once=ifelse(count>=1,1,0),
                                 spor=ifelse(!is.na(Q2) & count>=Q2,1,ifelse(is.na(Q2),as.numeric(NA),0)),
                                 freq=ifelse(!is.na(Q3) & count>=Q3,1,ifelse(is.na(Q3),as.numeric(NA),0))),
                              .(pid,code)],
             id.vars = c("pid","code"))[!is.na(value)][,.(pid,code_type=paste(type,code,variable,sep="_"),count=value)]
        ,pid~code_type)
    output[is.na(output)] <- 0
    return(output)
}


#' Estimate the apparent relative risk for one covariate
#'
#' @param dt A dataset in data.table format, including
#' @param expo The column name for exposure or outcome
#' @param cova The column name for covariates
#' @param ...
#'
#' @return A data.table including the apparent relative risk for one covariate
#'
estBiasTable<-function(dt,expo,cova,...){
    temp<-dt[, .(count = .N), by = c(expo,cova)]
    setnames(temp,c(expo,cova),c("e","c"))
    temp <- merge(temp,
                  CJ(e=c(0,1),c=c(0,1)),by=c("e","c"),
                  all.y=T)
    temp[is.na(count), count := 0]
    return(temp)
}



#' ARR Bias estimation for all covariates
#'
#' @param hdpsCohort The dataset after recurrent assesemnt in data.table format
#' @param cova Column name of the desire covariates
#' @param expo Column name of the desire exposure
#' @param outc Column name of the desire outcome
#' @param correction When the outcome is rare, one of the 2 by 2 table could be zero. In this case, the algorithm will add 0.1 to the cell.
#' @param ...
#'
#' @return A data.table including the apparent relative risk for one covariates
#'
estBias <- function(hdpsCohort,cova,expo,outc,correction=TRUE,...){
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

    if(correction==TRUE){
        if(e0c1 ==0) e0c1 <-0.1
        if(e1c1 ==0) e1c1 <-0.1
        if(e0c0 ==0) e0c0 <-0.1
        if(e1c0 ==0) e1c0 <-0.1

        if(d0c1 ==0) d0c1 <-0.1
        if(d1c1 ==0) d1c1 <-0.1
        if(d0c0 ==0) d0c0 <-0.1
        if(d1c0 ==0) d1c0 <-0.1
    }

    pc1 <- e1c1 / e1
    pc0 <- ifelse(e0c1 / e0==0,0.1,e0c1 / e0)
    rrCE <- ifelse(pc1/pc0==0,NA,pc1/pc0)

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


#' @title Calculate the apparent relative risk for all covariates
#' Prioritise the large pool of covariates generated in the previous step is prioritised. This is typically achieved using the Bross formula, which uses univariate associations of covariates with treatment and outcome, to identify those with the highest potential tobias the treatment-outcome relationship
#' @param dt Dataset after reccurent assessment in data.table format
#' @param expo The column name for exposure
#' @param outc The column name for outcome
#' @param cova The column name for covariates included for ARR estimation. This can be omit if \emph{cova_exc} is used to include all columns except pre-defined columns.
#' @param cova_exc The column name for covariates excluded for ARR estimation if all columns except the defined columns.
#' @param correction When the outcome is rare, one of the 2 by 2 table could be zero. In this case, the algorithm will add 0.1 to the cell.
#'
#' @return A data.table including the apparent relative risk for all covariates
#' @export
#'
prioritize <- function(dt,expo,outc,cova,cova_exc,correction=TRUE){
    if(missing(cova)){
        if(missing(cova_exc)){
            stop("Please provide covarites included for the prioritizing~")
        }else{
            cova <- setdiff(colnames(dt),c(cova_exc))
        }

    }
    return(rbindlist(pbapply::pblapply(cova,
                                       function(x) estBias(dt,cova = x,
                                                           expo=expo,
                                                           outc=outc,
                                                           correction = correction))))
}

