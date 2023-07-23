# # -----------------------------------------------------------------------------
# # PROGRAM NAME:  HDPS Prioritisation step
# # AUTHOR:        John Tazare
# # DATE CREATED:   03 Sept 2020
# # -----------------------------------------------------------------------------
#
# # Ranking confounders for potential for causing bias
#
#
# hdpsCohort <- hdpsCohort %>%
#     replace(is.na(.), 0)
#
# vars <- colnames(hdpsCohort[, !names(hdpsCohort) %in% c("patid", "indexdate", "exposed", "outcome")])
#
# e1 <- nrow(hdpsCohort %>% filter(exposed == 1 ))
# e0 <- nrow(hdpsCohort %>% filter(exposed == 0))
# d1 <- nrow(hdpsCohort %>% filter(outcome == 1))
# d0 <- nrow(hdpsCohort %>% filter(outcome == 0 ))
# n <- nrow(hdpsCohort)
#
# biasInfo <- data.frame(
#     code = as.character(),
#     e1 =  as.numeric(),
#     e0 =  as.numeric(),
#     d1 =  as.numeric(),
#     d0 =  as.numeric(),
#     c1 =  as.numeric(),
#     c0 =  as.numeric(),
#     e1c1 =  as.numeric(),
#     e0c1 =  as.numeric(),
#     e1c0 =  as.numeric(),
#     e0c0 =  as.numeric(),
#     d1c1 =  as.numeric(),
#     d0c1 =  as.numeric(),
#     d1c0 =  as.numeric(),
#     d0c0 =  as.numeric(),
#     pc1 =  as.numeric(),
#     pc0 =  as.numeric(),
#     rrCE =  as.numeric(),
#     rrCD =  as.numeric(),
#     bias =  as.numeric(),
#     absLogBias =  as.numeric(),
#     ceStrength =  as.numeric(),
#     cdStrength =  as.numeric()
# )
#
# counter <- 0
# for(v in vars){
#     counter <- counter + 1
#     if (counter %% 50 == 0) {
#         print(paste0("Started generation of bias information for code ", counter, ", out of ", length(vars) ))
#     }
#
#     invisible(capture.output (
#         tempFrameEx <- hdpsCohort %>%
#             dplyr::select(exposed, outcome,all_of(v) ) %>%
#             rename(code := paste(all_of(v))) %>%
#             group_by(exposed) %>%
#             summarise(sum = sum(code)) %>%
#             ungroup()
#     ))
#     c1 <- as.numeric(tempFrameEx[2,2])  + as.numeric(tempFrameEx[1,2])
#     c0 <- n - c1
#     e1c1 <- as.numeric(tempFrameEx[2, 2])
#     e0c1 <- as.numeric(tempFrameEx[1, 2])
#     e1c0 <- e1 - e1c1
#     e0c0 <- e0 - e0c1
#
#     invisible(capture.output(
#         tempFrameOut <- hdpsCohort %>%
#             dplyr::select(exposed, outcome,all_of(v) ) %>%
#             rename(code := paste(all_of(v))) %>%
#             group_by(outcome) %>%
#             summarise(sum = sum(code)) %>%
#             ungroup()
#     ))
#
#
#     d1c1 <- as.numeric(tempFrameOut[2, 2])
#     d0c1 <- as.numeric(tempFrameOut[1, 2])
#     d1c0 <- d1 - d1c1
#     d0c0 <- d0 - d0c1
#
#     pc1 <- e1c1 / e1
#     pc0 <- e0c1 / e0
#
#     rrCE <- pc1/pc0
#
#     if (rrCE == 0) {
#         rrCE <- NA
#     }
#
#
#     rrCD <- (d1c1/c1)/(d1c0/c0)
#
#     if (rrCD == 0) {
#         rrCD <- NA
#     }
#
#     bias <- (pc1*(rrCD - 1) + 1)/ (pc0*(rrCD - 1) + 1)
#
#     absLogBias <- abs(log(bias))
#
#     ce_strength <- abs(rrCE-1)
#     cd_strength <- abs(rrCD-1)
#
#
#     biasInfo <- biasInfo %>%
#         add_row(
#             code := paste(all_of(v)),
#             e1 =  e1,
#             e0 =  e0,
#             d1 =  d1,
#             d0 =  d0,
#             c1 =  c1,
#             c0 =  c0,
#             e1c1 =  e1c1,
#             e0c1 = e0c1,
#             e1c0 = e1c0,
#             e0c0 = e0c0,
#             d1c1 = d1c1,
#             d0c1 = d0c1,
#             d1c0 = d1c0,
#             d0c0 = d0c0,
#             pc1 =  pc1,
#             pc0 =  pc0,
#             rrCE = rrCE,
#             rrCD = rrCD,
#             bias = bias ,
#             absLogBias =  absLogBias,
#             ceStrength =  ce_strength,
#             cdStrength =  cd_strength
#         )
#
#
# }
#
# # Rank covariates
# biasInfo1 <- biasInfo %>%
#     arrange(-absLogBias) %>%
#     mutate(rank = row_number())
#
# top250 <- biasInfo1 %>%
#     filter(rank<=250)
#
# top500 <- biasInfo1 %>%
#     filter(rank<=500)
#
# top750 <- biasInfo1 %>%
#     filter(rank<=750)
#
# top900 <- biasInfo1 %>%
#     filter(rank<=900)
#
# top1000 <- biasInfo1 %>%
#     filter(rank<=1000)
#
# selectedVars250 <- c(top250[[1]], "patid", "indexdate")
# names250 <- names(hdpsCohort)[(names(hdpsCohort) %in% selectedVars250)]
# cohort250 <- hdpsCohort[, names250]
#
# selectedVars500 <- c(top500[[1]], "patid", "indexdate")
# names500 <- names(hdpsCohort)[(names(hdpsCohort) %in% selectedVars500)]
# cohort500 <- hdpsCohort[, names500]
#
# selectedVars750 <- c(top750[[1]], "patid", "indexdate")
# names750 <- names(hdpsCohort)[(names(hdpsCohort) %in% selectedVars750)]
# cohort750 <- hdpsCohort[, names750]
#
# selectedVars900 <- c(top900[[1]], "patid", "indexdate")
# names900 <- names(hdpsCohort)[(names(hdpsCohort) %in% selectedVars900)]
# cohort900 <- hdpsCohort[, names900]
#
# selectedVars1000 <- c(top1000[[1]], "patid", "indexdate")
# names1000 <- names(hdpsCohort)[(names(hdpsCohort) %in% selectedVars1000)]
# cohort1000 <- hdpsCohort[, names1000]
