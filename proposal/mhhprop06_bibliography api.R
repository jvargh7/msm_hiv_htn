rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/preprocessing/bibliography_api.R")

require(xml2)
cited <- readLines(paste0(path_msm_hiv_htn_proposal,"/writing/05 Bibliography and References Cited/cited_submission.txt"))

cited_updated <- bibliography_api(cited)


writeLines(cited_updated,paste0(path_msm_hiv_htn_proposal,"/writing/05 Bibliography and References Cited/cited_updated.txt"))

