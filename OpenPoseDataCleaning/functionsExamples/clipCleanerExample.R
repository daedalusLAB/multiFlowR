load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/clipCleaner.rda")

clipCleaner(read.path = TRUE,path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawCSV/",
            extract.goodCLips = T,
            save.goodClips.path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/cleanRawCSV/")


