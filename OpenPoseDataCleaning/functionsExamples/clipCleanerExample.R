load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/clipCleaner.rda")

clipCleaner(read.path = TRUE,path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData/",
            extract.goodCLips = T,
            save.goodClips.path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/cleanRawCSV/")


