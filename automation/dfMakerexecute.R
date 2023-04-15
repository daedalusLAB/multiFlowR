load("~/multiFlowR//dfMaker/functionsRData/dfMaker.rda")
load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/clipCleaner.rda")
load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/xyCorrector.rda")
load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/cramerOpenPose.rda")


args <- commandArgs(trailingOnly = TRUE)

args[1]->extra
args[2]->saveRaw
args[3]->rawFolder
args[4]->cleaner
args[5]->outPutFolder


if (args[2]==TRUE) {
  
  data<-dfMaker(input.folders = "videosJSON/",
                extra.var = extra,
                save.csv = saveRaw, output.folder =  rawFolder,
                return.empty = F)
                
  
}else{
  
  data<-dfMaker(input.folders = "videosJSON/",
                extra.var = extra,
                save.csv = saveRaw,
                return.empty = F)
  
}


if (args[4]==TRUE) {
  data<-clipCleaner(data = data,read.path = F,extract.goodCLips = F)
}




xyCorrector(df = data,df.full = T,set.NAs = T,fixed.point.x = 1,fixed.point.y = 1,reverse.y = T)->data

outPut<-cramerOpenPose(data = data,v.i = 5,orthonormal = T, save.video.csv = T,
                  output.folder = outPutFolder )


