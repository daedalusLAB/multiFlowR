cramerOpenPose<-function(data,v.i,v.j,orthonormal=T,save.video.csv,path.save.videos) {
  load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/cramerOpenPoseFrame.rda")
  
  matriXY<-NULL
  frame<-length(data$x)/25-1
  for (f in 0:frame) {
    
    if (orthonormal==T) {
      
      n<-cramerOpenPoseFrame(x=data$x2[(25*f+1):(25*f+1+24)],y = data$y2[(25*f+1):(25*f+1+24)],v.i =v.i, orthonormal = T)
      
    }else{
      n<-cramerOpenPoseFrame(x=data$x2[(25*f+1):(25*f+1+24)],y = data$y2[(25*f+1):(25*f+1+24)],v.i =v.i, orthonormal = F, v.j = v.j)
    }
    
    matriXY<-rbind(matriXY,n)
    
  }
  colnames(matriXY)<-c("nx","ny")
  
  
  cbind(data,matriXY)->processedData
  
  if (save.video.csv==T) {
    
    unique(processedData$name)->videoNames
    
    processedData[processedData$name==videoNames[1],]->oneVideoProcessedData
    
    dir.create(path = path.save.videos,recursive = T,showWarnings = F)
    
    
    
    
    for (i in 1:length(videoNames)) {
      
      processedData[processedData$name==videoNames[i],]->oneVideoProcessedData
      csvFileName <- paste(path.save.videos,videoNames[i],".csv",sep="")
      write.csv(x = oneVideoProcessedData, file = csvFileName, row.names = F,)
    }
    
  }
  
  return(processedData)
  
}
