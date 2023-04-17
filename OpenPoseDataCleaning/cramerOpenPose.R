cramerOpenPose<-function(data,v.i,v.j,orthonormal=T,save.video.csv,output.folder, save.parquet=F) {
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
    
    folder<-output.folder
    dir.create(folder,recursive = T, showWarnings = F)
    
    
    unique(processedData$name)->videoNames
    
    for (i in 1:length(videoNames)) {
      
      processedData[processedData$name==videoNames[i],]->oneVideoProcessedData
      write.csv(x = oneVideoProcessedData, paste(folder,"/",videoNames[i],".csv",sep = ""),row.names = F)
      
    }
    
  }
  
  
  
  if (save.parquet==TRUE) {
    
    require(arrow)
    folder<-output.folder
    dir.create(folder,recursive = T, showWarnings = F)
    
    unique(processedData$name)->videoNames
    
    
    for (i in 1:length(videoNames)) {
      
      processedData[processedData$name==videoNames[i],]->oneVideoProcessedData
      write_parquet(x = oneVideoProcessedData, sink = paste(folder,"/",videoNames[i],".parquet",sep = ""))
      
      
    }}
    
    
  
  
  return(processedData)
  
}


  







