fast.mode<-function(fast.mode, path){
  start_time=NULL
  end_time =NULL
  start_time <- Sys.time()
  if(fast.mode==TRUE){
    read.csv(path)->dataVideos
  }else{
    list.files(path,pattern = ".csv")->videos ## video list
    dataVideos=NULL
    start_time <- Sys.time()
    for (i in 1:length(videos)) {
      read.csv(paste(path,videos[i],sep = ""))->data
      dataVideos<- rbind(dataVideos,data)
    }
  }
  end_time <- Sys.time()
  time<-end_time - start_time
  print(time)
  return(dataVideos)
}

# fast.mode(fast.mode = F,path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData/")->hola
