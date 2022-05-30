max.people<-function(path,max,full.folder,videos,files){
  if (full.folder==T) {
    list.files(path)->videos} else{
      
      read.csv(files)->videos
      videos[,1]->videos
      paste(videos,".csv",sep = "")->videos
    }
  
  
  
  dataVideos=NULL
  for (i in 1:length(videos)) {
    read.csv(paste(path,videos[i],sep = ""))->dataOneVideo
    if (max(dataOneVideo$people)<=max) {
      
      
      dataVideos<-rbind(dataVideos,dataOneVideo)
    }}
  
  return(dataVideos)
}