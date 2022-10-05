clipCleaner<- function(path,extract.goodCLips, save.goodClips.path){
  
  load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/maxPeople.rda") ## loading function
  maxPeople(path = path , max = 2,full.folder = TRUE)->data
  data[data$typePoint=="pose_keypoints",]->dataPose # only pose_keypoints
  dataPose[dataPose$people==2,]->data2Pose # second ID detected
  
  c(unique(dataPose$name),unique(data2Pose$name))->names 
  onePerson=NULL
  for (i in 1:length(names)) {
    if(sum(names[i]==names)==1){
      onePerson<-c(onePerson,names[i])
    }
  } # only one ID person detected
  
  
  if (nrow(data2Pose)==0) {
    data->dataGood
    print("There is no misdetected points")
    if (extract.goodCLips==T) {
      for (i in 1:length(onePerson)) {
        read.csv(paste(path,onePerson[i],".csv",sep = ""))->dataOneClip
        dir.create(showWarnings = F, path = save.goodClips.path,recursive = T)
        
        write.csv(dataOneClip,
                  file = paste(save.goodClips.path, onePerson[i],".csv",sep = "")
                  ,row.names = F)
        
      }}}else{
  aggregate(data2Pose[2],
            by=list(point=as.factor(data2Pose$point),name=as.factor(data2Pose$name)),
            FUN = sum)->secondPerson # sum of all x coord. values 
  
  
  
  ## if sum is equal to 0 for the point number 1 (chest) then there is no a chest
  ## detected in the clip: x==0 & c(point==1)
  secondPerson[secondPerson$point==1 & secondPerson$x==0,]->floatingHeads
  
  unique(floatingHeads$name)->rescuedVideos #  no chest for second ID videos
  
  c(onePerson,as.character(rescuedVideos))->goodVideos#1 ID & 2 ID no chest videos
  
  dataPose[dataPose$name%in%goodVideos,]->dataGood # clean data frame 
  
  if (extract.goodCLips==T) {
    
    for (i in 1:length(goodVideos)) {
      read.csv(paste(path,goodVideos[i],".csv",sep = ""))->dataOneClip
      dir.create(showWarnings = F, path = save.goodClips.path,recursive = T)
      
      write.csv(dataOneClip,
                file = paste(save.goodClips.path, goodVideos[i],".csv",sep = "")
                ,row.names = F)
      
    }}
  
      }
  return(dataGood)
}





