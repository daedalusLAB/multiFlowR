xyCorrector<-function(df,df.full,set.NAs=T,fixed.point.x,fixed.point.y,reverse.y=T,set.x=T,set.y=T) {
  
  if (df.full==T) {
    
    df[df$typePoint=="pose_keypoints",]->dfpose
  }else{
    dfpose<-df
  }
  
  if (set.NAs==T) {
    
    dfpose$x[dfpose$x == 0] <- NA
    
    dfpose$y[dfpose$y == 0] <- NA
  }
  
  
  
  
  
  
  frame<-(nrow(dfpose)/25)-1
  
  x2<-c()
  y2<-c()
  
  
  
  for (i in 0:frame) {
    
    first<- 25*i+1
    
    pose<-(dfpose[first:(first+24),])
    
    
    
    if(set.x==T){
      m<-pose$x-pose[pose$point==fixed.point.x,"x"]
      
      x2<-c(x2,m)}
    
    if (reverse.y==T &set.y==F) {
      n<-max(pose$y,na.rm = T)- pose$y
      
      y2<-c(y2,n)
      
    }
    
    
    if(set.y==T& reverse.y==F){
      o<-pose$y-pose[pose$point==fixed.point.y,"y"]
      
      y2<-c(y2,o)}
    
    
    
    if(set.y==T& reverse.y==T){
      
      n<-max(pose$y,na.rm = T)- pose$y
      o<-n-n[fixed.point.y+1]
      y2<-c(y2,o)
      
      
      
      
      
    }}
  
  
  
  if (set.y==T & set.x==T | reverse.y==T & set.x==T ) {
    newdf<-cbind(dfpose,x2,y2)
    return(newdf)
  }else{
    if(set.y==F & reverse.y==F){
      newdf<-cbind(dfpose,x2)
    }else
      if (set.x==F) {
        newdf<-cbind(dfpose,y2)
      }
  }
  
  
  
  
  
}

