load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/maxPeople.rda")

example<-maxPeople(path = 
                    "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData/"
                   ,max = 1,full.folder = T)

head(example)

max(example$people)==1 # there is only videos with one person detected

unique(example$name)

###### Get only some csv from the folder


# Only the videos which names are inside filenames.txt are taken as input.

example<-maxPeople(path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData/"
                   ,max = 1,full.folder = F, fileNames = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/filenames.txt")



unique(example$name)

# In both cases the video 2014-10-10_0600_US_KCAL_Entertainment_Tonight_374-380_ID933from_beginning_to_end.csv is removed
# because it has more than one person

