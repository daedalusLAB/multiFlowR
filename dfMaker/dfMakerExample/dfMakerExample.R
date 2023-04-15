
load("~/multiFlowR/dfMaker/functionsRData/dfMaker.rda")

example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
        save.csv = T,
        output.folder = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData",
        return.empty = T,
        save.parquet = T,
        extra.var = T)

str(example)

example[2]

###############################################################################3
# Warning!!! The output.folder is inside OpenPoseDataCleaning to use data in cleaning functions #
##########################################################################

# In this case there is no empty frames, then the scond list is NULL

# We can work with data frame already in R

example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
                 save.csv = F,
                 output.folder = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawCSV",
                 return.empty = T,
                  extra.var = T)

str(example[[1]])


# Otherwise if the attribute `return.empty` is FALSE the object generate is a

# dataFrame instead a list


example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
                 save.csv = F,
                 output.folder = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawCSV",
                 return.empty = F,
                  extra.var = T)

str(example)

########## And now it is also posible to use dfMaker without the variables obtained from file names (`extra.var=F`)

dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
        return.empty = F,
        extra.var = F)
