
load("~/multiFlowR/dfMaker/functionsRData/dfMaker.rda")

example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
        save.csv = T,
        output.folder = "~/multiFlowR/dfMaker/dfMakerExample/resultCSV",
        return.empty = T)

str(example)


# In this case there is no empty frames, then the scond list is NULL

# We can work with data frame already in R

example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
                 save.csv = F,
                 output.folder = "~/multiFlowR/dfMaker/dfMakerExample/resultCSV",
                 return.empty = T)

str(example[[1]])


# Otherwise if the atribute `return.empty` is FALSE the object generate is a

# dataFrame insted a list


example<-dfMaker(input.folders = "~/multiFlowR/dfMaker/dfMakerExample/exampleVideos/",
                 save.csv = F,
                 output.folder = "~/multiFlowR/dfMaker/dfMakerExample/resultCSV",
                 return.empty = F)

str(example)
