# `max.people`

This function reads `CSV` files generated by `dfmaker` and select data with a maximum amount of people detected. It must be improve, but right now is useful to select data where only one person has been detected by `OpenPose` which implies the speaker is who makes the gesticulation. The function can read a full folder or only some videos inside the folder when a `CSV` file with the names of wanted videos is provided. 

### Function atributes

max.people(path = "/home/user/dfMakerOutputCSV/", max = 2,full.folder = `TRUE`)

max.people(path = "/home/user/dfMakerOutputCSV/", max = 2,full.folder = `FALSE`,
  files = "/home/user/selectedFiles.csv")



