# The below function extracts data from the csv file

ExtractDreams <- function(){
  DreamExtract <- read.csv("C:/Kamal/R/projects/eyes2C/data/raw/DreamExtract.csv", header=FALSE)
  DreamExtract <- setNames(DreamExtract,c("Id","Name","Scene1","Scene2","Scene3","Scene4","Scene5","Created_dDate"))
  
  #DreamExtractList <- split(DreamExtract,seq(nrow(DreamExtract)))
  #DreamExtractList <- DreamExtractList[-1]
  return(DreamExtract)
}

