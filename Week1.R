getwd()
myfile <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/hw1_data.csv", header = TRUE)
data.matrix(myfile)
as.matrix(myfile)
  myfile[1:2,] #Q12 
  
  nrow(myfile) #Q13
  
  myfile[c(152,153),] #Q14
  
  myfile[47,'Ozone'] #Q15
  
  OzoneCol <- myfile[,'Ozone'] #Q16 (This is a vector because we subsetted one element)
  CheckNA <- is.na(OzoneCol) 
  length(CheckNA[CheckNA == TRUE])
  
  mean(OzoneCol[CheckNA == FALSE]) #Q17
  
  TempCol <- myfile[,'Temp'] #Q18
  Q18 <- myfile[OzoneCol>31 & TempCol>90,]
  Q18
  Solar.R <- Q18[,'Solar.R']
  mean(Solar.R[is.na(Solar.R) == FALSE])
  
  MonthCol <- myfile[,'Month'] #Q19
  mean(TempCol[MonthCol == 6])
  
  max(OzoneCol[CheckNA == FALSE & MonthCol == 5]) #Q20
  

