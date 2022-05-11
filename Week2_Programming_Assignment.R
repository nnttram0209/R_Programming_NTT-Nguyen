#Gop file: An cap source code
setwd("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_specdata/specdata")
all_file_names <- list.files(pattern = "*.csv")

df_list <- lapply(all_file_names, read.table, skip = 1, sep = ",")
df_final <- do.call(rbind, df_list)

header <- readLines(all_file_names[1], n=1)
header <- strsplit(header, ",")

names(df_final) <- header[[1]]


sulfate <- df_final[ ,2]
nitrate <- df_final[ ,3]
id <- df_final[ ,4]
mean(sulfate[id %in% 1:2], na.rm = TRUE)

#Part2
df_final_cc <- df_final[complete.cases(df_final),]
id_cc <- df_final_cc[,4]


complete <- function(directory, id_input) {
  if (directory == "specdata") {
      cc_result = data.frame()
      for (i in id_input) {
        count_caseperid <- length(id_cc[id_cc == i])
        df <- data.frame(i, count_caseperid)
        cc_result <- rbind(cc_result,df)
      }
      names(cc_result) <- c("id", "nobs")
      print(cc_result)
  }
  
  else {
    print ("Folder not found")
  }
}

complete("specdata", 1:5)


#Part1
pollutantmean <- function(directory, pollutant, id_input = 1:332) {
  if (directory == "specdata") {
    if (pollutant == "sulfate") {
      print(mean(sulfate[id %in% id_input], na.rm = TRUE))
    }
    else if (pollutant == "nitrate") {
      print(mean(nitrate[id %in% id_input], na.rm = TRUE))
    }
    else {
      print("Pollutant name is incorrect")
    }
  }
  else {
    print("Folder not found")
  }
}


