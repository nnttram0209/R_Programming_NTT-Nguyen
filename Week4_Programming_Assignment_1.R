getwd()
hospital_data <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/hospital-data.csv", head=TRUE)
outcome <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", head=TRUE)

#Khi thuc hien lenh as.numeric thi nhung cai nao kp so se chuyen thanh NA, trong bai nay la "Not Available" chuyen thanh NA
outcome[,c(11,17,23)] <- sapply(outcome[,c(11,17,23)],as.numeric)

#Tao bo data chi gom ten BV va cac outcome)
df <- outcome[,c(2,7,11,17,23)]
names(df) <- c("Hospital.Name", "State", "HA", "HF", "P")
df
# Tao function best
best <- function(state, outcome) {
  if (state %in% df$State) {
    if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      if (outcome == "heart attack") {
        df_final <- df[complete.cases(df$HA),]
        min_HA <- min(df_final[df_final$State == state,3])
        i <- which(df_final$State == state & df_final$HA == min_HA)
        print(df_final[i,1])
      }
      else if (outcome == "heart failure") {
        df_final <- df[complete.cases(df$HF),]
        min_HF <- min(df_final[df_final$State == state,4])
        j <- which(df_final$State == state & df_final$HF == min_HF)
        print(df_final[j,1])
      }
      else {
        df_final <- df[complete.cases(df$P),]
        min_P <- min(df_final[df_final$State == state,5])
        k <- which(df_final$State == state & df_final$P == min_P)
        print(df_final[k,1])
      }
    }
    else {
     print ("invalid outcome")
    }
  }
  else {
    print ("invalid state")
  }
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
