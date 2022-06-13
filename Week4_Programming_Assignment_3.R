getwd()
hospital_data <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/hospital-data.csv", head=TRUE)
outcome <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", head=TRUE)
#Khi thuc hien lenh as.numeric thi nhung cai nao kp so se chuyen thanh NA, trong bai nay la "Not Available" chuyen thanh NA
outcome[,c(11,17,23)] <- sapply(outcome[,c(11,17,23)],as.numeric)

#Tao bo data chi gom ten BV va cac outcome va loai cac o NA)
df <- outcome[,c(2,7,11,17,23)]
names(df) <- c("Hospital.Name", "State", "HA", "HF", "P")

# Tao ham rankall
rankall <- function(outcome, rank) {
     if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
          if (outcome == "heart attack") {
               D <- df[complete.cases(df$HA),c(1,2,3)] #Sort lan luot theo thu tu tang dan HA, roi Hospital.Name
          }
          else if (outcome == "heart failure") {
               D <- df[complete.cases(df$HF),c(1,2,4)]
          }
          else {
               D <- df[complete.cases(df$P),c(1,2,5)]
          }
          names(D) <- c("Hospital.Name", "State", "Outcome")
          
          result_final <- data.frame()
          U <- unique(D$State)
          unique_State <- U[order(U)]
          for (state in unique_State) {
               D_temp <- D[D$State == state,]
               D_final <- D_temp[with(D_temp, order(Outcome, Hospital.Name)), ]
               ranking <- rank(D_final[,3], ties.method = "first")
               D_final$ranking <- ranking
               
               if (rank == "best") {
                    i <- which(D_final$ranking == min(D_final$ranking))
                    hospital <- D_final[i,1]                             
               }
               else if (rank == "worst") {
                    i <- which(D_final$ranking == max(D_final$ranking))
                    hospital <- D_final[i,1]                             
               }
               else if (rank <= max(D_final$ranking)) {
                    i <- which(D_final$ranking == rank)
                    hospital <- D_final[i,1]
               }
               else {
                    hospital <- NA
               }
               result <- data.frame(hospital, state)
               result_final <- rbind(result_final, result)
          }
          return(result_final)
     }
     else {
          print("invalid outcome")
     }
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
