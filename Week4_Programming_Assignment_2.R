getwd()
hospital_data <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/hospital-data.csv", head=TRUE)
outcome <- read.csv("C:/Users/ASUS/Documents/R_Programming_NTT-Nguyen/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", head=TRUE)
#Khi thuc hien lenh as.numeric thi nhung cai nao kp so se chuyen thanh NA, trong bai nay la "Not Available" chuyen thanh NA
outcome[,c(11,17,23)] <- sapply(outcome[,c(11,17,23)],as.numeric)

#Tao bo data chi gom ten BV va cac outcome va loai cac o NA)
df <- outcome[,c(2,7,11,17,23)]
names(df) <- c("Hospital.Name", "State", "HA", "HF", "P")

#Tao function RankHospital
#B1: Split du lieu theo State va rank tung outcome cho tung State
df_state_split <- split(df, df$State)
names(df_state_split)

#B2: Tao ham rankhospital
rankhospital <- function(state, outcome, rank) {
     if (state %in% names(df_state_split)) {
          if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
               df_state <- df_state_split[[state]]
               if (outcome == "heart attack") {
                    D <- df_state[complete.cases(df_state$HA),c(1,2,3)] #Sort lan luot theo thu tu tang dan HA, roi Hospital.Name
                    D_final <- D[with(D, order(HA, Hospital.Name)), ]
               }
               else if (outcome == "heart failure") {
                    D <- df_state[complete.cases(df_state$HF),c(1,2,4)]
                    D_final <- D[with(D, order(HF, Hospital.Name)), ]
               }
               else {
                    D <- df_state[complete.cases(df_state$P),c(1,2,5)]
                    D_final <- D[with(D, order(P, Hospital.Name)), ]
               }
                    
               
               ranking <- rank(D_final[,3], ties.method = "first")
               D_final$ranking <- ranking
               
               if (rank == "best") {
                    i <- which(D_final$ranking == min(D_final$ranking))
                    print(D_final[i,1])                              
               }
               else if (rank == "worst") {
                    i <- which(D_final$ranking == max(D_final$ranking))
                    print(D_final[i,1])                              
               }
               else if (rank <= max(D_final$ranking)) {
                    i <- which(D_final$ranking == rank)
                    print(D_final[i,1])
               }
               else {
                    print("NA")
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

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
