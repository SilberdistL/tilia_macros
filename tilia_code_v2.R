#######Little script to transform macrofossil counts in csv file suitable for tilia software#####

#set working directory
setwd("~/Desktop/PhD/Lai_da_Vons/macros")

dm<- data.frame(read.csv("macro_count_2022_2.csv", sep=";")) #import dfset
#structure of the dfset is : 
#colnames : Core name
#line1 : top depth
#line2 : bottom depth
#line3 : sample size

#col1: taxa names


#Normalize sample for 12ml
dm[dm == ""] <- 0
for (i in 2:ncol(dm)) for (j in 5:nrow(dm)) {
  dm[j,i] <- (as.numeric(dm[j,i])*12)/(as.numeric(dm[3,i]))
} #Change here (the "12") to your desired size (mL)
dm[is.na(dm)] <- 0
#create several columns per sample
for (i in 2:ncol(dm)) {
  col1 <- data.frame(dm[,i])
  colnames(col1) <- (as.numeric(dm[1,i])+0.0001)
  col2 <- data.frame(dm[,i])
  colnames(col2) <- (as.numeric(dm[2,i])-0.0001)
  dm$new <- data.frame(rep(0, nrow(dm)))
  colnames(dm$new) <- 0

      #continuous samples
     if (as.numeric(dm[2,i]) == as.numeric(dm[1, i + 1])) {
    assign(paste0("sample_",i), cbind(col1, col2))
   
      #not continuous samples (add 2 zero columns)
        } else {
    assign(paste0("sample_",i), cbind(col1, col2, dm$new, dm$new))
    
}}

#get the list of samples created
a <- 2:(ncol(dm)-1)
df <- (paste0("sample_", a))
sample_x <- paste(df, collapse = ", ") 
print(sample_x) #copy the result into the cbind function

# CAREFUL : keep dm$new at the beginning and the end = zero column. z.B : df <- cbind(dm$new, sample X, sample Y, etc..., dm$new)
df <- cbind(dm$new, sample_2, sample_3, sample_4, sample_5, sample_6, sample_7, sample_8, sample_9, sample_10, sample_11, sample_12, sample_13, sample_14, sample_15, sample_16, sample_17, sample_18, sample_19, sample_20, sample_21, sample_22, sample_23, sample_24, sample_25, sample_26, sample_27, sample_28, sample_29, sample_30, sample_31, sample_32, sample_33, sample_34, sample_35, sample_36, sample_37, sample_38, sample_39, sample_40, sample_41, sample_42, sample_43, sample_44, sample_45, sample_46, sample_47, sample_48, sample_49, sample_50, sample_51, sample_52, sample_53, sample_54, sample_55, sample_56, sample_57, sample_58, sample_59, sample_60, sample_61, sample_62, sample_63, sample_64, sample_65, sample_66, dm$new)

#Gives name to the last column
colnames(df)[ncol(df)] <- as.numeric(colnames(df)[ncol(df)-1])+0.0001

#name of taxa as rownames
row.names(df) <- dm[,1]
df <- df[-c(1:3,nrow(df)),]

#replace NA and blanks by 0
df[df == ""] <- 0
df[is.na(df)] <- 0


#Giving names to zero columns according to depth
for (i in 1:ncol(df)) {
  if ((colnames(df)[i] == 0) && (colnames(df)[i+1] == 0)) {
  colnames(df)[i] <- as.numeric(colnames(df)[i-1])+0.0001
  
  
  } else if ((colnames(df)[i] == 0) && (colnames(df)[i+1] != 0)) {
    
    colnames(df)[i] <- as.numeric(colnames(df)[i+1])-0.0001
  }}

#check the results
df

#add dates to the file
age <- read.table("laidavons_smooth_spline_ages.txt", header=T)

tilia_df <- rbind(df, a)
rownames(tilia_df)[nrow(tilia_df)] <- "age"

for (i in 1:ncol(tilia_df)) for (j in 1:nrow(age)) {
   if (colnames(tilia_df[i]) == age$depth[j] ) {
    tilia_df["age",i] <-  age$best[j]
   }else if (colnames(tilia_df[i]) == age$depth[j]+0.0001) {
      tilia_df["age",i] <-  age$best[j]+1
      
   } else if (colnames(tilia_df[i]) == age$depth[j]-0.0001 ) {
        tilia_df["age",i] <-  age$best[j]-1
}}
tilia_df["age",]
tilia_df <- tilia_df[-c((ncol(tilia_df)-1),ncol(tilia_df))]


#save
write.csv(tilia_df, "Lai_da_Vons_macrocounts_TILIA_07-2022.csv")
