#Read the csv file and store it in the dataframe.
getwd()
setwd("C:/Users/hp/Documents")
New_dataframe <- read.csv("NIPostcodes.csv", header = FALSE)
New_dataframe

#Question A Struture of the dataframe.
str(New_dataframe)

# Total of Rows.
nrow(New_dataframe)
ncol(New_dataframe)

#First and Last 10 rows of dataframe.
head(New_dataframe, n=10)
tail(New_dataframe, n=10)

#Question B Change the appropraite column name in dataframe.
new_colnames <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                  "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                  "X-Cordinates","Y-Cordinates","Primary_Key")

colnames(New_dataframe) <- new_colnames
head(New_dataframe,10)

#Question C Replace and recode all missing values.
New_dataframe[New_dataframe==""] <- NA
sum(is.na(New_dataframe))
sum(!complete.cases(New_dataframe))

#graphical view
#install.packages("mice")
library(mice)
md.pattern(New_dataframe)
library("VIM")
missing_values <- aggr(New_dataframe, prop = FALSE, numbers = TRUE)


#Question D Missing Data Columnwise.
Missing_Count <- sapply(New_dataframe, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)
Missing_Count

#Question E Move the Primary Key Identifier to the start of the database.
New_dataframe <- New_dataframe[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
New_dataframe

#Question F Create Limavady_data dataframe.
Limavady_data <- New_dataframe[which(New_dataframe$Locality == "LIMAVADY" | New_dataframe$Townland == "LIMAVADY" & New_dataframe$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)
# Save the dataset with new csv file.
write.csv(Limavady_data,"Limavady.csv")
write.csv(New_dataframe,"CleanNIPostcodeData.csv")


#Section-2

rm(list = ls(all=TRUE))#helps to remove  objects
getwd()#displays the current working directory
setwd("C:/Users/hp/Documents/NI Crime Data")

#(A)Looking for patterns in a certain path and getting files

File=list.files(pattern = "[.]csv$",recursive = T)

#Assuming values with tab seperator with a header
ALLNICrimeData_List=lapply(File,function(x)read.csv(x, header = TRUE))

#Now Consider the same header columns for all files
ALLNICrimeData=do.call("rbind",ALLNICrimeData_List)
write.csv(ALLNICrimeData,"ALLNICrimeData.csv")
nrow(ALLNICrimeData)

#(B) Modifying and Removing the attributes from the newly created file

NewAllNICrimeData=subset(ALLNICrimeData,select = -c(Crime.ID,Reported.by,Falls.within,LSOA.code,LSOA.name,
                                                    Last.outcome.category))

#(C)Shortening each crime type

unique(ALLNICrimeData$Crime.type)

library(dplyr)

NewAllNICrimeData=NewAllNICrimeData%>%mutate(Crime.type=recode_factor(Crime.type,'Anti-social behaviour'='ASBO',
                                                                      'Bicycle theft'='BITH','Burglary'='BURG',
                                                                      'Criminal damage and arson'='CDAR',
                                                                      'Drugs'='DRUG','Other theft'='OTTH',
                                                                      'Possession of weapons'='POFW',
                                                                      'Public order'='PUBO','Robbery'='ROBY',
                                                                      'Shoplifting'='SHOP',
                                                                      'Theft from the person'='THPR',
                                                                      'Vehicle crime'='VECR',
                                                                      'Violence and sexual offences'='VISO',
                                                                      'Other crime'='OTCR'))

#(D)Showing a plot of each crime frequency in crime-type by using Plot() function

counts <- table(NewAllNICrimeData$Crime.type)

My_Plot=barplot(counts,main = "Distribution of Crime Type and frequency of each crime type", ylab = 'frequency',
                col = c(rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.4,0.5,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.9,0.9,0.4,0.6),rgb(0.3,0.3,0.4,0.6),rgb(0.7,0.1,0.4,0.6),rgb(0.1,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.1,0.9,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.4,0.9),rgb(0.3,0.9,0.4,0.9)),xlab = 'Crime Type',las=2) 
text(My_Plot,counts/2,paste("",counts,sep = ""),cex=1)

#(E) Modifying ALLNICrimeData to set Location atribute with street names
library(stringr)
NewAllNICrimeData$Location <- str_remove(NewAllNICrimeData$Location,"On or near")




