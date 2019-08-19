url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone <- read.table(url, sep = ",")
abalone

#. What's the character delimiter?        comas  ,
#. Is there a row for column names? NO
#  . Are there any missing values?NO
#  . What are the data types of each column? la primera es una character, la ultima (rings es entero), las otras son numerics mas  exactamente decimales

# download copy
origin <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
destination <- 'abalone.data'
download.file(origin, destination)

#Reading and inspecting data --->Parece haber 4177 registros
abalone <- read.table("abalone.data", sep = ",")
head(abalone)
tail(abalone)

# check data frame's structure , vec len nos muestra que tantos elmentos queremos ver por el factor de conversion
str(abalone, vec.len = 1)


#vector of col names
col_names <- c(
  'sex',
  'length',
  'diameter',
  'height',
  'whole_weight',
  'shucked_weight',
  'viscera_weight',
  'shell_weight',
  'rings'
)

#vector of col types
col_types <- c(
  'factor',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'integer'
)

#forma de leer los datos de manera mas eficiente
#pasandoles los nombres y tipos (recordar que read table retorna un data frame)
?read.table
abalone <- read.table(
  'abalone.data',
  col.names = col_names,
  colClasses = col_types,
  sep = ","
)
?colnames()
# check its structure again


#-----MY TURN ----
#Read the Abalone data with the read.csv() function
#new names
col_names2 <- c(
  'sex',
  'length',
  'diameter',
  'height',
  'whole',
  'shucked',
  'viscera',
  'shell',
  'rings'
)
#reading data
abalone2<-read.csv("abalone.data", sep = ",",header=FALSE,col.names = col_names2,colClasses = col_types)
head(abalone2)

#We will use the apply function to aplly other functions such as
#sd,mean,max and min in only the numeric cols to check whether they match the known values
?apply  #Pareciera que apply funcion solo cuando se le pasa mas de una columna como en este caso
#investigar mas a fondo su uso como tarea propia....
a<-apply(abalone2[,(2:9)],2,min)
b<-apply(abalone2[,(2:9)],2,max)
c<-apply(abalone2[,(2:9)],2,mean)
d<-apply(abalone2[,(2:9)],2,sd)

#this is the custom Summary with which we can can get only the Min,Max,MEan and Sd of the numeric col to check their values
customSummary<-rbind(a,b,c,d)
row.names(customSummary)<-c("Min","Max","Mean","SD")
round(customSummary,3)



#----------Plots---------
# plot of a factor
plot(abalone$sex)

#First with the frequency table
table_sex <- table(abalone$sex)
barplot(table_sex)


hist(abalone$diameter)
boxplot(abalone$diameter, horizontal = TRUE)


#------------------PITSBURGH BRIDGES.....
#Read the description, and take a look at the data set:
#  . Are there column names? NO
#  . What is the field separator? comas ,
#  . Are there any missing values? SI
#  . What is the character for missing values (if any)?   signo de interrogacions:  ?
#  . What is the data type of each variable (i.e. column)?  

# download a copy of the data file
origin2 <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1'
destination2 <- 'bridges.data'
download.file(origin2, destination2)

# vector of column names

col_names3 <- c(
  'IDENTIF',
  'RIVER',
  'LOCATION',
  'ERECTED',
  'PURPOSE',
  'LENGTH',
  'LANES',
  'CLEAR-G',
  'T-OR-D',
  'MATERIAL',
  'SPAN',
  'REL-L',
  'TYPE'
)

#vector of col types
col_types3 <- c(
  'factor',
  'character',
  'numeric',
  'numeric',
  'factor',
  'numeric',
  'factor',
  'factor',
  'factor',
  'factor',
  'factor',
  'factor',
  'factor'
)
# reading the data with 'read.table()'
bridges <- read.table(
  'bridges.data',
  col.names = col_names3,
  colClasses = col_types3,
  sep = ",",
  na.strings = "?"
)
bridges
# re reading the data with 'read.csv()'
bridges<-read.csv("bridges.data", sep = ",",header=FALSE,col.names = col_names3,colClasses = col_types3,na.strings = "?")

#Basic Inspection
str(bridges)
summary(bridges)
head(bridges)
tail(bridges)
dim(bridges)
names(bridges)
colnames(bridges) #mostro lo mismo que names(bridges)
nrow(bridges)
ncol(bridges)

#Research Questions
#. Year of the oldest bridge
subset(bridges,ERECTED==min(ERECTED))
#. Year of the most recent erected bridge
subset(bridges,ERECTED==max(ERECTED))
#. Frequency of bridges by purpose
tablePurpose<-table(bridges$PURPOSE)
tablePurpose
#. Frequency of materials
tableMaterials<-table(bridges$MATERIAL)
tableMaterials
#. Average length of the bridges
mean(bridges$LENGTH, na.rm=TRUE)
#. Plot a timeline: year -vs- length
plot(bridges$ERECTED,bridges$LENGTH)


