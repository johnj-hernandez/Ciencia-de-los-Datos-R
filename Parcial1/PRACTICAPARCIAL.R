###############################################################
### Data Science UTB
### Professor: Enrique J. De La Hoz D.
### Test 1 con R: 05.09.2018
### Nombre:John Javier Hernandez Araujo
########################################################

#################
## Ejercicio 1 ##
#################

age <- rpois(20, 40)
hei <- round(rnorm(20, 175, 10), 2)

# 1a What is the height of the tallest person?
max(hei)
 
# 1b Which is the person with the lowest height and how old is that person?
index<-which(hei==min(hei))
age[index]

# 1c How many people have an age between 30 and 40 years, both including?
n<-sum(age>29)-sum(age>40)

# 1d Use function sample to create a vector called uni with the university 
#careers of these people: Statistics, Physics, Biology, and Medicine.

# You may use the following vector: c('Statistics', 'Physics', 'Biology', 'Medicine')

possibleValues<-c('Statistics', 'Physics', 'Biology', 'Medicine')
uni <-sample(possibleValues,20,replace=TRUE)

# 1e What is the median age of those who studied physics or medicine?
median(age[uni %in% c('Physics','Medicine')])

# 1f Overwrite the values of 'Statistics' and 'Physics' by 'Stats' and 'Phys', respectively.
uni[uni=='Physics']<-'Phys'

uni[uni=='Statistics']<-'Stats'
# 1g Create a data frame that contains the three variables -age, hei, uni- and save the workspace.
myDataFrame<-cbind(age,hei,uni)
save.image(file = "myws.RData")
?save
#################
## Ejercicio 2 ##
#################

state.x77
state.region

# 2a Is object state.x77 a matrix or a data frame?
class(state.x77)  #Es una matriz

# 2b  How many variables does state.x77 have?
ncol(state.x77)

# 2c Convert state.x77 into a data frame with name states77 and add state.region 
#as a new variable with name Regio.

states77 <- as.data.frame(state.x77)
states77$Regio=state.region
colnames(states77)
# 2d Delete the variables HS.Grad and Frost.
states77$`HS Grad`<-NULL
states77$Frost<-NULL

# Cambio de los nombres de las variables
names(states77) <- substr(tolower(names(states77)), 1, 4)
names(states77)

# 2e Overwrite the life expectancies of the first and last state, Alabama and Wyoming, 
#by the values 71.3 and 72.1, respectively.
states77[1,"life"]=71.3
states77[nrow(state.x77),"life"]=72.1
# 2f Compute the correlations (of Pearson) among the variables inco, illi, and murd.
cor(states77[,c("inco","illi","murd")])

?cor
# 2g Which of the four regions has the highest average salary? How much is it?
library(dplyr)
averages<-states77%>%group_by(regi)%>%summarise(
  averageSalaries=mean(inco)
)
averages[which(averages$averageSalaries==max(averages$averageSalaries)),]
#################
## Ejercicio 3 ##
#################
# 3a
datosEx3 <- read.table("Test1Exercise3.txt",
                       header=TRUE,
                       skip = 5,
                       dec = ",",
                       na.strings = c("*","NA")
                       )

# 3b Which are Pedro's age, height, and weight?
datosEx3["Pedro",1:3]


# 3c Calculate the mean of variable 'Height' and the median of variable 'Weight'.
datosEx3%>%summarise(
  meanHeight=mean(Height,na.rm = TRUE),
  medianWeight=median(Weight,na.rm = TRUE)
)

