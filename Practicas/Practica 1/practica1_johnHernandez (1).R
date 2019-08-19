#we remove existing objects
rm(list=ls())

#we download the .RData file into our working directory from the repository in github
#first we storage the url in our rdata variable
rdata<-"https://github.com/ucb-stat133/stat133-fall-2016/raw/master/data/usa-states.RData"
#now let's download the data from the url
download.file(url=rdata,destfile = 'usa-states.RData')
#and we load the downloaded data in our R session
load('usa-states.RData')

# list the available objects with ls()
ls()

#inspecting data types 
#typeof() type of storage of any object
#class() gives you the class of the object
# str() displays the structure of an object in a compact way
#• mode() gives the data type (as used in R)
#• object.size() gives an estimate of the memory space used by an object
#• length() gives the length (i.e. number of elements)
#• head() take a peek at the first elements
#• tail() take a peek at the last elements
#• summary() shows a summary of a given object

#now we will test the class of each object
class(area) #numeric
class(capital)#character
class(seats)#numeric
class(state)#character
class(water)#numeric

#Now use length(), head(), tail(), and summary() to start exploring the content of the objects:
#for area: which shows us that the 50 area values are numeric with mean 196667
length(area)
head(area)
tail(area)
summary(area)

#for state: which shows us that the 50 state values are characters
length(state)
head(state)
tail(state)
summary(state)

#for capital: which shows us that the 50 capital values are characters
length(capital)
head(capital)
tail(capital)
summary(capital)

#for water: which shows us that the 50 water values are numeric with mean 13718
length(water)
head(water)
tail(water)
summary(water)

#for seats: which shows us that the 50 seats values are numeric with mean 8.70
length(seats)
head(seats)
tail(seats)
summary(seats)


#------vectors in R---------
#we test the type of the vectors:
typeof(state)
typeof(capital)
typeof(area)
typeof(water)
typeof(seats)

# first element of 'state'
state[1]
# first five elements of 'state' numeric range:
state[1:5]
# numeric vector
state[c(1, 3, 5, 7)]
# different order
state[c(20, 9, 10, 50)]
# third element (four times)
state[rep(3, 4)]

#----------Logical subsetting--------------------
# area of California
area[state == 'California']
# name of states with areas greater than 400,000 square km
state[area > 400000]
# name of states with areas between 100,000 and 125,000 square km
state[area > 100000 & area < 125000]

#Write commands to answer the following questions:
# name of the state with largest area
state[area==max(area)]
# name of the state with smallest area
state[area==min(area)]
# name of the state with largest number of seats
state[seats==max(seats)]
# capital of the state with the smallest water area
stateMinWater<-state[water==min(water)]
capital[state==stateMinWater]

#-------subsetting with character vectors-----------
#A third type of subsetting involves passing a character vector inside brackets. 
#When you do this, the characters
#are supposed to be names of the manipulated vector.
names(state)
# create 'total'
total<-(water+area)
# assign 'state' as names of 'total'
names(total)<-state
names(total)
#NOW WE CAN USE CHARACTER SUBSETTING!!
total["Alabama"]
total[c("California", "Oregon", "Washington")]
total[c("Texas", "Alaska")]



#------Some plotting-----------
plot(area,water)

#getting a better display
log_area<-log(area)
log_water<-log(water)

plot(log_area,log_water)
text(log_area, log_water, labels = state)

plot(log_area,log_water)
text(log_area, log_water,labels=abbreviate(state))


#----Vectorization-------
#This is called Vectorization in R parlance. Most functions that operate with vectors in R are vectorized
#functions. This means that an action is applied to all elements of the vector without the need to explicitly
#type commands to traverse all the elements.
sqrt(area)
sqrt(water)

#----Reciclying-------
#To convert from square kilometers to aquare miles use the following conversion: 
#1 sqr km =0.386 sqr mi
areaSquareMi=area*0.386
areaSquareMi
#Well, R uses the recycling rule, which takes the shorter vector (in this case 0.386) and recycles its elements
#to form a temporary vector that matches the length of the longer vector (i.e. area)

#now let's do the same with the waterBodies area
w_areaSquareMi=water*0.386
w_areaSquareMi

#Make a scatterplot of area_square_miles and water_square_miles transforming the vectors into log scale
#getting a better display
log_areaSM<-log(areaSquareMi)
log_waterSM<-log(w_areaSquareMi)

plot(log_areaSM,log_waterSM)
text(log_areaSM, log_waterSM, labels = state)
