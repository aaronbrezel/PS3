#Activity 5: The Sorting Hat!

studentmaker <- function(person){ #function that create an object of class students
  name1 <- person #name of the sudent
  ambition1 <- sample(1:100,1) #next 4 lines are values for the four major character traits
  intelligence1 <- sample(1:100,1)
  courage1 <- sample(1:100,1)
  effort1 <- sample(1:100,1)
  student <- list(name = name1,courage = courage1,ambition = ambition1,intelligence = intelligence1,effort = effort1) #creation of a student list object with a name plus all four character traits 
  class(student) <- "student" #sets class of student to student
  return(student) #returns newly created list object
}

harry <- studentmaker("Harry") #creates a student called Harry
matrixStats <- matrix(sample(1:100,16), nrow = 4) #creates a matrix

sort.student <- function(x, y){ #sort.student method
  if(nrow(y) != 4 | ncol(y) != 4) { #checks is the matrix being fed in as right right size 
    return("Second argument must be a 4x4 matrix")
  }
  a = matrix(c(x$courage, x$ambition, x$intelligence, x$effort), ncol = 1) #restructures student object into matrix
  b = y%*%a #matrix multiplication
  if(identical(b[1], max(b))){ #if statements to decide house sort
    return("Gryffindor")
  } else if(identical(b[2], max(b))){
    return("Slytherin")
  } else if(identical(b[3], max(b))){
    return("Ravenclaw")
  } else if(identical(b[4], max(b))){
    return("Hufflepuff")
  } else{
    return("Undecided")
  }
}


house <- sort.student(harry,matrixStats)
print(house)
harry <- structure(harry, class = c("student", house)) #gives harry a second class based on the house he was sorted into
#4

Gryffindor_Tower <- new.env() #four new environments. One for each house
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()


curfew <- function(x){ #creates generic function cerfew
  UseMethod("curfew",x)
}
#create four methods. One for each house
curfew.Gryffindor <- function(x){
  assign(as.character(x$name), x, envir = Gryffindor_Tower) #assign a new object inside the class specific environment with all the properties of the original student 
  name <- deparse(substitute(x)) #creates a string of the object name
  rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv()) #removes the old object from the global environment, leaving only the new object in the correct environment
}

curfew.Slytherin <- function(x){
  assign(as.character(x$name), x, envir = Black_Lake)
  name <- deparse(substitute(x))
  rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
}

curfew.Ravenclaw <- function(x){
  assign(as.character(x$name), x, envir = Ravenclaw_Tower)
  name <- deparse(substitute(x))
  rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
}

curfew.Hufflepuff <- function(x){
  assign(as.character(x$name), x, envir = Basement)
  name <- deparse(substitute(x))
  rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
}


curfew(harry)
ls(Basement)
