#S3

door <- structure(sample(1:3,1), class = "door") #creates an object door of class door. Door contains a random door choice of one, two or three

PlayGame <- function(x){ #creates a S3 generic function
  UseMethod("PlayGame", x)
}

PlayGame.door <- function(x){ #creates a class-specific function for PlayGame. For class "door" 
  doorNum <- x[1] #sets doorNum equal to the number inside the door object
  car <- sample(1:3,1) #places the "car" inside a random "door": one two or three
  if(identical(doorNum,car)){ #if doorNum and car are identical. 
    return("You win a brand new car!") 
  }
  else{ #if doorNum, and car are not identical
    return("I am sorry, you picked wrong")
  }
}

PlayGame.door(door) #test function

#S4

setClass(Class="door", representation = representation(doorNum = "integer"), prototype = prototype(doorNum = c())) #

setValidity("door", function(object){
  if(!is.integer(object@doorNum)){
    return("doorNum must be of type numeric when in class 'door'")
  }
})

#setMethod("initialize", "door", function(.Object, ...){
#  value = callNextMethod()
#  validObject(value)
 # return(value)
#})

class(3)
door<- new("door", doorNum = as.integer(2))
door@doorNum

