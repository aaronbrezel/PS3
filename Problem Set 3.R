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

setClass(Class="door", representation = representation(doorNum = "numeric"), prototype = prototype(doorNum = c())) #S4 creation of door class. doorNum set to require integer

setValidity("door", function(object){
  if(is.integer(object@doorNum)){ #if object at doorNum is an integer
    return("doorNum must be of type numeric when in class 'door'")
  }
  else if(object@doorNum < 1 | object@doorNum > 3){#if object at doorNum is not between 1 and 3 
    return("Yo, you got to pick a number between 1 and 3")
  }
})

#what do you do?
setMethod("initialize", "door", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})


door<- new("door", doorNum = as.integer(3))


setGeneric("PlayGame", function(object){standardGeneric("PlayGame")})
setMethod("PlayGame", "door", function(object){ 
  doorNum <- object@doorNum #sets doorNum equal to the number inside the x object
  car <- sample(1:3,1) #places the "car" inside a random "door": one two or three
  if(identical(doorNum,car)){ #if doorNum and car are identical. 
    return("You win a brand new car!") 
  }
  else{ #if doorNum, and car are not identical
    return("I am sorry, you picked wrong")
  }
})
PlayGame(door)
