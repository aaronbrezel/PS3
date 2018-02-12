#Activity 5: The Sorting Hat!

studentmaker <- function(person){
name1 <- person
ambition1 <- sample(1:100,1)
intelligence1 <- sample(1:100,1)
courage1 <- sample(1:100,1)
effort1 <- sample(1:100,1)
student <- list(name = name1,courage = courage1,ambition = ambition1,intelligence = intelligence1,effort = effort1)
class(student) <- "student"
return(student)
}
harry <- studentmaker("Harry")

matrixStats <- matrix(sample(1:100,16), nrow = 4)

sort.student <- function(x, y){
  if(!identical(length(y), 16)) {
    return("Second argument must be a matrix with 16 cells")
  }
  a = c(x$courage, x$ambition, x$intelligence, x$effort)
  b = y*a
  if(identical(b[1], max(b))){
    return("GRIFFINDOR!")
  } else if(identical(b[2], max(b))){
      return("SYLTHERIN!")
  } else if(identical(b[3], max(b))){
    return("RAVENCLAW!")
  } else if(identical(b[1], max(b))){
    return("HUFFLEPUFF!")
  } else{
    return("I can't decide!")
  }
}

