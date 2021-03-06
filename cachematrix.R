## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {        ## Define el argumento de la funci�n la cual por defecto es una matriz
  inv <- NULL                                      ## Inicia la variable "inv" como nula, la cual guardar� el valor de la matriz inversa
  set <- function(y) {                             ## Se define la funci�n "set" para asignar un nuevo valor de la matriz en el entorno padre
    x <<- y                                        ## Si hay una nueva matriz se restablece el valor de "inv" como nulo
    inv <<- NULL                        
  }
  get <- function() x                              ## Se define la funci�n "get" retorna el valor del argumento de la matriz           
  
  setinv <- function(inverse) inv <<- inverse      ## Asigna el valor de "inv" en el entorno padre
  getinv <- function() inv                         ## Obtiene el valor de la inversa donde sea llamada.
  list(set = set, get = get, setinv = setinv, getinv = getinv) 

}


## Write a short comment describing this function

## Esta funci�n calcula la inversa de la "matriz" especial devuelto por 
## la funci�n anterior "makeCacheMatrix".
## Si la inversa ya fue calculada (y la matriz no ha cambiado),
## Entonces la funci�n "cacheSolve" recuperar� la inversa de la memoria.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
