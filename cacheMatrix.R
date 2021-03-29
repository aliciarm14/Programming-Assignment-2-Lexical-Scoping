makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inicializar la matriz inversa
  i <- NULL
  

  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Método para obtener la matriz
  get <- function() {

    m
  }
  
  ## Método para establecer la matriz inversa
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Método para obtener la matriz inversa
  getInverse <- function() {

    i
  }
  
  ## Retornar la lista de métodos
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Funcion de cache
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("Obteniendo datos cache")
    return(m)
  }
  

  data <- x$get()
  
  ## Calcular la inversa usando la multiplicación de matrices 
  m <- solve(data) %*% data
  
  ## Inversa del objeto
  x$setInverse(m)
  
  ## Devolvemos la matriz
  m
}
