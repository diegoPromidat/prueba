box::use(
)

#' @export
calcular.distancias <- function(perfiles, transaccion) {
  aux <- strsplit(attr(transaccion, "concept"), ":")[[1]]
  if(length(aux) > 1) {
    perfil <- aux[2]
    perfil <- perfiles[which(attr(perfiles, "concept") == perfil), ]
  } else {
    perfil <- perfiles[which(attr(perfiles, "concept") == aux), ]
  }
  
  res <- sapply(names(perfil), function(x) {
    if("symbolic_interval" %in% class(perfil[[x]])) {
      minimo <- min(perfil[[x]])
      maximo <- max(perfil[[x]])
      valor <- max(transaccion[[x]])
      
      if(valor > maximo | valor < minimo) {
        return(0)
      } else {
        return(1)
      }
    } else {
      indice <- which(transaccion[[x]][1][[1]]$prop == 1)
      cat <- transaccion[[x]][1][[1]]$var[indice]
      
      indice <- which(perfil[[x]][1][[1]]$var == cat)
      if(length(indice) == 0) {
        return(0)
      } else {
        return(perfil[[x]][1][[1]]$prop[indice])
      }
    }
  })
  
  return(1 - mean(res))
}

#' @export
distancia.maxima <- function(perfiles, datos) {
  ids <- attr(perfiles, "concept")
  
  res <- lapply(ids, function(ID) {
    aux <- datos[datos$CUSTOMER_ID == ID, ]
    
    distancias <- sapply(1:nrow(aux), function(i) {
      transaccion <- classic.to.sym(aux[i, ], "CUSTOMER_ID")
      calcular.distancias(perfiles, transaccion)
    })
    
    max(distancias)
  })
  
  names(res) <- ids
  return(res)
}
