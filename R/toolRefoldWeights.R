#' Refold weights from NN training
#' Refold weights into their original configuration.
#'
#' @param x magpie object containing weights.
#' @author Marcos Alves
#' @importFrom stringr str_split
#' @export toolRefoldWeights

toolRefoldWeights <- function(x) {
  y <- list()
  for (i in getItems(x, dim = 3.1)) {
    tmp <- x[,,i]
    x_dim <- getItems(tmp,dim = 3.2) %>% str_split("_") %>% unlist() %>% as.numeric()
    tmp <- as.matrix(tmp)
    dim(tmp) <- x_dim
    y[[i]] <- tmp
  }
  return(y)
}
