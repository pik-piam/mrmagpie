#' Neural Network Reconstruction
#'
#' Reconstructs and evaluate a neural network from the weights and biases provided as arguments
#'
#' @param inputs_ml Neural Network input features properly scaled with
#' the scale and center attributes of the scaled training set in a matrix format.
#' @param weights The learned weights and biases in a list format as outputed by the
#' function \code{keras::get_weights()}.
#' @param activation Name of the activation function used for training.
#'  Currently implemented functions: `relu`, `softplus`, `sigmoid`. Optionally, a custom
#'  activation function can be passed using a "." to indicate where the layer inputs should be piped.
#' @return The evaluated result of the neural network for the \code{input_ml} parameter.
#' @author Marcos Alves
#' @import dplyr
#' @importFrom pbapply pboptions pbapply
#' @export toolNeuralNet

toolNeuralNet <- function(inputs_ml, weights, activation) {
  if (activation %in% c("relu", "softplus", "sigmoid")) {
    activation <- switch(activation,
      relu = "{pmax(0,.)}",
      softplus = "{log(1+exp(.))}",
      sigmoid = "{1/(1-exp(-.))}",
    )
  }
  if (dim(weights[[1]])[1] != dim(inputs_ml)[2]) {
    stop(paste0("Inputs and weights are not conformable. Inputs are expected to have ",
      dim(weights[[1]])[1], " features/columns"))
  }
  x <- paste0(".f <- function(input) {")
  x <- append(x, paste0("y <- {input %*% weights[[1]] + t(weights[[2]])} %>%"))
  x <- append(x, paste0(activation, " %>%"))
  for (i in seq(3, length(weights), 2)) {
    if (i < length(weights) - 2) {
      x <- append(x, paste0("{. %*% weights[[", i, "]] + t(weights[[", i + 1, "]])} %>%"))
      x <- append(x, paste0(activation, " %>%"))
    } else {
      x <- append(x, paste0("{. %*% weights[[", i, "]] + t(weights[[", i + 1, "]])}"))
    }
  }
  x <- append(x, "return(y)}")
  func <- eval(parse(text = x))
  pboptions(type = "txt", style = 3, char = "=")
  out <- system.time(pbapply(inputs_ml, 1, func))
  return(out)
}
