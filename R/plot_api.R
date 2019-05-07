#' Title
#'
#' @param p TBD
#' @param hxp TBD
#'
#' @importFrom sodium hex2bin bin2hex 
#' @importFrom protolite serialize_pb unserialize_pb
#'
#'@name plot_api
NULL

#' @describeIn plot_api TBD
#' @export
plot2hex <- function(p){
  obj <- rawToChar(serialize(p, NULL, ascii = TRUE))
  bin <- protolite::serialize_pb(obj)
  sodium::bin2hex(bin)
}

#' @describeIn plot_api TBD
#' @export
hex2plot <- function(hxp){
  obj <- protolite::unserialize_pb(sodium::hex2bin(hxp))
  unserialize(charToRaw(obj))
}
