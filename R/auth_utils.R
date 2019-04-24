#' OCPU Auth Utils
#'
#' @param msg TBD
#' @param ciphx TBD
#' @param ocpu_json TBD
#' @param local TBD
#' @param keyx TBD
#' @param noncex TBD
#' @param privx TBD
#' @param hash TBD
#' @param pubx TBD
#' @param ll TBD
#'
#' @importFrom sodium simple_encrypt auth_decrypt simple_decrypt data_decrypt password_verify hex2bin bin2hex hash 
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom protolite unserialize_pb serialize_pb
#'
#' @name auth_utils
NULL

#' @describeIn auth_utils simple encryption using server's pub key
#' @export
ocpu_cipher <- function(msg){
  pub   <- sodium::hex2bin(ocpu_pubx)
  bin   <- protolite::serialize_pb(msg)
  ciph  <- sodium::simple_encrypt(msg, pub)
  ciphx <- sodium::bin2hex(ciph)
  return(ciphx)
}

#' @describeIn auth_utils simple decryption using server's priv key
#' @export
ocpu_decipher <- function(ciphx, privx){
  priv <- sodium::hex2bin(privx)
  tryCatch({
    ciph <- sodium::hex2bin(ciphx)
    bin  <- sodium::simple_decrypt(ciph, priv)
    protolite::unserialize_pb(bin)
  }, error = function(c){
    stop("API ERROR (400): Decryption Failed", call. = FALSE)
  })
}

#' @describeIn auth_utils TBD
#' @export
ocpu_unserialize <- function(ocpu_json, local=FALSE){
  if(local == TRUE)
    ocpu_json <- eval(parse(text = ocpu_json))
  ciphll <- jsonlite::fromJSON(ocpu_json, simplifyVector = FALSE)
  ll <- lapply(unlist(ciphll, recursive = FALSE), ocpu_decipher)
  invisible(ll)
}

#' @describeIn auth_utils TBD
#' @export
ocpu_serialize <- function(ll, pubx=NULL){
  f <- function(var, pub){
    bin <- protolite::serialize_pb(var)
    ocpu_ciph <- sodium::simple_encrypt(bin, pub)
    ocpu_var  <- sodium::bin2hex(ocpu_ciph)
    return(ocpu_var)
  }
  if(is.null(pubx)){
    pub <- sodium::hex2bin(ocpu_pubx)
  }else{
    pub <- sodium::hex2bin(pubx)
  }
  cll <- lapply(ll, f, pub = pub)
  json <- paste0("'", as.character(jsonlite::toJSON(cll)), "'")
  return(json)
}

#' @describeIn auth_utils TBD
#' @export
valid_client_apikey <- function(keyx, noncex, privx, hash){
  ocpu_priv   <- sodium::hex2bin(privx)
  client_pub  <- sodium::hex2bin(client_pubx)

  ciph  <- sodium::hex2bin(keyx)
  nonce <- sodium::hex2bin(noncex)

  bin <- sodium::auth_decrypt(bin = ciph,
                              key = ocpu_priv,
                              pubkey = client_pub,
                              nonce = nonce)
  apikey  <- protolite::unserialize_pb(bin)
  isvalid <- sodium::password_verify(apikey, hash)
  stopifnot(isvalid)
  return(isvalid)
}

# decode_var <- function(var){
#   ocpu_ciph <- sodium::hex2bin(var)
#   ocpu_priv <- sodium::hex2bin(privx)
#   bin <- sodium::simple_decrypt(ocpu_ciph, ocpu_priv)
#   var <- protolite::unserialize_pb(bin)
#   return(var)
# }

