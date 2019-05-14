#' OCPU API Utils
#'
#' @param pkg TBD
#' @param fun TBD
#' @param verb TBD
#' @param host TBD
#' @param ... TBD
#' @param apikey TBD
#' @param url TBD
#' @param args TBD
#' @param config TBD
#' @param r TBD
#' 
#' @importFrom pryr dots make_call named_dots
#' @importFrom rlang invoke
#' @importFrom httr content stop_for_status GET POST status_code
#' @importFrom stringr str_split str_remove str_extract
#' @importFrom protolite unserialize_pb
#' @importFrom sodium hash hex2bin data_decrypt
#'
#' @name api_utils
NULL

#' @describeIn api_utils TBD
#' @export
ocpu_api <- function(pkg, fun, verb, host, ...){
  verb <- match.fun(match.arg(verb, choices = c("GET", "POST")))
  url  <- paste0("https://", host, "/ocpu/library/", pkg, "/R/", fun)
  body <- sapply(pryr::named_dots(...), eval, USE.NAMES = TRUE, simplify = FALSE)

  args <- list(url = url, body = body, encode = "json")
  resp <- do.call(verb, args = args)

  httr::stop_for_status(resp)
  parsed <- httr::content(resp, "text", encoding = "UTF-8")
  txt <- stringr::str_split(parsed, pattern = "\\n")[[1]]

  result <- txt[-which(sapply(txt, function(i) i == ""))]
  return(result)
}


#' @describeIn api_utils TBD
#' @export
ocpu_body <- function(...){
  wrap <- function(i) paste0("'", eval(i), "'")
  args <- sapply(pryr::named_dots(...), wrap, USE.NAMES = TRUE, simplify = FALSE)
  return(args)
}

#' @describeIn api_utils TBD
#' @export
ocpu_icecube_url <- function(fun){
  srv <- "https://icecube.hpds.network"
  lib <- "/ocpu/user/bobbyf/library/icecube"
  url <- paste0(srv, lib, "/R/", fun)
  return(url)
}

#' @describeIn api_utils TBD
#' @export
oops <- function() return(oops_html)

#' @describeIn api_utils TBD
#' @export
ocpu_radix_url <- function(apikey, fun=NULL){
  tryCatch({
    ciph_lib  <- sodium::hex2bin(ciph_lib_hex)
    ciph_srv  <- sodium::hex2bin(ciph_srv_hex)
    nonce_srv <- sodium::hex2bin(nonce_srv_hex)
    nonce_lib <- sodium::hex2bin(nonce_lib_hex)
    
    keyhash <- sodium::hash( sodium::hex2bin(apikey) )
    srv <- sodium::data_decrypt(ciph_srv, keyhash, nonce_srv)
    lib <- sodium::data_decrypt(ciph_lib, keyhash, nonce_lib)
    
    srvx <- protolite::unserialize_pb(srv)
    libx <- protolite::unserialize_pb(lib)
    paste0(srvx, libx, fun)  
  }, error = function(c){
    stop("Invalid api client key", call. = FALSE)
  })
}

#' @describeIn api_utils TBD
#' @export
ocpu_url <- function(node, usr=NULL, fun=NULL, apikey = NULL){
  if(is.null(usr)) usr <- node
  if(node == "radix"){
    if(Sys.getenv("HPDS_API") == "" & is.null(apikey))
      stop("System var HPDS_API is not set and argument 'apikey' not given")
    return( httr::parse_url(ocpu_radix_url(apikey, fun)) )
  }
  if(node == "icecube"){
    return( httr::parse_url(ocpu_icecube_url(fun)) )
  }
  url <- paste0("https://", node, ".hpds.network/ocpu/user/", usr, "/library/api/R/", fun)
  httr::parse_url(url)
}

#' @describeIn api_utils TBD
#' @export
wrap_args <- function(...){
  nams <- names(pryr::dots(...))
  ll <- as.list(rlang::invoke(paste0, "'", c(...), "'"))
  names(ll) <- nams
  return(ll)
}

#' @describeIn api_utils TBD
#' @export
ocpu_parsed_post <- function(url, args = NULL, config = NULL){
  resp <- httr::POST(url, config, body = args)
  status_code <- httr::status_code(resp)

  url_session  <- resp$headers$location
  url_console  <- paste0(url_session, "console")
  url_function <- resp$url

  r_endpoints  <- httr::content(httr::GET(url_session), "text", encoding = "UTF-8")
  console_txt  <- httr::content(httr::GET(url_console), "text", encoding = "UTF-8")
  function_txt <- httr::content(httr::GET(url_function), "text", encoding = "UTF-8")

  resp_content <- stringr::str_remove(httr::content(resp, "text", encoding = "UTF-8"), "\\n$")

  if(status_code == 201){
    output_value <- httr::content(httr::GET(paste0(url_session, "R/.val")), "text", encoding = "UTF-8")
  }else{
    output_value <- NULL
  }

  parsed <- list(
    "status_code"   = resp$status_code,
    "session_id"    = resp$headers$`x-ocpu-session`,
    "url_session"   = url_session,
    "content_text"  = resp_content,
    "r_endpoints"   = r_endpoints,
    "console_txt"   = console_txt,
    "request_url"   = resp$url,
    "output_value"  = output_value,
    "r_function"    = function_txt,
    "date"          = resp$headers$date,
    "server"        = resp$headers$server,
    "r_version"     = resp$headers$`x-ocpu-r`,
    "ocpu_version"  = resp$headers$`x-ocpu-version`,
    "ocpu_time"     = resp$headers$`x-ocpu-time`,
    "content_cache" = resp$headers$`cache-control`,
    "content_encod" = resp$headers$`content-encoding`,
    "content_type"  = resp$headers$`content-type`
  )
  return(parsed)
}

#' @describeIn api_utils TBD
#' @export
ocpu <- function(...){
  url <- "https://cortex.db.report/ocpu/user/intusurg/library/api/R/parse_entry"
  
  wrap_args <- function(...){
    nams <- names(pryr::dots(...))
    ll <- as.list(rlang::invoke(paste0, "'", c(...), "'"))
    names(ll) <- nams
    return(ll)
  }
  pryr::make_call("POST", url=url, body = list(wrap_args(...)))
}

#' @describeIn api_utils TBD
#' @export
call_ocpu <- function(...) do.call(ocpu, ...)


#' @describeIn api_utils TBD
#' @export
do_ocpu <- function(r){
  resp <- eval(r)
  list(
    "status"= resp$status_code,
    "id" = resp$headers$`x-ocpu-session`,
    "session" = list(
      "api"        = stringr::str_extract("https://cortex.db.report/ocpu/tmp/", ".+(?=\\/ocpu)"),
      "rvalue"     = paste0(resp$headers$location, "R/.val/json"),
      "console"    = paste0(resp$headers$location, "console/text"),
      "source"     = paste0(resp$headers$location, "source/text"),
      "stdout"     = paste0(resp$headers$location, "stdout/text"),
      "files"      = paste0(resp$headers$location, "files/"),
      "session"    = resp$headers$location
    ),
    "meta" = list(
      "request"       = resp$url,
      "date"          = resp$headers$date,
      "server"        = resp$headers$server,
      "r_version"     = resp$headers$`x-ocpu-r`,
      "ocpu_version"  = resp$headers$`x-ocpu-version`,
      "ocpu_time"     = resp$headers$`x-ocpu-time`,
      "content_cache" = resp$headers$`cache-control`,
      "content_encod" = resp$headers$`content-encoding`,
      "content_type"  = resp$headers$`content-type`
    )
  )
}

