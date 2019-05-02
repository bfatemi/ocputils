#' TBD
#'
#' @param fun TBD
#' @param args TBD
#' @param encoded TBD
#' 
#' @importFrom rlang expr_text fn_fmls fn_body parse_expr global_env new_function
#' @importFrom sodium bin2hex hex2bin
#' @importFrom protolite serialize_pb unserialize_pb
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom pryr make_call
#' 
#' @examples 
#' fun_local <- function(...) return(NULL)
#' arg_local <- list(a = "ARG1", b = letters)
#' 
#' encoded <- encode_call(fun_local, arg_local)
#' #
#' #.
#' #..
#' #.... SEND TO OCPU API ENDPOINT /do_encoded 
#' #..
#' #.
#' #
#' decoded <- decode_call(encoded)
#' 
#' fun_remote <- rlang::call_fn(decoded)
#' arg_remote <- rlang::call_args(decoded)
#'
#' @name ocpu_calls
NULL

#' @describeIn ocpu_calls TBD
#' @export
encode_call <- function(fun, args){
  
  splitfun <- function(fun){
    argx <- rlang::expr_text(rlang::fn_fmls(fun))
    bodx <- rlang::expr_text(rlang::fn_body(fun))   
    return(list(a=argx, b=bodx))
  }
  
  list(
    fun = paste0("'", sodium::bin2hex(protolite::serialize_pb(jsonlite::toJSON(splitfun(fun)))), "'"),
    args = paste0("'", sodium::bin2hex(protolite::serialize_pb(jsonlite::toJSON(args))), "'")
  )
}

#' @describeIn ocpu_calls TBD
#' @export
decode_call <- function(encoded){
  
  buildfun <- function(a, b){
    env <- rlang::global_env()
    rlang::new_function(
      eval(rlang::parse_expr(a)),
      rlang::parse_expr(b),
      env = env
    )
  }
  fbin <- sodium::hex2bin(eval(parse(text = encoded$fun)))
  abin <- sodium::hex2bin(eval(parse(text = encoded$args)))
  
  f <- do.call(buildfun, jsonlite::fromJSON(protolite::unserialize_pb(fbin)))
  args <- jsonlite::fromJSON(protolite::unserialize_pb(abin))
  ocl <- pryr::make_call(f, .args = args)
  return(ocl)
}

#' @describeIn ocpu_calls TBD
#' @export
do_encoded <- function(fun, args){
  cl <- decode_call(list(fun, args))
  return(eval(cl))
}

# foc <- function(f, ...){
#   url <-
#   encode_call(f, ...)
# }
# 
# foc(function(a) return(a + 2), a=1)



