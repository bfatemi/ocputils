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
  
  fun  <- do.call(buildfun, jsonlite::fromJSON(protolite::unserialize_pb(sodium::hex2bin(eval(parse(text = encoded$fun))))))
  vals <- jsonlite::fromJSON(protolite::unserialize_pb(sodium::hex2bin(eval(parse(text = encoded$args)))))
  pryr::make_call(fun, .args = vals)
}

# f <- function(...) return(NULL)
# args <- list(a = "ARG1", b = letters)
# eval(decode_call(encode_call(f, args)))