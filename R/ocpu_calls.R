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
decode_call <- function(fun, args){
  
  buildfun <- function(a, b){
    env <- rlang::global_env()
    rlang::new_function(
      eval(rlang::parse_expr(a)),
      rlang::parse_expr(b),
      env = env
    )
  }
  fbin <- sodium::hex2bin(fun)
  abin <- sodium::hex2bin(args)
  
  f <- do.call(buildfun, jsonlite::fromJSON(protolite::unserialize_pb(fbin)))
  args <- jsonlite::fromJSON(protolite::unserialize_pb(abin))
  ocl <- pryr::make_call(f, .args = args)
  return(ocl)
}




# foc <- function(f, ...){
#   r <- httr::POST(ocpu_icecube_url("do_encoded"), body = encode_call(f, ...))
#   if(httr::status_code(r)!=201){
#     return(httr::content(r, "text", encoding = "UTF-8"))
#   }
#   src <- httr::content(httr::GET(paste0(r$headers$location, "console")), "text", encoding = "UTF-8")
#   srclines <- stringr::str_split(src, "\\n")[[1]]
#   ind <- which(stringr::str_detect(srclines, "^\\#\\>"))
#   
#   clean_src <- srclines[-(1:(ind[1]-1))]
#   ind  <- which(stringr::str_detect(clean_src, "^\\#\\>"))
#   ind2 <- c(-ind, -which(stringr::str_detect(clean_src, "^$")))
#   
#   bgMaroon <- crayon::make_style("maroon", bg=TRUE)
#   yellow1 <- crayon::make_style("yellow1")
#   ivory <- crayon::make_style("ivory")
#   clean_src[ind] <- crayon::italic(crayon::cyan(clean_src[ind]))
#   clean_src[ind2] <- crayon::bold(yellow1(clean_src[ind2]))
#   
#   cat(paste0(clean_src, collapse = "\n"))
#   val <- jsonlite::fromJSON(httr::content(httr::GET(paste0(r$headers$location, "R/.val/json")), "text", encoding = "UTF-8"))
#   return(val)
# }
# 
# 
# val <- foc(function(a) return(a + 2), list(a=1))



