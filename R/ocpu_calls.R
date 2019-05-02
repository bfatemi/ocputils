#' TBD
#'
#' @param fun TBD
#' @param args TBD
#' @param encoded TBD
#' @param msg TBD
#' 
#' @importFrom rlang expr_text fn_fmls fn_body parse_expr global_env new_function
#' @importFrom sodium bin2hex hex2bin
#' @importFrom protolite serialize_pb unserialize_pb
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom pryr make_call
#' @importFrom stringr str_split str_c str_length
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

#' @describeIn ocpu_calls TBD
#' @export
do_encoded <- function(fun, args){
  ocl <- decode_call(list(fun, args))
  return(eval(ocl))
}

#' @describeIn ocpu_calls TBD
#' @export
printstamp <- function(msg=""){
  sym <- "#"
  msglines <- stringr::str_split(msg, "\\n")[[1]]
  msgmain <- msglines[which.max(sapply(msglines, stringr::str_length, simplify = FALSE)[[1]])]
  
  # make message length an even number for centering purposes
  if(stringr::str_length(msgmain) %% 2 == 1)
    msgmain <- stringr::str_c(msgmain, " ")
  
  msg <- stringr::str_c(" ", msglines, " ")
  scount <- stringr::str_length(msgmain)
  cushion <- ceiling(scount*1.3) - scount
  
  cushion <- cushion + cushion %% 2
  topcount  <- scount + cushion - 1 + 2
  sidecount <- sum(length(msglines), 2)
  
  # hdft   <- stringr::str_c(rep(sym, topcount), collapse = "")
  spaces <- stringr::str_c(rep(" ", topcount - 1), collapse = "")
  sides.left   <- rep(paste0(sym, ">"), sidecount)
  sides.right  <- rep(sym, sidecount)
  
  grid_col <- topcount + 1
  grid_row <- sidecount + 2
  
  tmp <- stringr::str_c(stringr::str_c(sides.left, spaces), collapse = "\n")
  txt <- stringr::str_split(stringr::str_split(tmp, "\n")[[1]], "")
  
  pad.l <- c(paste0(sym, "> "), rep("", cushion/2-1))
  pad.r <- " "#c(rep(" ", cushion/2-1), sym)
  txt[2:(1+length(msglines))] <- lapply(stringr::str_split(msglines, ""), function(i) c(pad.l, i, pad.r))
  
  cat("\n\n")
  cat(paste0(sapply(txt, function(itxt) paste0(c(itxt, "\n"), collapse = "")), collapse = ""))
  cat("\n")
}


# 
# foc <- function(f, ...){
#   r <- httr::POST(ocpu_icecube_url("do_encoded"), body = encode_call(f, ...))
#   if(httr::status_code(r)!=201){
#     return(httr::content(r, "text", encoding = "UTF-8"))
#   }
#   src <- httr::content(httr::GET(paste0(r$headers$location, "console")), "text", encoding = "UTF-8")
#   cat(paste0("\n", src, "\n"))
#   val <- jsonlite::fromJSON(httr::content(httr::GET(paste0(r$headers$location, "R/.val/json")), "text", encoding = "UTF-8"))
#   return(val)
# }
# 
# 
# foc(function(a) return(a + 2), list(a=1))



