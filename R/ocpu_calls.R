#' TBD
#'
#' @param fun TBD
#' @param args TBD
#' 
#' @importFrom rlang expr_text fn_fmls fn_body parse_expr global_env new_function
#' @importFrom sodium bin2hex hex2bin
#' @importFrom protolite serialize_pb unserialize_pb
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom pryr make_call
#' @importFrom utils capture.output str head
#' 
#' @examples 
#' #fun_local <- function(...) return(NULL)
#' #arg_local <- list(a = "ARG1", b = letters)
#' 
#' #encoded <- encode_call(fun_local, arg_local)
#' #
#' #.
#' #..
#' #.... SEND TO OCPU API ENDPOINT /do_encoded 
#' #..
#' #.
#' #
#' #decoded <- decode_call(encoded)
#' 
#' #fun_remote <- rlang::call_fn(decoded)
#' #arg_remote <- rlang::call_args(decoded)
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
print_ocpu_call <- function(fun, args){
  verbose <- function(FUN, ARGS){
    f0 <- function(i, ll){
      nam <- names(ll)[i]
      cls <- class(ll[[nam]])[1]
      len <- length(ll[[nam]])
      
      l0 <- paste0("name  : ", nam)
      l1 <- paste0("pos   : {", i, "}")
      l2 <- paste0("class : ", cls)
      l3 <- paste0("length: ", len)
      
      if(is.list(ll[[nam]]) | is.data.frame(ll[[nam]])){
        vec <- capture.output(str(ll[[nam]]))[-1]
        l4 <- vec[!stringr::str_detect(vec, "attr")]
      }else{
        top <- paste0(stringr::str_trunc(head(ll[[nam]]), 10), collapse = " | ")
        if(len > 6)
          top <- paste0(top, " | ...")
        l4 <- paste0("Head  : ", top)
      }
      c(l0, l1, l2, l3, l4)
    }
    
    S <- function(SYM, SEP) paste0("\n", SYM, SEP)
    W1 <- function(x, f, SYM, ...) paste0("\n", SYM, " ", f(x, ...))
    W2 <- function(x, SEP, W1, ...) c(S(SYM, SEP), W1(x, ...))
    
    SEP <- "----------------------------"
    
    head_lab1 <- "CALL ARGUMENTS"
    SYM  <- "|+|"
    SYMH <- paste0(SYM, "@@@ ")
    args <- c(S(SYMH, head_lab1), lapply(1:length(args), W2, SEP, W1, f=f0, SYM=SYM, ll=args))
    
    head_lab2 <- "FUNCTION BODY"
    SYM <- '|=|'
    SYMH <- paste0(SYM, "@@@ ")
    fun_txt <- rlang::expr_deparse(fun)
    fun <- c(S(SYMH, head_lab2), W2(unlist(lapply(fun_txt, W1, I, SYM)), SEP, I))
    
    return(c(list(fun),  args))
  }
  cat(paste0(unlist(verbose(fun, args)), collapse = ""))
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



