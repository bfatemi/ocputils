#' TITLE TBD
#' 
#' @param hxp TBD
#' @param width TBD
#' @param height TBD
#' @param res TBD
#' @param html TBD
#' @param fun TBD
#' @param args TBD
#' @param ui TBD
#' @param script TBD
#' @param style TBD
#' @param path TBD
#' 
#' @importFrom rlang call_fn call_args
#' @importFrom shiny plotPNG
#' @importFrom protolite unserialize_pb
#' @importFrom sodium hex2bin 
#'
#' @name api_endpoints
NULL

#' @describeIn api_endpoints TBD
#' @export
publishUI <- function(ui=NULL, script=NULL, style=NULL, path=NULL){
  url <- ocputils::ocpu_icecube_url("ui")
  
  if(is.null(path)){
    html <- tags$html(
      HTML(paste0("<head>\n", 
                  script, 
                  style, 
                  "\n</head>")),
      tags$body(ui)
    )
  }else{
    html <- htmltools::htmlTemplate(path)
  }
  txt <- deparse(as.character(html))
  
  r <- httr::POST(url, body = list(html = txt))
  r$headers$`x-ocpu-session`
}

#' @describeIn api_endpoints TBD
#' @export
view_api <- function(path){
  url <- paste0("https://icecube.hpds.network/ocpu/tmp", path)
  browseURL(url)
}


#' @describeIn api_endpoints TBD
#' @export
do_encoded <- function(fun, args){
  ocputils::printmsg("OpenCPU API Call")
  cl <- ocputils::decode_call(fun, args)
  decoded <- ocputils::decode_call(fun, args)
  fun_remote <- rlang::call_fn(decoded)
  arg_remote <- rlang::call_args(decoded)
  ocputils::printstamp("Executing function with arguments:")
  print(fun_remote)
  cat("\n")
  print(arg_remote)
  return(eval(cl))
}


#' @describeIn api_endpoints TBD
#' @export
ui <- function(html=NULL){
  writeLines(html, "ui.html")
}


#' @describeIn api_endpoints TBD
#' @export
publishPlot <- function(hxp, width, height, res){
  tryCatch({
    f <- function(){
      obj <- protolite::unserialize_pb(sodium::hex2bin(hxp))
      print(unserialize(charToRaw(obj)))
    }
    return(shiny::plotPNG(f,width,height,res,filename="plot.png"))
  }, error=function(c){
    writeLines(oops_html, "oops.html")
    return("oops.html")
  })
}