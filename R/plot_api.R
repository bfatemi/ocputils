#' Title
#'
#' @param p TBD
#' @param hxp TBD
#' @param h TBD
#' @param w TBD
#' @param res TBD
#' 
#' @import shiny
#' @importFrom httr POST
#' @importFrom htmltools htmlTemplate
#' @importFrom sodium hex2bin bin2hex 
#' @importFrom protolite serialize_pb unserialize_pb
#' @importFrom utils browseURL
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

#' @describeIn plot_api TBD
#' @export
plotpost <- function(p, h=500, w=500, res=98){
  url <- ocputils::ocpu_icecube_url("publishPlot")
  r <- httr::POST(url, body = list(hxp = plot2hex(p), #paste0("'", plot2hex(p), "'"), 
                                   height = h, 
                                   width = w, 
                                   res = res))
  id <- r$headers$`x-ocpu-session`
  api_path <- paste0("/", id, "/files/plot.png")
  return(api_path)
}





# viewUI( publishUI("inst/login.html") )
# 
# 
# ui <- basicPage(h1("Heyyy"))
# 
# script <- includeScript("R/widget/sigma.js")
# style  <- includeCSS("R/widget/style.css")
# 
# id <- publish(ui, script, style)
# 
# viewUI(id)