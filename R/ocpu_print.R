#' OCPU PRINT UTILITY FUNCTIONS
#' 
#' @param msg TBD
#' @param sym TBD
#' @param slim TBD
#' @param content TBD
#' @param addtime TBD
#' 
#' @importFrom stringr str_split str_c str_length str_count str_trunc
#' @importFrom crayon bgWhite bold magenta bgMagenta white make_style combine_styles bgCyan
#' 
#' @name ocpu_print
NULL


#' @describeIn ocpu_print TBD
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


#' @describeIn ocpu_print TBD
#' @export
printmsg <- function(msg, sym="+", slim = TRUE, content=NULL, addtime=TRUE){
  yellow1 <- crayon::make_style("yellow1")
  ivory <- crayon::make_style("ivory")
  bgMaroon <- crayon::make_style("maroon", bg = TRUE)
  fancy <- crayon::combine_styles(ivory, crayon::bgCyan)
  
  ## can redo this code later: first construct the middle, then just repeat sym
  ## and cutoff at length of middle.. which can vary with slim.
  
  # Will be messing with console width so grab global setting to reset later
  globscipen <- options()$width
  on.exit(options(width = globscipen))
  
  ##
  ## Get parameters for placement
  ##
  # Calibrate position by requiring length to be closest even integer to true length
  numchars  <- ceiling(stringr::str_count(msg)/2)*2
  
  # border should by some factor of twice the length of the message for centering aesthetics
  lenAdj <- ifelse(slim, 1.25, 2)
  blen <- round(numchars*lenAdj)
  
  # construct topbottom first
  topbottom <- paste0(c("\n", rep(sym, blen), "\n"), collapse="")
  
  # construct middle
  ind <- paste0(rep(" ", ceiling((blen - numchars)/2-1)), collapse="")
  middle <- paste0(sym, fancy(ind), fancy(msg), fancy(ind), sym)
  
  # if middle is shorter (likely only by 1), then adjust one side's spacing
  # not sure when this would be negative, but too tired to think so will include 'max'
  adjby <- max(0, blen - stringr::str_length(middle))
  if(adjby > 0)
    middle <- paste0(sym, fancy(ind), fancy(msg), fancy(ind), " ", sym)
  
  # final message
  trunc_topbot <- stringr::str_trunc(topbottom, stringr::str_length(middle)+2, ellipsis = "\n")
  finalmsg <- paste0(trunc_topbot, middle, trunc_topbot, collapse="")
  
  # Display - temporarily set the console width then print
  options(width=stringr::str_count(topbottom))
  cat(finalmsg)
  
  # add time if applicable
  if(addtime){
    stamp <- paste0("\n", ind, "Timestamp: ", Sys.time(), "\n")
    cat(yellow1(stamp))
  }
  
  # if content was provided, display that now
  if(!is.null(content)){
    if(class(content) %in% c("matrix", "data.frame")){
      print(content, print.gap = TRUE, quote = FALSE)
    }else{
      cat(paste0("\n", content,"\n"))
    }
  }else{
    cat("\n")
  }
}


# f <- function(...){
#   return(NULL)
# }
# 
# args <- list(a = 1, 
#              letter = letters, 
#              dt = data.table(d1 = "first", d2 = "second"), 
#              l = list(1:100))
# 
# 
# decoded <- list(FUN=f, ARGS=args)
# 
# print_ocpu_call(decoded)






