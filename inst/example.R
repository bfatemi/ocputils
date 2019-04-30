library(httr)

a <- as.list(1:100)
b <- as.list(101:200)
c <- as.list(201:300)

names(a) <- paste0("a", a)
names(b) <- paste0("b", b)
names(c) <- paste0("c", c)

abc <- list("a" = a, "b" = b, "c" = c)