library(sodium)
library(protolite)

apikey <- "018ec6a3d3927f773a0d1b0adf516fa36b300929"
host <- "http://23.124.72.213"


hash_apikey <- sodium::hash(sodium::hex2bin(apikey))

bin_srv_url <- protolite::serialize_pb(host)
bin_lib_url <- protolite::serialize_pb("/ocpu/user/hpds/library/api/R/")

ciph_srv_url <- sodium::data_encrypt(bin_srv_url, hash_apikey)
ciph_lib_url <- sodium::data_encrypt(bin_lib_url, hash_apikey)

nonce_srv <- attr(ciph_srv_url, "nonce")
nonce_lib <- attr(ciph_lib_url, "nonce")

attr(ciph_srv_url, "nonce") <- NULL
attr(ciph_lib_url, "nonce") <- NULL

ciph_srv_hex  <- sodium::bin2hex(ciph_srv_url)
ciph_lib_hex  <- sodium::bin2hex(ciph_lib_url)
nonce_srv_hex <- sodium::bin2hex(nonce_srv)
nonce_lib_hex <- sodium::bin2hex(nonce_lib)



ocpu_pubx   <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
radix_pubx  <- "4db944c28272dea4145bc9164d20d270088ae662c8a0424942576fb765f63c5e"
client_pubx <- "1459e377f0b9192f924b7636c11f2f803400da25fc856c5f17093e324b3acf0c"



# save to package ---------------------------------------------------------



usethis::use_data(
                  ocpu_pubx, 
                  radix_pubx, 
                  ciph_srv_hex, 
                  ciph_lib_hex, 
                  nonce_srv_hex, 
                  nonce_lib_hex, 
                  client_pubx,
                  internal = TRUE, 
                  overwrite = TRUE)
