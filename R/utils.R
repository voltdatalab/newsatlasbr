###################################################################################################

# Get Atlas da Noticia's API from user and password informed in atlasbr_signin()

atlasbr_key <- function (){

  key <- Sys.getenv("ATLAS_DA_NOTICIA_API_TOKEN")

  if (key == "") {

    stop("There's no key available. Please check your sign-in information.\nIf you haven't included an authorized e-mail and password in this R session yet, please do so using the atlasbr_signin() function")

  } else

    return(key)

}

###################################################################################################

# Get token from Atlas da Noticia's API

get_token_atlasbr <- function(){

  post_atlas <- httr::POST("https://api.atlas.jor.br/api/v1/auth/login",
                           body = list(email = Sys.getenv("email_atlasdanoticia"),
                                       password = Sys.getenv("password_atlasdanoticia")))

  post_atlas <- httr::content(post_atlas, as = "text", encoding = "utf8")

  access_atlas <- jsonlite::fromJSON(post_atlas)[[1]]

  accesstoken_atlas <- paste("Bearer", access_atlas)

  if(accesstoken_atlas != "Bearer Unauthorized"){

    Sys.setenv("ATLAS_DA_NOTICIA_API_TOKEN" = accesstoken_atlas)

  } else

    warning("These credentials do not correspond to News Atlas' records. Please check your e-mail and password.
  If you need a new password, send an e-mail to contato@atlas.jor.br and Atlas' team will provide a new one.")

}

###################################################################################################

# Extract data from Atlas da Noticia's API

extract_data_atlasbr_api <- function(link){

  atlas_request <- httr::GET(link,
                             httr::add_headers(Authorization = atlasbr_key()))

  atlas_request_data <- httr::content(atlas_request, as = "text", encoding = "utf8")

  banco <- jsonlite::fromJSON(atlas_request_data)

  if(class(banco) != "data.frame"){

    get_token_atlasbr()

    atlas_request <- httr::GET(link,
                               httr::add_headers(Authorization = atlasbr_key()))

    atlas_request_data <- httr::content(atlas_request, as = "text", encoding = "utf8")

    banco <- jsonlite::fromJSON(atlas_request_data)

  } else

    banco <- jsonlite::fromJSON(atlas_request_data)

}

###################################################################################################

# Quiet (suggested in: https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html)

quiet <- function(x) {

  sink(tempfile())
  on.exit(sink())
  invisible(force(x))

}

###################################################################################################

# Calculates percentages from a ratio

percentages <- function(x) round(x*100, digits = 2)

###################################################################################################

# Exports graphs and plots

export_graph <- function(function_name){

  ggplot2::ggsave(paste0(getwd(), "/", function_name, ".jpg"), dpi = 300)
  message(paste("\nImage saved at", getwd(), "folder."))

}

