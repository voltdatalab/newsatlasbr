#' Retrieve data on Brazilian news organizations by region
#'
#' @description Retrieve detailed data on Brazilian news organizations, such as
#'   contact information, n. of employees, business models, media channels and
#'   others by the state where they are based.
#'
#' @usage organizations_region(region, news = 1, media = "all")
#'
#' @param region the name of the Brazilian region for which the user wants to
#'   retrieve data. User should choose only one region as input. Accepted inputs
#'   are "All", "North", "Northeast", "Central-West", "Southeast" or "South". "All"
#'   returns data for every news organization found by News Atlas. The user
#'   should note that \code{region = "All"} takes longer to run.
#' @param news indicates if the organization is a news organization or no.
#'   Default is set to \code{news = 1}, which retrieves only news organizations.
#'   If \code{news = 0}, retrieves other organizations. And if \code{news =
#'   "all"} retrieves all organizations.
#' @param media indicates the type of media used by the organization. Default
#'   \code{media = "all"} includes all results. The other inputs allowed are
#'   "print", "online", "radio" and "TV".
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item id: News Atlas' ID for the organization. \item
#'   nome_veiculo: news organization's name. \item media_source_id: News Atlas'
#'   data source. \item segment_id: media type ID. \item segmento: media type.
#'   \item city_id: News Atlas' internal city ID. \item municipio: municipality
#'   name. \item codmun: IBGE's (Brazilian Institute of Geography and
#'   Statistics) code for the municipality (7-digit). \item state_id: News
#'   Atlas' internal state ID. \item region: abbreviation of the state's name.
#'   \item region_id: News Atlas' internal region ID. \item regiao: name of the
#'   country's region. \item address: news organization address. \item
#'   annotations: any observations on the organization. \item email:
#'   organization's email address. \item employees_range_id: News Atlas'
#'   internal ID for the number of employees. \item num_funcionario: number of
#'   employees (categorical). \item periodicity: periodicity of the publication.
#'   \item eh_jornal: is it a news organization? "1" for yes. \item
#'   eh_site_pago: is there a paywall? "1" for yes, "0" for no ("0" includes all
#'   media). \item data_fechamento: shutdown date (in case the organization is
#'   no longer operating). \item ativo: is it active? "1" for yes, "0" for no.
#'   \item data_inclusao: date it was included into News Atlas' dataset. \item
#'   business_models: a \code{list}. Returns the organization business model.
#'   \item features: a \code{list}. Returns features of the organization. \item
#'   collaborators: a \code{list}. Returns collaborators who provided data about
#'   the organization. \item media_channels: a \code{list}. Returns the
#'   organizations' media channels (such as the website, Facebook page, Twitter
#'   profile and others). }
#'
#' @details \code{organizations_region} returns a detailed dataset on Brazilian
#'   news organizations, such as contact information, n. of employees, business
#'   models, media channels and others.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on all news organizations from the southeast region
#'
#' municipalities_with_media <- organizations_region(region = "Southeast")
#'

organizations_region <- function(region,
                                 news = 1,
                                 media = "all"){

  if(missing(region)){

    stop("Please insert a valid state in the \"region\" argument.")

  }

  message("\nExtracting data from News Atlas' API.\n \n...")

  banco <- NULL

  region <- toupper(region)

  if(region == "ALL"){

    regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

    for(regiao in regioes){

      banco_regiao <- extract_data_atlasbr_api(paste0("https://api.atlas.jor.br/api/v1/data/analytic?regiao=", regiao))

      banco <- dplyr::bind_rows(banco, banco_regiao)

      Sys.sleep(0.5)

      if(regiao == "Centro-Oeste"){message("\nPlease wait.\n")}

    }


  } else {

    if(!region %in% c("N", "NORTH", "NE", "NORTHEAST", "CO", "CW", "CENTRAL-WEST",
                      "CENTER-WEST", "SE", "SOUTHEAST", "S", "SOUTH")){

      stop("Please insert a valid state in the \"region\" argument.")

    }

    region <- ifelse(region %in% c("N", "NORTH"), "Norte", region)
    region <- ifelse(region %in% c("NE", "NORTHEAST"), "Nordeste", region)
    region <- ifelse(region %in% c("CO", "CW", "CENTER-WEST", "CENTRAL-WEST"), "Centro-Oeste", region)
    region <- ifelse(region %in% c("SE", "SOUTHEAST"), "Sudeste", region)
    region <- ifelse(region %in% c("S", "SOUTH"), "Sul", region)

    banco <- extract_data_atlasbr_api(paste0("https://api.atlas.jor.br/api/v1/data/analytic?regiao=", region))

  }

  news <- toupper(news)

  if(news != "ALL"){

    if(!news %in% c(1,0)){stop("Please check the \"news\" argument. Accepted inputs are 0, 1 or \"all\".")}

    banco <- banco %>%
      dplyr::rename(municipio = .data$cidade,
                    codmun    = .data$cod_mun_ibge,
                    uf        = .data$estado) %>%
      dplyr::filter(.data$eh_jornal == news)

  }

  banco <- banco %>%
    dplyr::mutate(segmento = tolower(stringi::stri_trans_general(.data$segmento, "latin-ascii")))

  if(media != "all"){

    media <- tolower(media)

    if(!media %in% c("print", "online", "radio", "tv")){
      stop("Please check the \"media\" argument.
       Accepted inputs are \"all\", \"print\", \"online\", \"radio\" or \"TV\".")
    }

    media <- ifelse(media == "print",  "impresso",  media)
    media <- ifelse(media == "online", "online",    media)
    media <- ifelse(media == "radio",  "radio",     media)
    media <- ifelse(media == "tv",     "televisao", media)

    banco <- dplyr::filter(banco, .data$segmento == media)

  }

  message("Done.\n")

  return(banco)

}
