#' Plot maps of Brazilian "almost news deserts"
#'
#' @description Plot a map of Brazilian "almost news deserts" (municipalities
#'   with 1 or 2 news organizations recorded by Atlas da Noticia's research).
#'
#' @usage almost_deserts_map(aggregation = "municipalities", export = F)
#'
#' @param aggregation aggregation (character). Can be one out of 3 levels:
#'   "municipalities", "states" or "regions" (which can also be written in
#'   Portuguese, with no accents: "municipios:, "estados" and "regioes").
#'   Default is set to \code{aggregation = "municipalities"}.
#' @param export logical. Should the map be exported as a .jpg file? Default is
#'   set to no \code{export = F}.
#'
#' @return A \code{ggplot} object with a Brazil map filled according to the
#'   municipality being an "almost news desert".
#'
#' @details \code{almost_deserts_map} returns \code{ggplot} object with a Brazil
#'   map filled according to the city being an "almost news desert". Spatial
#'   data is extracted using the \href{https://github.com/ipeaGIT/geobr}{geobr
#'   package}.
#'
#' @seealso \code{\link{news_deserts_map}} and \code{\link{n_orgs_100k_map}} for
#'   other functions that generates maps, and \code{\link{almost_deserts_map}}
#'   for a function to extract data on the "almost news deserts".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Plot a map of Brazilian almost news deserts
#'
#' almost_deserts_map()
#'
#' # Plot and export a map showing the absolute n. of municipalities
#' # that are almost news deserts in each Brazilian region
#'
#' almost_deserts_map(aggregation = "regions", export = TRUE)

almost_deserts_map <- function(aggregation = "municipalities",
                               export = F){

  if(!aggregation %in% c("municipalities", "municipios",
                         "states", "estados",
                         "regions", "regioes")) {

    stop("Please provide a valid input to the aggregation argument. The possible values are
    \"municipalities\", \"municipios\", \"states\", \"estados\", \"regions\", \"regioes\".")

  } else

    message("\nExtracting spatial data from geobr package.\n")

  if(aggregation == "municipalities" | aggregation == "municipios"){

    municipalities <- geobr::read_municipality()
    municipalities$code_muni <- as.character(municipalities$code_muni)

    cities_almost_deserts <- almost_deserts()

    cities_almost_deserts <- dplyr::left_join(municipalities, cities_almost_deserts,
                                              by = c("code_muni" = "codmun_ibge"))
    cities_almost_deserts$desert <- ifelse(is.na(cities_almost_deserts$ano), "No", "Yes")

    message("\nGenerating map.\n \n...\n")

    mapa <- ggplot2::ggplot(cities_almost_deserts) +
      ggplot2::geom_sf(size = 0.03, ggplot2::aes(color = .data$desert, fill  = .data$desert)) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.title = ggplot2::element_text(size = 10, hjust = 0.5),
                     plot.title   = ggplot2::element_text(hjust = 0.5),
                     plot.caption = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_manual(values  = c("#A9A9A9", "#000000"), guide = FALSE) +
      ggplot2::scale_fill_manual(values = c("#F5F5F5", "#FF7F50")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Almost news desert\n(1 or 2 media organizations)"))

    if(export == T){

      export_graph("cities_almost_deserts")

    }

  }

  if(aggregation == "states" | aggregation == "estados"){

  states <- geobr::read_state()

  states_df <- almost_deserts_state(regions = F)

  states_deserts <- dplyr::left_join(states, states_df,
                                     by = c("abbrev_state" = "uf"))

  message("\nGenerating map.\n \n...\n")

  mapa <- states_deserts %>%
    dplyr::mutate(qtd_quase_desertos = ifelse(is.na(.data$qtd_quase_desertos), 0, .data$qtd_quase_desertos),
                  qtd_desertos_quart = cut(.data$qtd_quase_desertos,
                                           stats::quantile(.data$qtd_quase_desertos, probs = seq(0, 1, 0.25)),
                                           include.lowest = T, dig.lab = 6)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$qtd_desertos_quart)) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_text(size = 8, hjust = 0.5),
                   plot.title   = ggplot2::element_text(hjust = 0.5),
                   plot.caption = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_manual(values = c("#feedde", "#fdbe85", "#fd8d3c", "#d94701"),
                               na.value = "darkgrey") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of almost\nnews deserts"))

    if(export == T) {

      export_graph("states_almost_deserts")

    }

  }

  if(aggregation == "regions" | aggregation == "regioes"){

    states <- geobr::read_state() %>%
      dplyr::mutate(name_region = ifelse(.data$name_region == "Centro Oeste", "Centro-Oeste", .data$name_region))

    regions_df <- news_deserts_state(regions = T)

    regions_deserts <- dplyr::left_join(states, regions_df,
                                        by = c("name_region" = "regiao"))

    message("\nGenerating map.\n \n...\n")

    mapa <- regions_deserts %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$qtd_desertos)) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.title = ggplot2::element_text(size = 8, hjust = 0.5),
                     plot.title   = ggplot2::element_text(hjust = 0.5),
                     plot.caption = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = 1,
                                    palette = "Oranges", na.value = "darkgrey") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of almost\nnews deserts"))

    if(export == T) {

             export_graph("region_almost_deserts")

      }

    }

  return(mapa)

  message("\nDone.\n")

}



