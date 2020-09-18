#' Plot maps of Brazilian "news deserts"
#'
#' @description Plot a map with information on the number of Brazilian "news
#'   deserts" (municipalities with no news organizations recorded by News Atlas'
#'   research).
#'
#' @usage news_deserts_map(aggregation = "municipalities",
#'                         percentage = F, export = F)
#'
#' @param aggregation aggregation (character). Can be one out of 3 levels:
#'   "municipalities", "states" or "regions" (which can also be written in
#'   Portuguese, with no accents: "municipios:, "estados" and "regioes").
#'   Default is set to \code{aggregation = "municipalities"}.
#' @param percentage logical. Should the map be filled with absolute number of
#'   news deserts municipalities or the percentage of them in the chosen
#'   aggregation? Default is set to return the absolute number \code{percentage
#'   = F}. If the user sets aggregation to the municipality level, this
#'   parameter cannot be set to \code{percentage = T}.
#' @param export logical. Should the map be exported as a .jpg file? Default is
#'   set to no \code{export = F}.
#'
#' @return A \code{ggplot} object with a Brazil map filled according to the
#'   number of news deserts.
#'
#' @details \code{news_deserts_maps} returns \code{ggplot} object with a Brazil
#'   map filled according to the number of "news deserts". Spatial data is
#'   extracted using the \href{https://github.com/ipeaGIT/geobr}{geobr package}.
#'   Percentages are calculated based on the total number of municipalities in
#'   each state or region. When \code{aggregation = "states"}, data are
#'   categorized by quartiles.
#'
#' @seealso \code{\link{almost_deserts_map}} and \code{\link{n_orgs_100k_map}}
#'   for other functions that generates maps, and \code{\link{news_deserts}} and
#'   \code{\link{news_deserts_state}} for functions to extract data on "news
#'   deserts".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Plot a map with the absolute number of news deserts in each Brazilian region
#'
#' news_deserts_map(aggregation = "regions", percentage = FALSE, export = FALSE)
#'
#' # Save a ggplot object into an object called states_deserts with the number of news
#' # deserts in each of them (in percentages)
#'
#' states_deserts <- news_deserts_map(aggregation = "states", percentage = FALSE, export = FALSE)
#'
#' # Plot and export a map showing the municipalities that are news deserts
#'
#' news_deserts_map(aggregation = "municipalities", export = TRUE)

news_deserts_map <- function(aggregation = "municipalities",
                             percentage = F,
                             export = F){

  if(!aggregation %in% c("municipalities", "municipios",
                         "states", "estados",
                         "regions", "regioes")) {

    stop("Please provide a valid input to the aggregation argument. The possible values are
    \"municipalities\", \"municipios\", \"states\", \"estados\", \"regions\", \"regioes\".")

  } else

  message("\nExtracting spatial data from geobr package.\n")

  if(aggregation == "municipalities" | aggregation == "municipios"){

    if(percentage == T){

      message("ATTENTION: When plotting municipalities, newsatlasbr only shows the localities which are news deserts.\n\nTherefore, it does not show percentages.\n")

      }

    municipalities <- geobr::read_municipality()
    municipalities$code_muni <- as.character(municipalities$code_muni)

    cities_deserts <- news_deserts()

    cities_deserts <- dplyr::left_join(municipalities, cities_deserts,
                                by = c("code_muni" = "codmun"))
    cities_deserts$desert <- ifelse(is.na(cities_deserts$ano), "No", "Yes")

    message("\nGenerating map.\n \n...\n")

    mapa <- ggplot2::ggplot(cities_deserts) +
      ggplot2::geom_sf(size = 0.03, ggplot2::aes(color = .data$desert,
                               fill  = .data$desert)) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(size = 10, hjust = 0.5),
            plot.title   = ggplot2::element_text(hjust = 0.5),
            plot.caption = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_manual(values  = c("#A9A9A9", "#000000"), guide = FALSE) +
      ggplot2::scale_fill_manual(values = c("#F5F5F5", "#DC143C")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "News desert"))

    if(export == T){

      export_graph("cities_deserts")

    }

  }

  if(aggregation == "states" | aggregation == "estados"){

    states <- geobr::read_state()

    states_df <- news_deserts_state(regions = F)

    states_deserts <- dplyr::left_join(states, states_df,
                                by = c("abbrev_state" = "uf")) %>%
      dplyr::mutate(percentage_deserts = percentages(.data$qtd_desertos/.data$qtd_cidades))

    message("\nGenerating map.\n \n...\n")

    if(percentage == T) {

      mapa <- states_deserts %>%
        # Insert a zero in case there's NA (usually the DF) and categorize variable in quartiles
        dplyr::mutate(percentage_deserts = ifelse(is.na(.data$percentage_deserts), 0, .data$percentage_deserts),
                      percentage_deserts_quart = cut(.data$percentage_deserts,
                                                     stats::quantile(.data$percentage_deserts, probs = seq(0, 1, 0.25)),
                                                     include.lowest = T, dig.lab = 6)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$percentage_deserts_quart)) +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(size = 8, hjust = 0.5),
              plot.title   = ggplot2::element_text(hjust = 0.5),
              plot.caption = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_manual(values = c('#fee5d9','#fcae91','#fb6a4a','#cb181d'),
                                   na.value = "darkgrey") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of news\ndeserts (in %)"))

      if(export == T) {

        export_graph("states_deserts_percentage")

      }

    }

    if(percentage == F){

      mapa <- states_deserts %>%
        dplyr::mutate(qtd_desertos = ifelse(is.na(.data$qtd_desertos), 0, .data$qtd_desertos),
                      qtd_desertos_quart = cut(.data$qtd_desertos,
                                               stats::quantile(.data$qtd_desertos, probs = seq(0, 1, 0.25)),
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
        ggplot2::scale_fill_manual(values = c('#fee5d9','#fcae91','#fb6a4a','#cb181d'),
                                   na.value = "darkgrey") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of\nnews deserts"))

      if(export == T) {

        export_graph("states_deserts")

      }

    }

  }

  if(aggregation == "regions" | aggregation == "regioes"){

    states <- geobr::read_state() %>%
      dplyr::mutate(name_region = ifelse(.data$name_region == "Centro Oeste", "Centro-Oeste", .data$name_region))

    regions_df <- news_deserts_state(regions = T)

    regions_df <- regions_df %>%
      dplyr::mutate(percentage_deserts_region = percentages(.data$qtd_desertos/.data$qtd_cidades))

    regions_deserts <- dplyr::left_join(states, regions_df,
                                 by = c("name_region" = "regiao"))

    message("\nGenerating map.\n \n...\n")

    if(percentage == T) {

      mapa <- regions_deserts %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$percentage_deserts_region)) +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(size = 8, hjust = 0.5),
              plot.title   = ggplot2::element_text(hjust = 0.5),
              plot.caption = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_distiller(type = "seq", direction = 1,
                             palette = "Reds", na.value = "darkgrey") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of news\ndeserts (in %)"))

      if(export == T) {

        export_graph("region_deserts_percentage")

      }

    }

    if(percentage == F){

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
                             palette = "Reds", na.value = "darkgrey") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title = "Number of\nnews deserts"))

    if(export == T) {

      export_graph("region_deserts")

      }

    }

  }

  return(mapa)

  message("\nDone.\n")

}

