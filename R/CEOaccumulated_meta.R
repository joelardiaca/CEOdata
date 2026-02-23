#' Internal cache for accumulated-series metadata
#'
#' @keywords internal
CEOaccumulated_metadata <- function() {
  if (is.null(the$CEOaccumulated_metadata)) {
    the$CEOaccumulated_metadata <- getCEOaccumulated_metadata()
  }
  the$CEOaccumulated_metadata
}

#' Download and prepare the metadata of accumulated microdata series (Dades Obertes)
#'
#' Source dataset (Socrata):
#' https://analisi.transparenciacatalunya.cat/resource/gp4k-sxxn.json
#'
#' @return A tibble with the accumulated series metadata.
#' @keywords internal
getCEOaccumulated_metadata <- function() {

  url.acc <- "https://analisi.transparenciacatalunya.cat/resource/gp4k-sxxn.json"

  acc <- tryCatch(
    jsonlite::fromJSON(url.acc),
    error = function(e) e
  )

  if (inherits(acc, "error")) {
    message(
      paste0(
        "A problem downloading the accumulated-series metadata has occurred. ",
        "The server may be temporarily down, or the API has changed. ",
        "Please try again later or open an issue at https://github.com/ceopinio/CEOdata indicating ",
        "'Problem with accumulated metadata file'.\n\n",
        "Underlying error: ", conditionMessage(acc)
      )
    )
    return(NULL)
  }

  # In this endpoint, jsonlite::fromJSON() already returns a data.frame
  if (!is.data.frame(acc)) {
    message(
      paste0(
        "Unexpected response type from accumulated-series API. ",
        "Expected a data.frame but got: ", paste(class(acc), collapse = ", ")
      )
    )
    return(NULL)
  }

  d <- tibble::as_tibble(acc)

  # Flatten nested url objects (microdades_1 and microdades_2 come as data.frames with column `url`)
  if ("microdades_1" %in% names(d)) {
    if (is.data.frame(d$microdades_1) && "url" %in% names(d$microdades_1)) {
      d$microdades_1 <- d$microdades_1$url
    } else {
      d$microdades_1 <- as.character(d$microdades_1)
    }
  } else {
    d$microdades_1 <- NA_character_
  }

  if ("microdades_2" %in% names(d)) {
    if (is.data.frame(d$microdades_2) && "url" %in% names(d$microdades_2)) {
      d$microdades_2 <- d$microdades_2$url
    } else {
      d$microdades_2 <- as.character(d$microdades_2)
    }
  } else {
    d$microdades_2 <- NA_character_
  }

  # Helper: robust date conversion for character timestamps
  to_date_safe <- function(x) {
    if (is.null(x)) return(as.Date(NA))
    if (all(is.na(x))) return(as.Date(x))

    x_chr <- as.character(x)
    x_chr <- stringr::str_sub(x_chr, 1L, 10L)  # keep YYYY-MM-DD
    as.Date(x_chr, format = "%Y-%m-%d")
  }

  # Ensure stable schema (create missing columns as NA)
  expected <- c(
    "codi_serie", "titol_serie", "mode_admin",
    "data_inici", "data_fi", "reo", "estat", "univers",
    "microdades_1", "microdades_2"
  )

  for (nm in expected) {
    if (!(nm %in% names(d))) d[[nm]] <- NA
  }

  # Type normalization
  d <- d |>
    dplyr::mutate(
      codi_serie   = as.character(codi_serie),
      titol_serie  = as.character(titol_serie),
      mode_admin   = as.character(mode_admin),
      reo          = as.character(reo),
      estat        = as.character(estat),
      univers      = as.character(univers),
      microdades_1 = as.character(microdades_1),
      microdades_2 = as.character(microdades_2),
      data_inici   = to_date_safe(data_inici),
      data_fi      = to_date_safe(data_fi)
    ) |>
    dplyr::filter(!is.na(codi_serie) & nzchar(codi_serie))

  d
}


#' Import metadata for accumulated microdata series (Dades Obertes)
#'
#' Returns the catalogue of "microdades acumulades" series published in
#' Dades Obertes (Socrata). Each row corresponds to a series, identified by
#' `codi_serie` (e.g. "BOP_presencial").
#'
#' @param series Optional character vector. If provided, filters results to those `codi_serie`.
#' @param active_only Logical. If TRUE, keeps only series marked as active (best-effort; depends on `estat` field).
#' @export
#' @return A tibble with the metadata of the accumulated microdata series.
#' @examples
#' \dontrun{
#' m <- CEOaccumulated_meta()
#' unique(m$codi_serie)
#'
#' CEOaccumulated_meta(series = "BOP_presencial")
#' }
CEOaccumulated_meta <- function(series = NULL, active_only = FALSE) {

  d <- CEOaccumulated_metadata()

  if (is.null(d)) {
    return(NULL)
  }

  if (!is.null(series)) {
    if (!is.character(series)) {
      stop("`series` must be a character vector.", call. = FALSE)
    }
    d <- d |>
      dplyr::filter(codi_serie %in% series)
  }

  if (isTRUE(active_only)) {
    # Best-effort: the portal seems to use strings like "Serie activa" / "Serie inactiva"
    d <- d |>
      dplyr::filter(stringr::str_detect(tolower(estat), "activa"))
  }

  d
}