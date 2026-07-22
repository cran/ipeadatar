## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# install.packages("ipeadatar")

## -----------------------------------------------------------------------------
# pak::pak("ipea/ipeadatar")

## -----------------------------------------------------------------------------
# library(ipeadatar)

## -----------------------------------------------------------------------------
# series <- available_series(label = FALSE)
# head(series)

## -----------------------------------------------------------------------------
# series_br <- available_series(language = "br", label = FALSE)
# head(series_br)

## -----------------------------------------------------------------------------
# subjects <- available_subjects(label = FALSE)
# head(subjects)

## -----------------------------------------------------------------------------
# territories <- available_territories(label = FALSE)
# head(territories)

## -----------------------------------------------------------------------------
# search_series(terms = "inflation", label = FALSE)

## -----------------------------------------------------------------------------
# search_series(
#   terms = "inflação",
#   language = "br",
#   label = FALSE
# )

## -----------------------------------------------------------------------------
# search_series(label = FALSE)

## -----------------------------------------------------------------------------
# meta <- metadata(
#   code = "PRECOS12_IPCA12",
#   label = FALSE
# )
# 
# meta

## -----------------------------------------------------------------------------
# meta <- metadata(
#   code = c("PRECOS12_IPCA12", "BM12_TJCDI12"),
#   label = FALSE
# )
# 
# meta

## -----------------------------------------------------------------------------
# ipca <- ipeadata(
#   code = "PRECOS12_IPCA12",
#   label = FALSE
# )
# 
# head(ipca)

## -----------------------------------------------------------------------------
# ipca <- ipeadata(
#   code = "PRECOS12_IPCA12",
#   label = FALSE
# )
# 
# plot(
#   ipca$date,
#   ipca$value,
#   type = "l",
#   xlab = "Date",
#   ylab = "Value",
#   main = "IPCA"
# )

## -----------------------------------------------------------------------------
# series <- available_series(label = TRUE)

## -----------------------------------------------------------------------------
# series <- available_series(label = FALSE)

## -----------------------------------------------------------------------------
# available_series(label = FALSE)
# available_subjects(label = FALSE)
# available_territories(label = FALSE)
# search_series(terms = "rural", label = FALSE)
# metadata(code = "PRECOS12_IPCA12", label = FALSE)
# ipeadata(code = "PRECOS12_IPCA12", label = FALSE)

