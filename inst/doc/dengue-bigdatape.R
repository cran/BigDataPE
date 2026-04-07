## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----libraries----------------------------------------------------------------
# library(BigDataPE)
# library(dplyr)
# library(tibble)

## ----store-token--------------------------------------------------------------
# bdpe_store_token("dengue", "your-token-here")
# #> ✔ Token stored in environment variable: `BigDataPE_dengue`

## ----list-tokens--------------------------------------------------------------
# bdpe_list_tokens()
# #> [1] "dengue"

## ----basic-fetch--------------------------------------------------------------
# data <- bdpe_fetch_data("dengue", limit = 100, offset = 0)
# glimpse(data)
# #> Rows: 100
# #> Columns: 126
# #> $ nu_notificacao           <chr> "3517726", "3613049", "3507055", ...
# #> $ tp_notificacao           <chr> "2", "2", "2", ...
# #> $ co_cid                   <chr> "A90", "A90", "A90", ...
# #> $ dt_notificacao           <chr> "2020-07-07", "2020-06-27", "2020-01-03", ...
# #> $ ds_semana_notificacao    <chr> "202028", "202026", "202001", ...
# #> $ notificacao_ano          <chr> "2020", "2020", "2020", ...
# #> $ co_municipio_notificacao <chr> "261160", "260790", "261160", ...
# #> $ tp_sexo                  <chr> "M", "M", "M", ...
# #> $ febre                    <chr> "1", "1", "1", ...
# #> $ mialgia                  <chr> "1", "2", "1", ...
# #> $ cefaleia                 <chr> "2", "1", "1", ...
# #> $ ...

## ----query-year---------------------------------------------------------------
# dengue_2020 <- bdpe_fetch_data(
#   "dengue",
#   limit = 50,
#   offset = 0,
#   query = list(notificacao_ano = "2020")
# )
# nrow(dengue_2020)
# #> [1] 50

## ----query-municipality-------------------------------------------------------
# dengue_recife <- bdpe_fetch_data(
#   "dengue",
#   limit = 100,
#   offset = 0,
#   query = list(co_municipio_residencia = "261160")
# )
# nrow(dengue_recife)
# #> [1] 100

## ----query-combined-----------------------------------------------------------
# dengue_female_recife <- bdpe_fetch_data(
#   "dengue",
#   limit = 100,
#   offset = 0,
#   query = list(
#     co_municipio_residencia = "261160",
#     tp_sexo = "F"
#   )
# )
# nrow(dengue_female_recife)

## ----chunks-------------------------------------------------------------------
# all_data <- bdpe_fetch_chunks(
#   "dengue",
#   total_limit = Inf,
#   chunk_size = 500,
#   verbosity = 1
# )
# #> ℹ Fetched 500 records (total: 500).
# #> ℹ Fetched 500 records (total: 1000).
# #> ℹ Fetched 9 records (total: 1009).
# #> ✔ Fetching complete: 1009 records retrieved.
# 
# dim(all_data)
# #> [1] 1009  126

## ----dist-sex-----------------------------------------------------------------
# all_data |>
#   count(tp_sexo, sort = TRUE)
# #> # A tibble: 3 x 2
# #>   tp_sexo     n
# #>   <chr>   <int>
# #> 1 F         561
# #> 2 M         443
# #> 3 I           5

## ----dist-year----------------------------------------------------------------
# all_data |>
#   count(notificacao_ano, sort = TRUE)
# #> # A tibble: 1 x 2
# #>   notificacao_ano     n
# #>   <chr>           <int>
# #> 1 2020             1009

## ----symptoms-----------------------------------------------------------------
# symptoms <- c("febre", "mialgia", "cefaleia", "exantema", "vomito",
#               "nausea", "dor_costas", "conjutivite", "artrite",
#               "artralgia", "dor_retro")
# 
# all_data |>
#   summarise(across(all_of(symptoms), ~ mean(.x == "1", na.rm = TRUE))) |>
#   tidyr::pivot_longer(everything(),
#                       names_to  = "symptom",
#                       values_to = "proportion") |>
#   arrange(desc(proportion))
# #> # A tibble: 11 x 2
# #>    symptom      proportion
# #>    <chr>             <dbl>
# #>  1 febre             0.914
# #>  2 cefaleia          0.531
# #>  3 mialgia           0.512
# #>  4 artralgia         0.194
# #>  5 exantema          0.181
# #>  6 dor_costas        0.155
# #>  7 vomito            0.150
# #>  8 nausea            0.139
# #>  9 dor_retro         0.125
# #> 10 conjutivite       0.053
# #> 11 artrite           0.046

## ----neighbourhoods-----------------------------------------------------------
# all_data |>
#   filter(no_bairro_residencia != "") |>
#   count(no_bairro_residencia, sort = TRUE) |>
#   head(10)
# #> # A tibble: 10 x 2
# #>    no_bairro_residencia     n
# #>    <chr>                <int>
# #>  1 IBURA                  46
# #>  2 VARZEA                 37
# #>  3 COHAB                  33
# #>  4 BOA VIAGEM             30
# #>  5 AGUA FRIA              27
# #>  ...

## ----hospitalisations---------------------------------------------------------
# all_data |>
#   count(st_ocorreu_hospitalizacao) |>
#   mutate(description = case_match(
#     st_ocorreu_hospitalizacao,
#     "1" ~ "Yes",
#     "2" ~ "No",
#     "9" ~ "Unknown",
#     ""  ~ "Not reported"
#   ))
# #> # A tibble: 4 x 3
# #>   st_ocorreu_hospitalizacao     n description
# #>   <chr>                     <int> <chr>
# #> 1                             330 Not reported
# #> 2 1                            51 Yes
# #> 3 2                           565 No
# #> 4 9                            63 Unknown

## ----get-token----------------------------------------------------------------
# my_token <- bdpe_get_token("dengue")

## ----remove-token-------------------------------------------------------------
# bdpe_remove_token("dengue")
# #> ✔ Token successfully removed for dataset: "dengue"

