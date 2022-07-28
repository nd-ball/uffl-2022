library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)

baseurl <- "https://www.uffl.org/pastproceedings.html"

base_page <- read_html(baseurl)


conf_urls <- base_page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  filter(
    str_detect(value, "(vol)|(lifelearning)")
) %>%
  mutate(
    value = str_replace(value, "(http://www.uffl.org/)|(http://uffl.org/)", "")
  ) %>%
  mutate(
    value = str_glue("http://uffl.org/{value}")
  )


download_pdfs <- function(conf_page){
  D <- read_html(conf_page)
  pdf_urls <- D %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    filter(
      str_detect(value, "(pdf)")
    ) %>%
    filter(
      !str_detect(value, "(UFL_2015_DeavelC)|(contributors)|(acknowledgements)|(UFL_boards)|(intro05)|(wilkins05)")
    ) %>%
    mutate(
    value = str_replace(
      value, "http://www.uffl.org", "")
    ) %>%
    mutate(
      value = str_glue("http://uffl.org/{value}")
    )
  return(pdf_urls)
}

pdf_urls <- sapply(
  conf_urls$value, 
  download_pdfs,
  simplify=TRUE
) %>%
    flatten()  %>%
    unique()


# run this and keep finding errors, or implement tryCatch or purrr?

for (url in pdf_urls){
    write(url, "urls.txt", append=TRUE)
#    download.file(url, destfile = basename(url), mode = "wb")
    }


lapply(pdf_urls, write, "urls.txt", append=TRUE, ncolumns=1000)
