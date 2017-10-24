#' Make authors
#'
#' Cat the authors in latex-ready format
#' @return Latex text
#' @param authors A dataframe with columns: name, email, affiliation, footnote
#' @param institutions A dataframe with columns: code, institution, address
#' @param cat_it Whether to cat (as opposed to return)
#' @param include_address Whether to include the address
#' @param include_country Whether to include the country; only relevant if include_address is FALSE
#' @param seperator What to put between author names: usually a comma or line break
#' @export

make_authors <- function(

  authors = data.frame(name = c('Joe Brew',
                                'Kizito Gondo',
                                'Elton Dorkin',
                                'Eduardo Nhamahanga',
                                'Menno Pradhan',
                                'Laia Cirera',
                                'Ranjeeta Thomas',
                                'Elisa Sicuri'),
                       email = c('joe@databrew.cc',
                                 'KGondo@illovo.co.za',
                                 'EDorkin@illovo.co.za',
                                 'ENhamahanga@illovo.co.za',
                                 'm.p.pradhan@vu.nl',
                                 'laia.cirera@isglobal.org',
                                 'ranjeeta.thomas@imperial.ac.uk',
                                 'elisa.sicuri@isglobal.org'),
                       affilitation = c("isglobal,cism,vu",
                                        'ma',
                                        'ma',
                                        'ma',
                                        "vu,uva",
                                        'isglobal,cism',
                                        "icl",
                                        "isglobal,cism,icl"),
                       footnote = c("Corresponding Author",
                                    rep('', 7)),
                       stringsAsFactors = FALSE),

  institutions = data.frame(code = c('isglobal',
                                     'icl',
                                     'cism',
                                     'vu',
                                     'uva',
                                     'ma'),
                            institution = c('Barcelona Institute for Global Health',
                                            'Imperial College London',
                                            'Centro de Investigação em Saúde de Manhiça',
                                            'VU University Amsterdam',
                                            'University of Amsterdam',
                                            'Maragra Açucar SA, Subsidiary of Illovo Sugar Ltd'),
                            country = c('Spain', 'UK', 'Mozambique', 'Netherlands', 'Netherlands', 'Mozambique'),
                            address = c('c/ Rosselló, 132, 5è 2a. 08036, Barcelona, Spain',
                                        'South Kensington Campus, London SW7 2AZ, U.K.',
                                        'Vila da Manhiça, Bairro Cambeve, Rua 12, Distrito da Manhiça, CP 1929, Maputo, Mozambique',
                                        'De Boelelaan 1105, 1081 HV Amsterdam, Netherlands',
                                        'REC E, Roetersstraat 11, Amsterdam, Netherlands',
                                        'CP 2789, Maputo, Mozambique'),
                            stringsAsFactors = FALSE),
  cat_it = TRUE,
  include_address = TRUE,
  include_country = FALSE,
  seperator = '\n'){

  out <- c()
  for (i in 1:nrow(authors)){
    this_author <- authors$name[i]
    these_institutions <- authors$affilitation[i]
    these_institutions <- unlist(strsplit(these_institutions, ','))
    author_text <- paste0(this_author)
    for (j in 1:length(these_institutions)){
      this_institution <- these_institutions[j]
      if(any(grepl(this_institution, out))){
        author_text <- paste0(author_text,
                              '\\footrecall{',
                              this_institution,
                              '}'
        )
      } else {
        # new institution, get full name
        full_name <- institutions$institution[institutions$code == this_institution]
        address <- institutions$address[institutions$code == this_institution]
        country <- institutions$country[institutions$code == this_institution]
        author_text <- paste0(author_text,
                              '\\footremember{',
                              this_institution,
                              '}{',
                              full_name,
                              ifelse(include_address, paste0(': ', address), ''),
                              ifelse(include_country, paste0(', ', country), ''),
                              '}')
      }
    }
    out[i] <- author_text
  }
  out <- paste0(out, collapse = seperator)
  # cat(out)
  if(cat_it){
    cat(out)
  } else {
    return(out)
  }
}
