.onAttach <- function(...) {
  if(!interactive()){
    packageStartupMessage(
      cat(crayon::bold(glue::glue("Welcome to geoChronR version {packageVersion('geoChronR')}!")),"\n",
          glue::glue("Note that major changes to parameter names (terms within functions) were made for version 1.0.0
      If you've been using geoChronR for awhile, this will almost certainly break some of your code, sorry.
      If you're new, enjoy the newly standardized terminology!")
      )
    )
  }
}