#' Shiny modules providing ui and server logic for export of registry data.
#'
#' Functions for registries that wants to implement exporting of registry
#' databases, \emph{e.g.} for local development purposes. Also includes
#' relevant helper functions.
#'
#' @param id Character string module ID
#' @param registry_name Character string registry name key
#' @param repo_name Character string defining the github repository name of the
#'   registry. Default value is \code{registry_name}.
#' @param proxy_url Character string providing an internet proxy server url.
#'   When set to NULL (default) no proxy server is assumed.
#' @param token Character string representing a GitHub Personal Access Token
#'   (PAT) that will be needed if calls to the github api requires
#'   authorization. In a development context and if the GITHUB_PAT environmental
#'   variable is set \code{token} can be left to its default (NULL). In
#'   production-like environments \code{token} should always be explicitly set.
#' @param eligible Logical defining if the module should be allowed to work at
#'   full capacity. This might be useful when access to module products should
#'   be restricted. Default is TRUE, \emph{i.e.} no restrictions.
#' @param pubkey Character vector with public keys
#' @param compress Logical if export data is to be compressed (using gzip).
#'   FALSE by default.
#' @param session Shiny session object
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name export
#' @aliases export_ui export_server export_app select_list_pubkey export_db
#' @examples
#' ## client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(
#'       export_ui("test"),
#'     ),
#'     shiny::mainPanel(
#'       NULL
#'     )
#'   )
#' )
#'
#' ## server function
#' server <- function(input, output, session) {
#'   export_server("test", registry_name = "rapbase")
#' }
#'
#' ## run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

# shiny modules
#' @rdname export
#' @export
export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pid")),
    shiny::uiOutput(ns("key")),
    shiny::checkboxInput(ns("compress"), "Komprimer eksport"),
    shiny::uiOutput(ns("download"))
  )
}

#' @rdname export
#' @export
export_server <- function(id, registry_name, repo_name = registry_name,
                          proxy_url = NULL, token = NULL, eligible = TRUE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      pubkey <- shiny::reactive({
        shiny::req(input$pid)
        keys <- get_github("keys", input$pid)
        sship::pubkey_filter(keys, "rsa")
      })

      encFile <- shiny::reactive({
        f <- export_db(
          registry_name,
          compress = input$compress,
          session = session
        )
        message(paste("Dump file size:", file.size(f)))
        ef <- sship::enc(f, pid = NULL, pubkey_holder = NULL,
                         pubkey = input$key)
        ef
      })

      if (eligible) {
        output$download_data <- shiny::downloadHandler(
          filename = function() {
            basename(encFile())
          },
          content = function(file) {
            file.copy(encFile(), file)
            rapbase::repLogger(
              session,
              msg = paste("Db export file", basename(encFile()), "downloaded.")
            )
          }
        )
      }

      ## UC
      output$pid <- shiny::renderUI({
        shiny::selectInput(
          ns("pid"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
          ),
          choices = get_github(
            what = "members",
            value = repo_name,
            proxy_url = proxy_url,
            token = token
          )
        )
      })
      output$key <- shiny::renderUI({
        if (length(pubkey()) == 0) {
          shiny::p("No keys found!")
        } else {
          shiny::selectInput(
            ns("key"),
            label = shiny::tags$div(
              shiny::HTML(as.character(shiny::icon("key")), "Velg \u00f8kkel:")
            ),
            choices = select_list_pubkey(pubkey()))
        }
      })
      output$download <- shiny::renderUI({
        if (!eligible | length(pubkey()) == 0) {
          shiny::tagList(
            shiny::hr(),
            shiny::h4("Funksjon utilgjengelig"),
            shiny::p("Kontakt registeret")
          )
        } else {
          shiny::tagList(
            shiny::hr(),
            shiny::downloadButton(
              ns("download_data"),
              label = "Last ned!")
          )
        }
      })

    })
}

#' @rdname export
#' @export
export_app <- function(registry_name = "rapbase") {
  ui <- shiny::fluidPage(
    export_ui("export")
  )
  server <- function(input, output, session) {
    export_server("export", registry_name)
  }

  shiny::shinyApp(ui, server)
}


#' Shiny modules providing the Export Guide
#'
#' @param id Character string module ID
#' @param registry_name Character string registry name key
#'
#' @return Functions ui and server representing the (module) app
#' @name export_guide
#' @aliases export_guide_ui export_guide_server export_guide_app
#' @examples
#' ui <- shiny::fluidPage(
#'   export_guide_ui("guide")
#' )
#'
#' server <- function(input, output, session) {
#'   export_guide_server("guide", "test")
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname export_guide
#' @export
export_guide_ui <- function(id) {

  shiny::htmlOutput(shiny::NS(id, "guide"))
}

#' @rdname export_guide
#' @export
export_guide_server <- function(id, registry_name) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

    output$guide <- shiny::renderUI({
      rapbase::renderRmd(
        sourceFile = system.file("export_guide.Rmd", package = "rapmod"),
        outputType = "html_fragment",
        params = list(registry_name = registry_name)
      )
    })
  })
}

#' @rdname export_guide
#' @export
export_guide_app <- function() {

  ui <- shiny::fluidPage(
    export_guide_ui("guide")
  )

  server <- function(input, output, session) {
    export_guide_server("guide", "test")
  }

  shiny::shinyApp(ui, server)
}


# helper functions

#' @rdname export
#' @export
select_list_pubkey <- function(pubkey) {

  list_name <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  list_name <- paste0(substr(pubkey, 1, 8), "...", list_name)
  names(pubkey) <- list_name

  as.list(pubkey)

}

#' @rdname export
#' @export
export_db <- function(registry_name, compress = FALSE, session) {

  stopifnot(Sys.which("mysqldump") != "")
  stopifnot(Sys.which("gzip") != "")

  f <- tempfile(pattern = registry_name, fileext = ".sql")
  conf <- rapbase::getConfig()[[registry_name]]
  cmd <- paste0("mysqldump --column-statistics=0 ",
                "--no-tablespaces --single-transaction --add-drop-database ")
  cmd <- paste0(cmd, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
                " ", conf$name, " > ", f)
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    invisible(system(cmd))
  }

  rapbase::repLogger(session, msg = paste(registry_name, "Db dump created."))

  invisible(f)
}


#' Collect various data from the GitHub API
#'
#' @param what Character string defining the api endpoint. Currently one of
#'   \code{c("contributors", "members", "keys")}.
#' @param value Character string specifying what to collect from the given
#'   endpoint. For "contributors" this should be the name of the repository, for
#'   "members" value should be the team slug and for "keys" this should be a
#'   github user name.
#' @param proxy_url Character string providing an internet proxy server url.
#'   When set to NULL (default) no proxy server is assumed.
#' @param token Character string providing av valid token that will be used if
#'   the api call requires authentication. Listing of team members do require a
#'   token with the appropriate credentials. When NULL (default) no token will
#'   be used.
#'
#' @return Character vector with results from the GitHub api request
#' @export

get_github <- function(what, value, proxy_url = NULL, token = NULL) {

  stopifnot(what %in% c("contributors", "members", "keys"))

  if (what %in% c("contributors")) {
    endpoint <- paste0("/repos/rapporteket/", value, "/contributors")
    vName <- "login"
  }

  if (what %in% c("members")) {
    endpoint <- paste0("/orgs/rapporteket/teams/", value, "/members")
    vName <- "login"
  }

  if (what %in% c("keys")) {
    endpoint <- paste0("/users/", value, "/keys")
    vName <- "key"
  }

  sship::gh(
    path = endpoint, proxy_url = proxy_url, token = token
  )$content[[vName]]
}
