test_that("a abbreviated named list can be provided from key(s)", {
  test_key <- "ssh-rsa averylongstring"
  test_name <- "ssh-rsa ...ngstring"
  expect_equal(names(select_list_pubkey(test_key)), test_name)

})

# rest of test on db export will have to be performed where a db is present,
# a valid shiny session object and somewhere to do logging
with_envvar(new = c("R_RAP_CONFIG_PATH" = tempdir()), {

  file.copy(system.file(c("rapbaseConfig.yml"), package = "rapbase"),
            Sys.getenv("R_RAP_CONFIG_PATH"), overwrite = TRUE)

  ## shiny session object
  session <- list()
  attr(session, "class") <- "ShinySession"

  ## db
  # Database infrastructure is only available at gh actions and our own dev env.
  # Tests running on other environments should be skipped
  checkDb <- function(is_test_that = TRUE) {
    if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
      NULL
    } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
      NULL
    } else {
      if (is_test_that) {
        testthat::skip("Possible lack of database infrastructure")
      } else {
        1
      }
    }
  }

  test_that("env vars needed for testing is present", {
    checkDb()
    expect_true("DB_HOST" %in% names(Sys.getenv()))
    expect_true("DB_USER" %in% names(Sys.getenv()))
    expect_true("DB_PASS" %in% names(Sys.getenv()))
  })

  # prep db for testing
  if (is.null(checkDb(is_test_that = FALSE))) {
    con <- RMariaDB::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("DB_HOST"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASS"),
      bigint = "integer"
    )
    RMariaDB::dbExecute(con, "CREATE DATABASE rapmod;")
    RMariaDB::dbDisconnect(con)
  }

  if (is.null(checkDb(is_test_that = FALSE))) {
    query <- c(
      "USE rapmod;",
      paste(
        "CREATE TABLE testTable (id int, someText varchar(50),",
        "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
        "someTime DATETIME);"
      )
    )
    drv <- RMariaDB::MariaDB()
    con <- RMariaDB::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("DB_HOST"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASS"),
      bigint = "integer"
    )
    for (q in query) {
      tmp <- DBI::dbExecute(con, q)
    }
    DBI::dbDisconnect(con)
  }

  # make temporary config
  reg_name <- "rapbase"
  test_config <- paste0(
    "rapbase:",
    "\n  host : ", Sys.getenv("DB_HOST"),
    "\n  name : rapmod",
    "\n  user : ", Sys.getenv("DB_USER"),
    "\n  pass : ", Sys.getenv("DB_PASS"),
    "\n  disp : ephemaralUnitTesting\n"
  )
  # preserve initial state
  cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
  writeLines(test_config, cf)
  close(cf)


  test_that("an existing file name is provided", {
    checkDb()
    f <- export_db(reg_name, compress = TRUE, session = session)
    expect_true(file.exists(f))
  })


  # The remaining test the corresponding shiny modules
  test_that("export ui returns a shiny tag list", {
    expect_true("shiny.tag.list" %in% class(export_ui("id")))
  })

  # To recreate the stored responses delete the 'gh_api_response' directory
  # recursively and re-run these tests

  with_mock_dir("gh_api_response", {

    test_that("module server provides sensible output", {
      #skip("Reqires API authentication")
      checkDb()
      shiny::testServer(
        export_server,
        args = list(registry_name = "rapbase"), {
          expect_equal(class(output$pid), "list")
          session$setInputs(pid = "areedv")
          expect_equal("character", class(pubkey()))
          session$setInputs(key = pubkey())
          expect_equal(class(output$key), "list")
          session$setInputs(compress = FALSE)
          expect_true(length(encFile()) == 1)
          session$setInputs(download = 1)
          expect_true(basename(output$download_data) == basename(encFile()))
        })
    })

    test_that("download is prevented when module is not eligible", {
      #skip("Reqires API authentication")
      checkDb()
      shiny::testServer(
        export_server,
        args = list(registry_name = reg_name, eligible = FALSE), {
          session$setInputs(pid = "areedv")
          session$setInputs(key = pubkey())
          session$setInputs(compress = TRUE)
          #session$setInputs(exportEncrypt = 1)
          expect_false(exists("output$download"))
        })
    })
  })


  test_that("guide test app returns an app object", {
    expect_equal(class(export_app()), "shiny.appobj")
  })

  # remove test db
  if (is.null(checkDb(is_test_that = FALSE))) {
    con <- rapbase::rapOpenDbConnection(reg_name)$con
    RMariaDB::dbExecute(con, "DROP DATABASE rapmod;")
    rapbase::rapCloseDbConnection(con)
  }

})
