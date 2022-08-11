test_that("guide UI returns a shiny tag object", {
  expect_equal(class(export_guide_ui("id")), "shiny.tag")
})

test_that("guide module server provides output", {
  shiny::testServer(export_guide_server, args = list(registry_name = "test"), {
    expect_equal(class(output$guide), "list")
  })
})

test_that("guide test app returns an app object", {
  expect_equal(class(export_guide_app()), "shiny.appobj")
})
