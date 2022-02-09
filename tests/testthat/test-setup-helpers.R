test_that("database configuration", {
  # load example data
  load(file = test_path("db_conf_example.Rdata"))

  # do I get a list
  expect_type(
    get_db_conf(db_name = "test_db",
                db_root = test_path("example_files")),
    type = "list"
  )

  # is it the same list as it should be
  expect_equal(
    get_db_conf(db_name = "test_db",
                db_root = test_path("example_files")),
    db_conf_example
  )
})
