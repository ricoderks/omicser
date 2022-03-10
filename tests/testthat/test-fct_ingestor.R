# load needed libraries
require(anndata)

test_that("database conf/def", {
  # load example data
  load(file = test_path("conf_def_example.Rdata"))

  # do I get a list
  expect_type(
    gen_config_table(ad_in = anndata::read_h5ad(filename = test_path("example_files/test_db/db_data.h5ad")),
                     db_name = "test_db",
                     db_root_path = test_path("example_files")),
    type = "list"
  )

  # is it the same list as it should be
  expect_equal(
    gen_config_table(ad_in = anndata::read_h5ad(filename = test_path("example_files/test_db/db_data.h5ad")),
                     db_name = "test_db",
                     db_root_path = test_path("example_files")),
    conf_def
  )
})
