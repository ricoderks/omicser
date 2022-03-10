.onLoad <- function(libname = find.package("omicser"), pkgname = "omicser") {
  # CRAN Notes avoidance
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        # gen_config_table
        "UI",
        "field",
        # make_cx_heatmap
        "dist",
        # mod_og_diff_expr_server
        "test_type",
        "versus",
        "f",
        "significant",
        "point_color",
        "neglogpval",
        "ID",
        # mod_pg_table_server
        "grouping",
        "reference",
        "logfoldchanges",
        "scores",
        "pvals",
        "pvals_adj",
        "group",
        "where", # from tidyselect
        # mod_side_selector_server
        "grp",
        # pack_anndata_from_seurat
        "sample_ID",
        "feature_name",
        # volg_ggplotly
        "de",
        "log2FoldChange",
        "pvalue",
        "diffexpressed",
        "delabel"
      )
    )
  }
  invisible()
}
