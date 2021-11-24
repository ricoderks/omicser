#==== INFO ========================================================
## code to prepare `yassene_example` dataset goes here
##
##  "WHAT IS THE CONTEXT?" lipidomics composition data
##  - data from yassenne / ricoo
##
#  formely `yassene_A` (conc and compos)

#==== 0. preamble/setup ==================================================

#  Step 1: Set paths--------------
#  path where the omicser repo is cloned to (see install_scrip.R)
REPO_PATH <- getwd() # e.g. "~/Projects/NDCN_dev/omicser"
OMICSER_RUN_DIR <- REPO_PATH # file.path(REPO_PATH,"quickstart")

# where are the raw data files for this script?
RAW_DATA_DIR <- file.path(OMICSER_RUN_DIR, "examples/raw_data/rico_data")
# set where we will put the database folder
DB_ROOT_PATH <- file.path(OMICSER_RUN_DIR,"examples/test_db")

# what will we call the database
DB_NAME <-  list("Rico lipids" = "rico_lipids")


# name your python envirnoment
OMICSER_PYTHON <- ".pyenv"# "pyenv_omicser"


# Step 2:  Load the omicser package
if (CLONED_OMICSER <- TRUE){
  # this should be a full path... e.g. ~/Projects/NDCN_dev/omicser
  # but for github, we will set relative to the repo BASE
  golem::document_and_reload(pkg = REPO_PATH)
} else {
  require(omicser)
}


#==== 2. helper functions =================================================================================
prep_lipidizer_files <- function(data_file, path_root){
  # read the file
  raw_table <- data.table::fread(file = file.path(path_root, data_file),
                                 header = TRUE)

  # remove empty groups, in this case this is the QC
  raw_table <- raw_table[raw_table$Group != "", ]
  # get the group information of the samples
  exp_group <- raw_table$Group

  # observation annotations
  meta_cols <- c("Name", "Group")
  obs_meta <- raw_table[, c("Name", "Group")]
  obs_meta$Group <- exp_group
  rownames(obs_meta) <- raw_table$Name

  # create the matrix
  dat_mat <- raw_table[, !c("Name", "Group")]
  dat_mat <- as.matrix(dat_mat)
  rownames(dat_mat) <- obs_meta$Name

  class(dat_mat) <- "numeric"

  # keep the original data
  raw <- dat_mat
  # zero out NA: is this tricky because its sparse?
  dat_mat[which(is.na(dat_mat), arr.ind = TRUE)] <- 0

  # make variable annotations (lipids)
  lipids <- colnames(dat_mat)
  var_annot <- as.data.frame(lipids)
  rownames(var_annot) <- var_annot$lipids
  # and add the lipid class
  var_annot$lipid_class <- stringr::str_extract(string = var_annot$lipids,
                                                pattern = "^[a-zA-Z]*")

  ####  marginals on un-scaled data with zeroed NA
  # calculate the mean and variance for the variables
  var_annot$mean <- colMeans(raw, na.rm = TRUE)
  var_annot$var <- matrixStats::colVars(raw, na.rm = TRUE)

  # determine which lipids have mean zero, so they can be remove later
  zero_var <- which(is.nan(var_annot$mean))

  # calculate the mean and variance for the observations, but this is for all samples together!! WHY?
  obs_meta$var <- matrixStats::rowVars(raw, na.rm = TRUE)
  obs_meta$mean <-rowMeans(raw, na.rm = TRUE)
  # experiment with the scaled (including)

  # determine which samples contain a lot of zero"s
  excess_zero_conc <- (colSums(dat_mat == 0) > 2/3 * dim(dat_mat)[1])
  var_annot$excess_zero_conc <- excess_zero_conc

  # make the output
  data_list <- list(data_mat = dat_mat[, -zero_var],
                    obs_meta = obs_meta,
                    var_annot = var_annot[-zero_var, ],
                    omics = rownames(var_annot)[-zero_var],
                    sample_ID = rownames(obs_meta),
                    etc = NULL,
                    raw = raw)

  return(data_list)
}



# TODO:  define "get marker genes" or some such


#==== 3. load data -========================================================================================

RAW_DIR <- file.path(OMICSER_RUN_DIR, "examples", "raw_data", "rico_data")


# a. load concentration data --------------------

conc_csv_name <- "example_species_conc.csv"
conc_dat_list <- prep_lipidizer_files(data_file = conc_csv_name,
                                      path_root = RAW_DIR)
# # b. load composition data --------------------
# #
# comp_csv_name <- "example_composition.csv"
# comp_dat_list <- prep_lipidizer_files(comp_csv_name, RAW_DIR)


#==== 4. pack into anndata =========================================================================

# helper_function<-("data-raw/ingest_helpers.R")
# source(helper_function)

DB_NAME <- "rico_lipids"

# concentrations
conc <- omicser::setup_database(database_name = DB_NAME,
                                db_path = DB_ROOT_PATH,
                                data_in = conc_dat_list,
                                db_meta = NULL ,
                                re_pack = TRUE)
# composition
# comp <- omicser::setup_database(database_name = DB_NAME,
#                                 db_path = DB_ROOT_PATH,
#                                 data_in = comp_dat_list,
#                                 db_meta = NULL ,
#                                 re_pack = TRUE)

# make a copy of conc
ad <- conc$copy()
# add raw data, don't do this now. It contains NA's
# raw <- ad$copy()
# raw$X <- conc_dat_list$raw
# ad$raw <- raw
# add the composition
# ad$layers <- list(composition = comp$X,
#                   raw_comp = comp_dat_list$raw)


ad$write_h5ad(filename = file.path(DB_ROOT_PATH, DB_NAME, "core_data.h5ad"))



#==== 5. post processing =========================================================================               --
# use scanpy to do some scaling and calculations...
# needed for creating differential tables
sc <- reticulate::import("scanpy")


# 5a. lasso regression to choose sig ----------------------------
#ZSCALE the matrix
# dat_mat <- scale(ad$X) #including zeros
# test_vals <- dat_mat
# regr_group <- ad$obs$Group
# g <- unique(regr_group)
# # g
# #
# regr_group <- as.numeric(factor(ad$obs$Group))
# ind_rem_group <- which(regr_group == which(table(regr_group) < 3))
#
# # remove groups with less than 3 observations..
# if (length(ind_rem_group) > 0) {
#   test_vals <- test_vals[-ind_rem_group, ]
#   regr_group <- regr_group[-ind_rem_group]
# }
#
# # choose the "significant" columns via lasso regression (glmnet)
# set.seed(100)
# cvfit <- glmnet::cv.glmnet(test_vals, regr_group, nlambda = 100, alpha = .8, family = "multinomial", type.multinomial = "grouped")
# coef <- coef(cvfit, s = "lambda.min")
# tmp <- as.matrix(coef$"1")
# tmp1 <- tmp[which(tmp != 0)]
# coef_names <- rownames(tmp)[which(tmp != 0)][-1]
# ind_coef <- which(colnames(test_vals) %in% coef_names)
#
# ad$var$sig_lasso_coef <- (colnames(test_vals) %in% coef_names)
# ad$uns <- list(comp_lasso_coef=coef)


#  don"t know how to make this work....
#sc$pp$highly_variable_genes(ad,n_top_genes=40)
ad$var$var_rank <- order(ad$var$var)
# choose top 40 proteins by variance across dataset as our "targets"
target_omics <- ad$var_names[which(ad$var$var_rank <= 40)]

# ad$var$decile <- dplyr::ntile(ad$var$var, 10)


# save an intermediate file (incase we want to revert...)
# ad$write_h5ad(filename = file.path(DB_ROOT_PATH, DB_NAME, "normalized_data.h5ad"))

#==== 5-a. dimension reduction - PCA / umap =========================================================================

## Step 3: Do some basic preprocessing to run PCA and compute the neighbor graphs
##
#
# comp <- ad$copy()
# comp$X <- ad$layers$get("composition")
# sc$pp$pca(comp)
# sc$pp$neighbors(comp)
# ## Step 4: Infer clusters with the leiden algorithm
# sc$tl$leiden(comp)
# ## Step 5: Compute tsne and umap embeddings
# sc$tl$umap(comp)
#
#
# sc$pp$pca(ad)
# sc$pp$neighbors(ad)
# sc$tl$leiden(ad)
# sc$tl$umap(ad)
#
#
# ad$obsm$comp_X_pca <- comp$obsm$X_pca
# ad$obsm$comp_X_umap <- comp$obsm$X_umap
# ad$varm$comp_PCs <- comp$varm$PCs
# ad$obsp$comp_distances <- comp$obsp$distances
# ad$obsp$comp_connectivities <- comp$obsp$connectivities
# # save an intermediate file (incase we want to revert...)
# ad$write_h5ad(filename = file.path(DB_ROOT_PATH, DB_NAME, "norm_data_plus_dr.h5ad"))


#==== 6. differential expression  ======================================================================

test_types <- c("wilcoxon",
                "t-test_overestim_var")

comp_types <- c("{LEAN}V{OBESE}",
                "{LEAN}V{OBESE+VLCD}",
                "{OBESE}V{OBESE+VLCD}")
obs_names <- c("Group")

diff_exp <- omicser::compute_de_table(adata = ad,
                                      comp_types = comp_types,
                                      test_types = test_types,
                                      obs_names = obs_names,
                                      sc = sc)

# remove infinite values and NaN"s
library(tidyverse)
diff_exp <- diff_exp %>%
  filter(!is.infinite(logfoldchanges),
         !is.nan(logfoldchanges))


# ad$write_h5ad(filename=file.path(DB_ROOT_PATH,DB_NAME, "norm_data_with_de.h5ad"))

saveRDS(diff_exp, file = file.path(DB_ROOT_PATH,DB_NAME, "db_de_table.rds"))

#==== 7. create configs =========================================================================
# what ad$obs do we want to make default values for...
omic_type <- "lipid" #c("transcript","prote","metabol","lipid","other")
aggregate_by_default <- ifelse(omic_type == "transcript", TRUE, FALSE) #e.g.  single cell

config_list <- list(
  ### grouping factors
  # if it needs to be in subset add here as well
  group_vars = c("lipid_class", "excess_zero_conc"),
  group_obs = c("Group"),

  ### layer info
  layer_values = c("X"),
  # are the names of the layers used?
  layer_names = c("Conc."),

  # ANNOTATIONS / TARGETS
  # what adata$obs do we want to make default values for...
  # # should just pack according to UI?

  ### observations
  default_obs = c("sample_ID"),
  # heatmap default selected
  obs_annots = c("Group"),

  ### variables
  default_var = c("feature_name"),
  # heatmap default selected
  var_annots = c("lipid_class", "excess_zero_conc"),

  ### set the target features, looks like they are not loaded yet
  target_features = target_omics,

  ### set the feature details when dot clicked in volcano plot
  # looks like this is not working, in domenico script it works
  feature_deets = c("feature_name",
                    "lipid_class",
                    "var_rank"),

  ### differential expression
  diffs = list(diff_exp_comps = levels(factor(diff_exp$versus)),
               diff_exp_comp_type = levels(factor(diff_exp$comp_type)), #i don"t think we need this
               diff_exp_obs_name = levels(factor(diff_exp$obs_name)),
               diff_exp_tests = levels(factor(diff_exp$test_type))),

  ### meta info
  annotation_database =  NA,
  publication = "TBD",
  method = "bulk", # c("single-cell","bulk","other")
  omic_type = omic_type, # see above
  aggregate_by_default = aggregate_by_default, # see above
  organism = 'mmusculus',
  lab = "Giera",
  title = "Lipidomics",
  date = format(Sys.time(), "%a %b %d %X %Y")
) # end config list

# write configuration to yaml file
omicser::write_db_conf(config_list = config_list,
                       db_name = DB_NAME,
                       db_root = DB_ROOT_PATH)

#==== 8. write data file to load  =========================================================================
# get rid of categorical variable -> does not matter, converted to categorical in write method
ad$var$lipid_class <- as.character(ad$var$lipid_class)

# write the database
ad $write_h5ad(filename = file.path(DB_ROOT_PATH, DB_NAME, "db_data.h5ad"))



# BOOTSTRAP the options we have already set up...
# NOTE: we are looking in the "quickstart" folder.  the default is to look for the config in with default getwd()
omicser_options <- omicser::get_config(in_path = OMICSER_RUN_DIR)
DB_ROOT_PATH <- omicser_options$db_root_path

#DB_NAME <- omicser_options$database_names[1]

if (! (DB_NAME %in% omicser_options$database_names)){
  omicser_options$database_names <- c(omicser_options$database_names,DB_NAME)
  omicser::write_config(omicser_options,in_path = OMICSER_RUN_DIR )
}




