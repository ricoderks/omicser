# ==== INFO ========================================================
## code to prepare `yassene_example` data set goes here
##
##  "WHAT IS THE CONTEXT?" lipidomics composition data
##  - data from Yassene / Rico
##
#  formerly `yassene_A` (conc. and compos)

# ==== 1. preamble/setup ==================================================
# a. Set paths ------------------------------------------
#  path where the omicser repo is cloned to (see install_scrip.R)
# e.g. "~/Projects/NDCN_dev/omicser"
REPO_PATH <- getwd()
OMICSER_RUN_DIR <- REPO_PATH

# where are the raw data files for this script?
RAW_DATA_DIR <- file.path(OMICSER_RUN_DIR, "examples/raw_data/rico_data")

# set where we will put the database folder
DB_ROOT_PATH <- file.path(OMICSER_RUN_DIR,"examples/databases")

# what will we call the database
DB_NAME <-  list("Rico lipids (new)" = "rico_lipids_new")

# name your python environment
OMICSER_PYTHON <- ".pyenv"# "pyenv_omicser"

# b. Load the omicser package ------------------------------------------
if (CLONED_OMICSER <- TRUE) {
  # this should be a full path... e.g. ~/Projects/NDCN_dev/omicser
  # but for github, we will set relative to the repo BASE
  golem::document_and_reload(pkg = REPO_PATH)
} else {
  require(omicser)
}

# ==== 2. helper functions ====================================================
# put helper functions to prepare / curate the data here

### convert lipidyzer lipid names to LipidMaps standardized lipid names
# include some additional information from LipidMaps
# Parameters:
#   * lipid_names: character vector containing the lipidyzer lipid names
# Returns:
#   a data.frame with the converted names and additional id's
convert_lipid_names <- function(lipid_names = NULL) {
  # install from github (ricoderks/lipidtranslator)
  # currently still private repository
  require(lipidtranslator)

  # get the converted lipid names and some id's
  res <- lipidyzer2lm(lipid_name = lipid_names,
                      get_ids = TRUE)

  # select only a few columns
  selected_columns <- c("lipidyzer_name", "abbreviation", "abbreviation_chains",
                        "regno", "lm_id", "kegg_id", "class_kegg_id")
  res <- res[, selected_columns]

  # return the results
  return(res)
}


### Extract lipid class information
# Parameters:
#   * lipid_names: character vector containing the lipidyzer lipid names
# Returns:
#   a character vector with the lipid classes
extract_lipid_class <- function(lipid_names = NULL) {
  # extract lipid classes
  res <- sub(
    x = lipid_names,
    pattern = "^([A-Z]{2,4})(\\([OP])?.*$",
    replacement = "\\1\\2"
  )

  ### some cleanup
  # remove additional '('
  res <- sub(
    x = res,
    pattern = "\\(",
    replacement = "\\-"
  )

  # remove the A from DAG, TAG
  res <- sub(
    x = res,
    pattern = "([TD])(A)(G)",
    replacement = "\\1\\3"
  )

  # change FFA to FA
  res <- sub(
    x = res,
    pattern = "FFA",
    replacement = "FA"
  )

  # return the result
  return(res)
}
# ==== 3. load data -=========================================================
# a. load all data ---------------------------------------------------------
### define file names
# the data
conc_csv_name <- "conc_data_matrix.csv"
# observations (samples)
samples_csv_name <- "sample_info.csv"
# variables (lipids)
variables_csv_name <- "variable_info.csv"

### load the data
# data (for now still contains sample_id and lipid info)
data_df <- read.csv(file = file.path(RAW_DATA_DIR,
                                     conc_csv_name),
                    header = TRUE,
                    na.strings = c(".", "", "NA"),
                    check.names = FALSE)
# remove first column and convert to matrix
data_matrix <- as.matrix(data_df[, -1])
rownames(data_matrix) <- data_df$sample_id

# samples info
sample_info_df <- read.csv(file = file.path(RAW_DATA_DIR,
                                            samples_csv_name),
                           header = TRUE,
                           na.strings = c(".", "", "NA"))
rownames(sample_info_df) <- sample_info_df$sample_id

# variable info
lipid_info_df <- read.csv(file = file.path(RAW_DATA_DIR,
                                           variables_csv_name),
                          header = TRUE,
                          na.strings = c(".", "", "NA"))
rownames(lipid_info_df) <- lipid_info_df$lipid_name

# b. clean up a bit ---------------------------------------------------------
# some sample_id's have no group information remove them here
remove_samples_idx <- which(is.na(sample_info_df$Group))
data_matrix <- data_matrix[-remove_samples_idx, ]
sample_info_df <- sample_info_df[-remove_samples_idx, ]

# remove columns with only NA's
remove_variable_idx <- which(apply(X = data_matrix,
                                   MARGIN = 2,
                                   FUN = function(x) {
                                     all(is.na(x))
                                   }))
data_matrix <- data_matrix[, -remove_variable_idx]
lipid_info_df <- data.frame(lipid_name = lipid_info_df$lipid_name[-remove_variable_idx])
rownames(lipid_info_df) <- lipid_info_df$lipid_name

# c. data pre-processing ----------------------
# do some pre-processing here, e.g. imputing, etc.

# Replace NA's by zero's: is this tricky because its sparse?
data_matrix[which(is.na(data_matrix), arr.ind = TRUE)] <- 0

### Variables
# add lipid class information
lipid_info_df$lipid_class <- extract_lipid_class(lipid_names = lipid_info_df$lipid_name)

# calculate the mean and variance for the variables
lipid_info_df$mean <- colMeans(x = data_matrix,
                               na.rm = TRUE)
lipid_info_df$var <- matrixStats::colVars(x = data_matrix,
                                          na.rm = TRUE)

# check if 2/3 of a variable contains zero's
excess_zero_conc <- colSums(data_matrix == 0) > 2/3 * dim(data_matrix)[1]
lipid_info_df$excess_zero_conc <- excess_zero_conc

# convert the lipid names and get some additional info
lipid_extra_info_df <- convert_lipid_names(lipid_name = lipid_info_df$lipid_name)

### Observations / samples
# calculate the mean and variance for the observations, but this is for all samples together!! WHY?
sample_info_df$var <- matrixStats::rowVars(x = data_matrix,
                                           na.rm = TRUE)
sample_info_df$mean <-rowMeans(x = data_matrix,
                               na.rm = TRUE)

# ==== 4. post processing ===================================================
# a. lasso regression to choose significant variabkes  ----------------------------
# choose the "significant" columns via lasso regression (glmnet)
set.seed(100)
cvfit <- glmnet::cv.glmnet(x = data_matrix,
                           y = sample_info_df$Group,
                           nlambda = 100,
                           alpha = 0.8,
                           family = "multinomial",
                           type.multinomial = "grouped")
coef <- coef(object = cvfit,
             s = "lambda.min")
tmp <- sapply(X = coef,
              FUN = function(x) {
                x[, 1]
              })
# which coefficients are not zero
tmp1 <- unique(which(tmp != 0, arr.ind = TRUE)[, 1])
# get  the names
coef_names <- rownames(tmp)[tmp1][-1]
# and the indexes
ind_coef <- which(colnames(data_matrix) %in% coef_names)
# store as logical
lipid_info_df$sig_lasso_coef <- colnames(data_matrix) %in% coef_names
# put into list
uns <- list(comp_lasso_coef = coef)

# ==== 5. pack into anndata ==================================================
# a. prepare to pack into anndata -----------------------------------------
# need to create a list containing :
#    * data_mat = data matrix
#    * obs_meta = data.frame containing observation information
#    * var_annot = data.frame containing variable information
#    * omics =
#    * sample_ID = character vector with all sample ID's
#    * etc = NULL
#    * raw = the original raw data

data_list <- list(
  data_mat = data_matrix,
  obs_meta = sample_info_df,
  var_annot = lipid_info_df,
  omics = NULL,
  sample_ID = sample_info_df$sample_id,
  etc = NULL,
  raw = NULL,
  uns = uns
)

# b. pack into anndata ------------------------------------------------------
ad <- omicser::setup_database(database_name = DB_NAME,
                              db_path = DB_ROOT_PATH,
                              data_in = data_list,
                              re_pack = TRUE)

# write to hdf5 file
ad$write_h5ad(filename = file.path(DB_ROOT_PATH, DB_NAME,
                                   "core_data.h5ad"))



#==== 6. differential expression  ======================================================================

test_types <- c("wilcoxon",
                "t-test_overestim_var")

comp_types <- c("{LEAN}V{OBESE}",
                "{LEAN}V{OBESE+VLCD}",
                "{OBESE}V{OBESE+VLCD}")
obs_names <- c("Group")

# need scanpy for function compute_de_table
sc <- reticulate::import("scanpy")
diff_exp <- omicser::compute_de_table(adata = ad,
                                      comp_types = comp_types,
                                      test_types = test_types,
                                      obs_names = obs_names,
                                      sc = sc)

# remove infinite values and NaN's
remove_idx <- c(which(is.infinite(diff_exp$logfoldchanges)),
                which(is.nan(diff_exp$logfoldchanges)))
diff_exp <- diff_exp[-remove_idx, ]

# ad$write_h5ad(filename=file.path(DB_ROOT_PATH,DB_NAME, "norm_data_with_de.h5ad"))

saveRDS(diff_exp, file = file.path(DB_ROOT_PATH,DB_NAME, "db_de_table.rds"))

#==== 7. create configs =========================================================================
# what ad$obs do we want to make default values for...
# choose from "transcript","prote","metabol","lipid","other"
omic_type <- "lipid"
aggregate_by_default <- ifelse(omic_type == "transcript", TRUE, FALSE) #e.g.  single cell

config_list <- list(
  ### grouping factors
  # if it needs to be in subset add here as well
  group_vars = c("lipid_class", "excess_zero_conc", "sig_lasso_coef"),
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
  var_annots = c("lipid_class", "excess_zero_conc", "sig_lasso_coef"),

  ### set the target features, looks like they are not loaded yet
  target_features = coef_names,

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




