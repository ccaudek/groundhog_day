# Author: Corrado Caudek
# Project: Groundhog Day
#
# Script purpose
# --------------
# Data preprocessing.


configfile: "config.yaml"


# Get paths from config file.
# excel_codes_path = config["excel_subj_codes"]
# subj_data_dir = config["subjects_data_dir"]
# raw_data = config["complete_raw_data"]
#### cleaned_data_path = config["cleaned_data"]


# rule my_rule:
#     input:
#         "data/raw/data.xlsx",
#     output:
#         "data/prep/groundhog_raw.RDS",
#     script:
#         "workflows/scripts/my_test.R"


# Read individual PRL data files and create a single file
# with all the subjects' raw data.
rule read_data:
    input:
        xlsx=config["excel_codes"],  #    "data/raw/data.xlsx",
        dir_data=config["data_dir"],  #"data/raw/experiment_data",
    output:
        rds=config["complete_data_raw"],
    script:
        "workflows/scripts/import_mpath_data.R"


# shell:
#     "Rscript workflows/scripts/import_mpath_data.R {input} {output}"
# include: "workflows/rules/read_data.smk"
# Data wrangling.
# rule data_wrangling:
#     input:
#         raw_data=f"{raw_data}",
#     output:
#         clean_data=f"{cleaned_data_path}",
#     script:
#         "scripts/data_wrangling.R"
# rule save_table:
#     input:
#         # data="../../../data/prep/penguin_subset.rds",
#         data=f"{penguin}",
#     output:
#         # tab="../../../results/tables/tab_1.txt",
#         tab=f"{tbl_1}",
#     script:
#         "scripts/make_table.R"
