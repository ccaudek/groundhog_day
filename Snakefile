# Author: Corrado Caudek
# Project: Groundhog Day
#
# Script purpose
# --------------
# Data preprocessing.


configfile: "config.yaml"


rule all:
    input:
        "data/prep/groundhog_raw.RDS",
        "data/prep/groundhog_clean.RDS",
        "data/prep/groundhog_hddmrl_data.csv",


# Read individual PRL files and create a single file
# with all subjects' raw data.
rule read_data:
    input:
        xlsx=config["excel_codes"],
        dir_data=config["data_dir"],
    output:
        rds=config["complete_data_raw"],
    script:
        "workflows/scripts/import_mpath_data.R"


rule data_wrangling:
    input:
        rds=config["complete_data_raw"],
    output:
        clean=config["cleaned_data"],
    script:
        "workflows/scripts/data_wrangling.R"


rule data_for_hddmrl:
    input:
        clean=config["cleaned_data"],
    output:
        hddmrl=config["hddmrl_data"],
    script:
        "workflows/scripts/hddmrl_data.R"
