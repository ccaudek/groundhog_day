# Author: Corrado Caudek
# Project: Groundhog Day
#
# Script purpose
# --------------
# Data preprocessing.

from pathlib import Path
import os

print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")


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
    log:
        "logs/read_data.log",
    script:
        "workflows/scripts/import_mpath_data.R"


# Data cleaning.
rule data_wrangling:
    input:
        rds=config["complete_data_raw"],
    output:
        clean=config["cleaned_data"],
    log:
        "logs/data_wrangling.log",
    script:
        "workflows/scripts/data_wrangling.R"


# Write input file for HDDMrl.
rule data_for_hddmrl:
    input:
        clean=config["cleaned_data"],
    output:
        hddmrl=config["hddmrl_data"],
    log:
        "logs/data_for_hddmrl.log",
    script:
        "workflows/scripts/hddmrl_data.R"
