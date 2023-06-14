# Important note:
# All paths defined in this configuration file must be either
# absolute or relative to the location of the Snakefile!

import os
from pathlib import Path
from snakemake.utils import min_version

# Snake Version
min_version("5.7.1")

# Configuration file
if len(config) == 0:
    if os.path.isfile("config/config.yaml"):

        configfile: "config/config.yaml"

    else:
        sys.exit(
            "Looks like there is no config.yaml file in "
            + os.getcwd()
            + " make sure there is one or at least specify one with the --configfile commandline parameter."
        )


## Sanitize provided input and output directories
import re


def getpath(str):
    if str in ["", ".", "./"]:
        return ""
    if str.startswith("./"):
        regex = re.compile("^\./?")
        str = regex.sub("", str)
    if not str.endswith("/"):
        str += "/"
    return str


print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")

prepdir = getpath(config["output_prep"])
brmsdir = getpath(config["output_brms"])
scriptsdir = getpath(config["scripts_dir"])

Renv = "workflows/envs/environment_R.yaml"


# Run all analyses
rule all:
    input:
        os.path.join(prepdir, "groundhog_raw.RDS"),
        os.path.join(prepdir, "groundhog_clean.RDS"),
        # os.path.join(prepdir, "groundhog_hddmrl_data.csv"),
        # os.path.join(brmsdir, "fitted_models", "brms_moodpre_1.RDS"),
        # os.path.join(brmsdir, "tables", "brms_moodpre_1.csv"),
        "workflows/report/control_report.html",
        os.path.join(prepdir, "quest.csv"),
        os.path.join(prepdir, "quest_scales", "rosenberg_scores.csv"),
        "data/my_test.csv",


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


# Effect of control on mood_pre.
rule moodpre_tilda_control:
    input:
        clean=config["cleaned_data"],
    output:
        fit=config["brms_fit_1"],
        csv=config["brms_table_1"],
    log:
        "logs/moodpre_tilda_control.log",
    script:
        "workflows/scripts/brms_moodpre_control.R"


rule control_report:
    input:
        clean=config["cleaned_data"],
    output:
        "workflows/report/control_report.html",
    script:
        "workflows/scripts/control.Rmd"


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
        # config["hddmrl_data"]


rule import_quest_data:
    input:
        q1=config["quest1"],
        q2=config["quest2"],
        q3=config["quest3"],
        q4=config["quest4"],
    output:
        csv=os.path.join(prepdir, "quest.csv"),
    log:
        "logs/import_quest_data.log",
    script:
        # "workflows/scripts/import_quest_data.R"
        config["import_quest_data"]


# Rosenberg Self Esteem Scale
rule select_cols_rosenberg:
    input:
        quest=os.path.join(prepdir, "quest.csv"),
    output:
        csv=os.path.join(prepdir, "quest_scales", "rosenberg_items.csv"),
    log:
        "logs/select_cols_rosenberg.log",
    script:
        # "workflows/scripts/select_rows_rosenberg.R"
        os.path.join(scriptsdir, "select_cols_rosenberg.R")


rule scoring_rosenberg:
    input:
        ros=os.path.join(prepdir, "quest_scales", "rosenberg_items.csv"),
    output:
        csv=os.path.join(prepdir, "quest_scales", "rosenberg_scores.csv"),
    log:
        "logs/scoring_rosenberg.log",
    script:
        os.path.join(scriptsdir, "scoring_rosenberg.R")


# include: "workflows/rules/rosenberg.smk"


include: "workflows/rules/closing_messages.smk"
include: "workflows/rules/my_test.smk"
