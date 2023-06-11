# Read individual PRL files and create a single file
# with all subjects' raw data.
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
