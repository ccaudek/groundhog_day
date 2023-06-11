# Write input file for HDDMrl.
rule data_for_hddmrl:
    input:
        clean=config["cleaned_data"],
    output:
        hddmrl=config["hddmrl_data"],
    log:
        "logs/data_for_hddmrl.log",
