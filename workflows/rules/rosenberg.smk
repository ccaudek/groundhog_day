rule select_rows_rosenberg:
    input:
        quest=os.path.join(prepdir, "quest.csv"),
    output:
        csv=os.path.join(prepdir, "quest_scales", "rosenberg_items.csv"),
    script:
        #"workflows/scripts/select_rows_rosenberg.R"
        os.path.join(scriptsdir, "select_rows_rosenberg.R")
