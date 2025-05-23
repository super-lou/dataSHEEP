


load_dataSHEEP_fonts = function () {
    fonts_dir = "fonts"
    
    lato_dirpath = system.file(fonts_dir, "Lato", package="dataSHEEP")
    sysfonts::font_add("Lato-test",
                       regular=file.path(lato_dirpath, "Lato-Regular.ttf"),
                       bold=file.path(lato_dirpath, "Lato-Bold.ttf"),
                       italic=file.path(lato_dirpath, "Lato-Italic.ttf"),
                       bolditalic=file.path(lato_dirpath, "Lato-BoldItalic.ttf"))

    raleway_dirpath = system.file(fonts_dir, "Raleway", "static", package="dataSHEEP")
    sysfonts::font_add("Raleway",
                       regular=file.path(raleway_dirpath, "Raleway-Regular.ttf"),
                       bold=file.path(raleway_dirpath, "Raleway-Bold.ttf"),
                       italic=file.path(raleway_dirpath, "Raleway-Italic.ttf"),
                       bolditalic=file.path(raleway_dirpath, "Raleway-BoldItalic.ttf"))

    showtext::showtext_auto()
}
