#####################
# Getting version
#####################
version = read.dcf("DESCRIPTION")[, "Version"]
# header
top = readLines("neuro_top.R")

# neurocLite definition
content = readLines("R/neuro_install.R")
content = paste0("\t", content)

vc = readLines("R/make_full_version.R")
vc = paste0("\t", vc)
content = c(content, vc)

# adding version to the head and the printout
version = paste0("# neurocInstall package version: ", version)
version = c(version, paste0("pkg_ver = '", version, "'"))
out = c(version, top, content)

# adding neuro_package tables so that things work
content = readLines("R/neuro_package_table.R")
content = paste0("\t", content)

out = c(out, "", content)
# adding bottom of if statement
bot = readLines("neuro_bottom.R")
out = c(out, bot)
writeLines(text = out, con = "neurocLite.R")
