for (f in list.files("data/census/")) {
  ls <- readLines(paste("data", "census", f, sep = "/"))[-(243:333)]
  writeLines(ls, paste("data", "census", f, sep = "/"))
}
