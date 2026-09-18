validate_workdir <- function(path) {
  if (length(path) != 1 || is.na(path) || !nzchar(trimws(path))) stop("请选择或输入一个文件夹。", call. = FALSE)
  path <- path.expand(trimws(path))
  if (!dir.exists(path)) stop("文件夹不存在，请重新选择。", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (file.access(path, 4) != 0 || file.access(path, 2) != 0) stop("此文件夹没有读取或写入权限，请选择其他文件夹。", call. = FALSE)
  path
}

save_to_workdir <- function(directory, prefix, extension, writer) {
  directory <- validate_workdir(directory)
  destination <- tempfile(paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-"), tmpdir = directory, fileext = extension)
  complete <- FALSE
  on.exit(if (!complete && file.exists(destination)) unlink(destination))
  writer(destination)
  complete <- TRUE
  destination
}
