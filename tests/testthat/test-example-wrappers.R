test_that("short self-contained examples are unwrapped and run without warnings", {
  for (name in c("sim_markers", "sim_envirome", "tpe_weights",
                 "project_risk", "print.soil_gmm")) {
    rd_path <- test_path("..", "..", "man", paste0(name, ".Rd"))
    doc <- if (file.exists(rd_path)) tools::parse_Rd(rd_path) else
      tools::Rd_db("EnvRtype")[[paste0(name, ".Rd")]]
    example_file <- tempfile(fileext = ".R")
    on.exit(unlink(example_file), add = TRUE)
    tools::Rd2ex(doc, out = example_file, commentDontrun = TRUE,
                 commentDonttest = TRUE)
    code <- readLines(example_file, warn = FALSE)
    expect_false(any(grepl("## (Not run:|No test:)", code)))

    example_env <- new.env(parent = asNamespace("EnvRtype"))
    capture.output(
      expect_warning(sys.source(example_file, envir = example_env), NA)
    )
    if (name == "sim_markers") {
      expect_s3_class(example_env$met, "sim_met")
      expect_equal(dim(example_env$X_st), c(150L, 2000L))
      expect_true(all(is.finite(c(example_env$v_lo, example_env$v_hi))))
    } else if (name == "sim_envirome") {
      expect_s3_class(example_env$met, "sim_met")
      expect_equal(dim(example_env$E_cl), c(20L, 30L))
      expect_true(all(is.finite(example_env$g_hi)))
      expect_true(all(is.finite(example_env$g_cl)))
    } else if (name == "tpe_weights") {
      expect_s3_class(example_env$rp, "env_risk")
    } else if (name == "project_risk") {
      expect_s3_class(example_env$chg, "env_risk_delta")
      expect_true(all(is.finite(example_env$chg$change$freq_diff)))
    } else if (requireNamespace("mclust", quietly = TRUE)) {
      expect_s3_class(example_env$fit, "soil_gmm")
    }
  }
})
