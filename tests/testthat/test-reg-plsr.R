test_that("plsr multivariate model fitting", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ------------------------------------------------------------------------------
  meats_x_tr_mat <- as.matrix(meats_x_tr)
  meats_y_tr_mat <- as.matrix(meats_y_tr)
  meats_x_te_mat <- as.matrix(meats_x_te)
  meats_y_te_mat <- as.matrix(meats_y_te)

  multi_model <- pls::plsr(meats_y_tr_mat ~ meats_x_tr_mat, ncomp = 3)

  # ------------------------------------------------------------------------------

  pls_spec <-
    parsnip::pls(num_comp = 3) %>%
    set_engine("plsr") %>%
    set_mode("regression")

  # ------------------------------------------------------------------------------

  expect_error(
    parsnip_pls_multi <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("plsr") %>%
      set_mode("regression") %>%
      fit_xy(x = meats_x_tr, y = meats_y_tr),
    regexp = NA
  )

  expect_equal(parsnip_pls_multi$fit$loadings, multi_model$loadings)

  expect_error(
    parsnip_pls_multi_num <- predict(parsnip_pls_multi, as.data.frame(meats_x_te)),
    regexp = NA
  )

  mo_pls_pred <- predict(multi_model, meats_x_te_mat)[, , 3]
  mo_pls_pred <- tibble::as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- paste0(".pred_", names(mo_pls_pred))

  expect_equal(
    as.data.frame(parsnip_pls_multi_num),
    as.data.frame(mo_pls_pred),
    ignore_attr = TRUE
  )

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_multi, as.data.frame(meats_x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(meats_x_te))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c("num_comp", ".pred_water", ".pred_fat", ".pred_protein")
  )

  mo_pls_pred_9 <- t(predict(multi_model, meats_x_te_mat)[9, , 1:2])
  mo_pls_pred_9 <- tibble::as_tibble(mo_pls_pred_9)
  names(mo_pls_pred_9) <- paste0(".pred_", names(mo_pls_pred_9))

  expect_equal(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][, -1]),
    as.data.frame(mo_pls_pred_9),
    ignore_attr = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("plsr univariate model fitting", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ------------------------------------------------------------------------------

  uni_model <- pls::plsr(meats_y_tr_mat[,1] ~ meats_x_tr_mat, ncomp = 3)

  # ------------------------------------------------------------------------------

  expect_error(
    parsnip_pls_uni <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("plsr") %>%
      set_mode("regression") %>%
      fit_xy(x = meats_x_tr, y = meats_y_tr[[1]]),
    regexp = NA
  )

  expect_equal(parsnip_pls_uni$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_pls_uni_num <- predict(parsnip_pls_uni, as.data.frame(meats_x_te)),
    regexp = NA
  )

  mo_pls_pred <- predict(uni_model, meats_x_te_mat)[, , 3]
  mo_pls_pred <- tibble::as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- ".pred"

  expect_equal(
    as.data.frame(parsnip_pls_uni_num),
    as.data.frame(mo_pls_pred),
    ignore_attr = TRUE
  )

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_uni, as.data.frame(meats_x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(meats_x_te))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c("num_comp", ".pred"),
    ignore_attr = TRUE
  )

  mo_pls_pred_9 <- predict(uni_model, meats_x_te_mat)[9, , 1:2]
  mo_pls_pred_9 <- tibble::tibble(.pred = mo_pls_pred_9)

  expect_equal(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][, -1]),
    as.data.frame(mo_pls_pred_9)
  )
})

# ------------------------------------------------------------------------------

test_that("plsr dummy variable encodings", {
  skip_if_not_installed("modeldata")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)
  expect_error(
    parsnip_pls_multi <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("plsr") %>%
      set_mode("regression") %>%
      fit(body_mass_g ~ ., data = penguins),
    regexp = NA
  )

  expect_true(!any(names(parsnip_pls_multi$fit$X) == "(Intercept)"))

  expect_error(
    predict(parsnip_pls_multi, penguins),
    regexp = NA
  )
  expect_error(
    multi_predict(parsnip_pls_multi, penguins, num_comp = 1:3),
    regexp = NA
  )
})
