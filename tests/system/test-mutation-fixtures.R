for (package in system_selected_packages()) {
  local({
    fixture <- package
    test_that(sprintf("mutation results for %s remain stable", fixture), {
      expect_snapshot_value(
        run_system_fixture(fixture),
      style = "json",
        variant = Sys.getenv("MUTATOR_SYSTEM_PROFILE", unset = "smoke")
      )
    })
  })
}
