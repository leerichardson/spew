context("Summarize spew in console")

test_that("functions for summarizing spew synecos in console",{

    data(tartanville)
    shapefile <- ggplot2::fortify(tartanville$shapefile, region = "place_id")
    if(!is.null(tartanville$roads)) roads <- ggplot2::fortify(tartanville$roads)

    ## spewing a syneco
    t_shapefile <- list(boundaries = tartanville$shapefile, roads = tartanville$roads)
    tartanville_syneco <- spew(tartanville$pop_table, t_shapefile,
                            tartanville$pums_h, tartanville$pums_p,
                            locations_method = "roads",
                            road_noise = .1)
    
    ## Plotting the syneco
    g <- plot_interior(shapefile)
    ## Roads
    g <- plot_roads(roads, g)
    ## Agents
    g <- plot_agents(tartanville_syneco, tartanville,  g)
    ## Boundaries
    g <- plot_bds(shapefile, g)
    ## Environments
    g <- plot_env(tartanville, g)
    ## Labels
    g <- plot_labs(region_name = "Tartanville", g)

    g <- plot_syneco(tartanville, tartanville_syneco,
                     region_name = "Tartanville")
    
    ## Summarizing the region
    out <- summarize_spew_region(tartanville_syneco[[1]],
                          type = "households",
                          marginals = NULL,
                          vars_to_sum = c("NP", "HHINC"))

    expect_true(out$pop_size == nrow(tartanville_syneco[[1]]$households))

    out <- summarize_syneco(tartanville_syneco, vars_to_sum_h = c("puma_id"),
                            vars_to_sum_p = c("SEX"),
                            vars_to_sum_env = NULL, top_region_id = "Tartanville")

     out <- summarize_spew_out(tartanville_syneco, vars_to_sum_h = c("puma_id"),
                            vars_to_sum_p = c("SEX"),
                            vars_to_sum_env = NULL, top_region_id = "Tartanville")
 })
