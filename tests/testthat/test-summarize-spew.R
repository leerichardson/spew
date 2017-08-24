context("Summarize spew in console")

test_that("functions for summarizing spew synecos in console",{

    data(tartanville)
    shapefile <- ggplot2::fortify(tartanville$shapefile, region = "place_id")
    if(!is.null(tartanville$roads)) roads <- fortify(tartanville$roads)

    ## spewing a syneco
    tartanville_syneco <- spew(tartanville$pop_table, shapefile,
                            tartanville$pums_h, tartanville$pums_p,
                            locations_method = "roads")
    
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
    print(g)

 })
