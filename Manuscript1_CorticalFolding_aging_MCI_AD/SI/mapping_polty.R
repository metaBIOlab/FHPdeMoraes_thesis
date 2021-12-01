# dados_datasetscomp_rate$Diagnostic <- factor(dados_datasetscomp_rate$Diagnostic, levels = c("AD", "MCI","CTL"))
# dados_datasetscomp$Diagnostic <- factor(dados_datasetscomp$Diagnostic, levels = c("AD", "MCI","CTL"))

#fig_K <- 
plot_ly(
        filter(dados_datasetscomp, ROI == "hemisphere"),
        x = ~ Age,
        y = ~ K,
        # symbol = ~ Sample,
        type = "scatter",
        mode = 'markers',
        color  = ~ Diagnostic,
        legendgroup = ~ Diagnostic
)

fig_K_shift <-
        plot_ly(
                filter(dados_datasetscomp_rate, ROI == "hemisphere"),
                x = ~ Age,
                y = ~ K_shiftc,
                # symbol = ~ Sample,
                type = "scatter",
                mode = 'markers',
                color  = ~ Diagnostic,
                legendgroup = ~ Diagnostic,
                showlegend = F
        )

fig <- subplot(fig_K, fig_K_shift, nrows = 2, shareX = T)
fig
