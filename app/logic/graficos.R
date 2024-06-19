box::use(
  plotly[plot_ly, layout, subplot],
  scales[rescale]
)

#' @export
plot.perfil <- function(dataSym, perfil, nrows = 5) {
  x <- RSDA:::to.v2(dataSym)
  i <- which(x$sym.obj.names == perfil)
  
  figs <- lapply(1:length(x$sym.var.names), function(j) {
    aux <- x[i, j]
    
    if(aux$sym.var.types == "$M") {
      fig <- plot_ly(
        x = names(aux$data),
        y = rescale(unlist(array(aux$data)), to = c(0, 100)),
        name = aux$sym.var.names, type = "bar"
      ) |> layout(xaxis = list(showticklabels = FALSE),
                  annotations = list(x = 0.5, y = 1, font = list(size = 10), 
                                     text = aux$sym.var.names,
                                     xanchor = "center",  yanchor = "bottom",
                                     xref = "paper", yref = "paper", showarrow = FALSE))
    } else if (aux$sym.var.types == "$I") {
      minimo <- aux$data[1, 1]
      maximo <- aux$data[1, 2]
      rectangulo <- data.frame(
        x = c(minimo, minimo, maximo, maximo, minimo),
        y = c(0, 1, 1, 0, 0)
      )
      fig <- plot_ly(
        x = rectangulo$x, y = rectangulo$y, type = "scatter",
        name = aux$sym.var.names,
        mode = 'markers', fill="toself"
      ) |> layout(
        xaxis = list(showticklabels = FALSE),
        annotations = list(list(x = mean(c(minimo, maximo)), y = 0.5, font = list(size = 15), 
                           text = paste0("[", round(minimo, 2), ", ", round(maximo, 2), "]"),
                           xref = "x", yref = "y", showarrow = FALSE),
                           list(x = 0.5, y = 1, font = list(size = 10), 
                                text = aux$sym.var.names,
                                xanchor = "center",  yanchor = "bottom",
                                xref = "paper", yref = "paper", showarrow = FALSE)))
    }
    
    return(fig)
  })
  
  fig <- subplot(figs, nrows = nrows) |> layout(showlegend = FALSE)
  return(fig)
}
