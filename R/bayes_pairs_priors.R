#' Plot pairs posteriors
#'
#' @param posteriors a data.frame with each posterior parameter as a column
#' @param priors a data.frame sampled from the prior distribution used to produce the posteriors
#'
#' @return a gtable with rug plots on the top and lefthand side, parameter names on the diagonal and 2D density plots in other positions
#' @export
#'
#' @examples
bayes_pairs_priors <- function(posteriors, priors){
  n_params <- ncol(posteriors)

  plt.list <- list()
  plt.idx <- 1
  for(i in 1:(n_params+1)){
    for(j in 1:(n_params+1)) local({
      i <- i
      j <- j
      plt.idx <- plt.idx
      if((i == 1) & (j == 1)) { ## TOP CORNER BLANK
        plt <- ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes(1,1), colour = 'white')+
          ggplot2::theme(plot.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.border = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.line = ggplot2::element_blank())
      }
      else if((i == 1)){ ## TOP RUG PLOTS
        plt <- ggplot2::ggplot() +
          ggplot2::stat_density(data=posteriors, ggplot2::aes(posteriors[[j-1]]),
                                size=0.2, geom = "line", position = "identity")+
          ggplot2::geom_density(data=priors, ggplot2::aes(priors[,j-1]), colour="red")+
          ggplot2::scale_x_continuous(limits = c(min(priors[,j-1]), max(priors[,j-1])))+
          theme_ajf()+
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
      }
      else if((j == 1)){ ## LHS RUG PLOTS
        plt <- ggplot2::ggplot() +
          ggplot2::stat_density(data=posteriors, ggplot2::aes(posteriors[[i-1]]),
                                size=0.2, geom = "line", position = "identity")+
          ggplot2::geom_density(data=priors, ggplot2::aes(priors[,i-1]), colour="red")+
          ggplot2::scale_x_continuous(limits = c(min(priors[,i-1]), max(priors[,i-1])))+
          theme_ajf()+
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())+
          ggplot2::coord_flip()+
          ggplot2::scale_y_reverse()
      }
      else if(i == j){ ## DIAGONAL PARAM NAMES
        ## plot param name
        plt <- ggplot2::ggplot()+
          ggplot2::annotate("text", x = 4, y = 25, size=4, label = as.character(colnames(posteriors)[i-1]))+
          ggplot2::theme(plot.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.border = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         axis.title = ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.line = ggplot2::element_blank())
      }
      else if(j>i){ ## TOP DIAG DENSITY PLOTS
        plt <- ggplot2::ggplot() +
          ggplot2::stat_density_2d(ggplot2::aes(x = posteriors[[j-1]], y = posteriors[[i-1]],
                                                fill=..level..),
                                   geom='polygon', size=0.2)+
          ggplot2::scale_x_continuous(limits = c(min(priors[,j-1]), max(priors[,j-1])))+
          ggplot2::scale_y_continuous(limits = c(min(priors[,i-1]), max(priors[,i-1])))+
          ggplot2::scale_fill_continuous(guide="none", low="yellow",high="red")+
          ggplot2::scale_alpha_continuous(guide="none")+
          theme_ajf()+
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank())
      }
      else { ## BOTTOM DIAG DENSITY PLOTS
        plt <- ggplot2::ggplot() +
          ggplot2::geom_density_2d(ggplot2::aes(x = posteriors[[j-1]], y = posteriors[[i-1]],
                                                colour=..level..), size=0.2)+
          ggplot2::scale_x_continuous(limits = c(min(priors[,j-1]), max(priors[,j-1])))+
          ggplot2::scale_y_continuous(limits = c(min(priors[,i-1]), max(priors[,i-1])))+
          ggplot2::scale_colour_continuous(guide="none", low="yellow",high="red")+
          ggplot2::scale_alpha_continuous(guide="none")+
          theme_ajf()+
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank())
      }

      plt.list[[plt.idx]] <<- plt
      plt.idx <<- plt.idx + 1
    })
  }

  plt.all <- gridExtra::grid.arrange(grobs=plt.list, nrow=n_params+1, ncol=n_params+1)
  return(plt.all)
}
