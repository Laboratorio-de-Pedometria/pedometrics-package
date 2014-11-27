#  Template documentation for spatial simulated annealing
#
################################################################################
#' @param iterations Integer value defining the maximum number of iterations 
#' that should be used for the optimization. See \sQuote{Details} for more 
#' information.
#' 
#' @param acceptance List with two sub-arguments: \code{initial} and 
#' \code{cooling}. \code{initial} is a numeric value between 0 and 1 defining
#' the initial acceptance probability. Defaults to \code{initial = 0.99}.
#' \code{cooling} is a numeric value defining the exponential factor by with 
#' the acceptance probability decreases at each iteration. Defaults to 
#' \code{cooling = iterations / 10}. See \sQuote{Details} for more 
#' information.
#' 
#' @param stopping A list with one sub-argument: \code{max.count}. 
#' \code{max.count} is an integer value defining the maximum allowable number 
#' of iterations without improvement of the objective function value. This is 
#' also known as the freezing criterion. Defaults to 
#' \code{max.count = iterations / 10}. See \sQuote{Details} for more 
#' information.
#' 
#' @param plotit Logical value for ploting the optimization results. This 
#' includes a) the progress of the objective function values and acceptance 
#' probabilities, and b) the original points, the perturbed points and the 
#' progress of the maximum perturbation in the x and y coordinates. The plots 
#' are updated at each 10 iterations. The boundary of the spatial domain is 
#' passed using the argument \code{boundary}. Defaults to \code{plotit = TRUE}.
#' 
#' @param boundary SpatialPolygon defining the boundary of the spatial domain. 
#' It is mandatory if \code{plotit = TRUE}.
#' 
#' @param progress Logical value for printing a progress bar. Defaults to 
#' \code{progress = TRUE}.
#' 
#' @param verbose Logical value for printing messages about the progress of the
#' optimization.
#' 
# End!