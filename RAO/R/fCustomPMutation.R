#' Mutation probability of the GA organisms
#' @export
fCustomPMutation = function (
   object,
   iUpdateAfterEvery = 0, 
   i = 0, 
   iMaxIterations
) { 

   # iUpdateAfterEvery * i = nbr of iterations already happened in previous runs
   # object@iter = iterations in this run
   # Effectively percent iterations left * runif(1)
   ( (iMaxIterations - (object@iter + (iUpdateAfterEvery * i))) / iMaxIterations ) * runif(1) 

}