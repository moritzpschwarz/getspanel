#' Simulated Panel Data
#'
#'
#'
#' @format A data frame with 400 rows and 9 variables:
#' \describe{
#'   \item{country}{A random country}
#'   \item{year}{Year}
#'   \item{gdp}{A simulated Gross Domestic Product}
#'   \item{temp}{A simulated variable standing for temperature}
#'   \item{const}{The constant}
#'   \item{country_1}{A dummy for country 1}
#'   \item{country_2}{A dummy for country 2}
#'   \item{country_3}{A dummy for country 3}
#'   \item{country_4}{A dummy for country 4}
#'
#'   ...
#' }
#' @source \url{https://github.com/moritzpschwarz/getspanel/}
"pandata_simulated"

#' CO2 Data for EU Road Emissions
#'
#'@format A data frame with 1550 rows and 13 variables:
#' \describe{
#'   \item{X}{Index}
#'   \item{country}{Country}
#'   \item{year}{Year}
#'   \item{gdp}{Gross Domestic Product}
#'   \item{pop}{Population}
#'   \item{transport.emissions}{Transport CO2 Emissions}
#'   \item{lgdp}{Log GDP}
#'   \item{lpop}{Log Population}
#'   \item{ltransport.emissions}{Log Transport CO2 Emissions}
#'   \item{const}{Constant}
#'   \item{L1.ltransport.emissions}{Lag 1 Log Transport CO2 Emissions}
#'   \item{L1.lgdp}{Lag 1 Log GDP}
#'   \item{L1.lpop}{Lag 1 Log Population}
#' }
#' @source EDGAR
#'

"EU_emissions_road"



#' CO2 Data for the EU Residential Sector
#'
#'@format A data frame with 1550 rows and 9 variables:
#' \describe{
#'   \item{country}{Country}
#'   \item{year}{Year}
#'   \item{lgdp}{Log Gross Domestic Product}
#'   \item{lhdd}{Log Heating Degree Days}
#'   \item{lcdd}{Log Cooling Degree Days}
#'   \item{urban}{Urban Share}
#'   \item{av.rate}{EU Interest Rate}
#'   \item{pop}{Population}
#'   \item{agg.directem}{Aggregated Direct Emissions}
#' }
#' @source IEA
#'

"EUCO2residential"

#' Example Results for Comparison Plotting
#'
#' @format A data frame with 3 rows and 5 variables:
#' \describe{
#'   \item{formula}{Model formula}
#'   \item{country_sample}{Countries included in the sample}
#'   \item{p_val}{P-value of the model}
#'   \item{model}{Unique Model descriptions}
#'   \item{is}{isatpanel objects}
#' }
#' @source \url{https://doi.org/10.1038/s41560-022-01095-6}
"compare_models_example"
