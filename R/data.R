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
#'@format A data frame with 899 rows and 8 variables:
#' \describe{
#'   \item{country}{Country}
#'   \item{year}{Year}
#'   \item{lgdp}{Lagged Gross Domestic Product}
#'   \item{lhdd}{Logged Heating Degree Days}
#'   \item{lcdd}{Logged Cooling Degree Days}
#'   \item{urban}{Urban Share}
#'   \item{av.rate}{EU Interest Rate}
#' }
#' @source IEA
#'

"EUCO2residential"
