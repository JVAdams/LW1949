# LW1949 1.0.0.9003

* Incorporated rounding in keeping with the true approach taken by Litchfield and Wilcoxon (1949)
    * `fitlinear()` - default setting for argument `constr` set to `c(0.0005, 0.9995)`
    * `assessfit()` - exclude records with expected effects < 0.005% or > 99.995%
    * `correctval()` - apply corrected values to expected effects < 0.5% or > 99.5%
    * `LWestimate()` - calculate `Nprime` as number of observations between ED15.5% and ED84.5%
* Introduced greater flexibility in plotting functions.
    * `plotDELP()` - added argument `grid`
    * `LWnomo1()` - added argument `...`
* Extended `predLines()` and `predLinesLP()` to cover entire x-range

# LW1949 1.0.0

* Package first submitted to CRAN
