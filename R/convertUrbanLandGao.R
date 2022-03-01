# not useful for gridded urban


convertUrbanLandGao<-function(x){
  CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  x   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)

  return(x)

}
