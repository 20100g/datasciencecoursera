f <- function (x){
    z<-10
    g <- function(y){
        y+z
    }
    #z<-4
    x + g(x)
}