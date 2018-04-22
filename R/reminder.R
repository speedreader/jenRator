#' A Heartfelt Reminder
#'
#' This function allows you to remind yourself or someone else that they are awesome.
#' @param name Defaults to "Jennifer" (probably because she's awesome).
#' @keywords cats
#' @export
#' @examples
#' reminder("Collette")

reminder <- function(name = 'Jennifer'){ data.frame(t=seq(0, 2*pi, by=0.1) )
 xhrt <- function(t) 16*sin(t)^3
 yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
 dat$y=yhrt(dat$t)
 dat$x=xhrt(dat$t)
 #with(dat, plot(x,y, type="l"))

textMessage <- c(paste0(name, '\n is \n Awesome'))

ggplot(dat, aes(x,y)) +
  geom_polygon(fill = "hotpink", alpha = 0.9) +
  annotate("text", x=0, y= - 1, label = textMessage, color = 'white', family = 'Mistral', lineheight = .75, size = 18, angle = 45) +
   theme_void() 

}

