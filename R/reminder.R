#' A Heartfelt Reminder
#'
#' This function allows you to remind yourself or someone else that they are awesome.
#' @param name Defaults to "Jennifer" (probably because she's awesome)
#' @param font Defaults to "Helvetica"
#' @keywords maybe miguel
#' @export
#' @import ggplot2
#' @examples
#' reminder(name = "Lisa", font = 'Mistral')

reminder <- function(name = 'Jennifer', font = 'Helvetica'){ 

 corazon <- data.frame(t=seq(0, 2*pi, by=0.1) )
 xHeart <- function(t) 16*sin(t)^3
 yHeart <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
 corazon$y=yHeart(corazon$t)
 corazon$x=xHeart(corazon$t)


textMessage <- c(paste0(name, '\n is \n Awesome'))

ggplot(corazon, aes(x,y)) +
  geom_polygon(fill = "hotpink", alpha = 0.9) +
  annotate("text", x=0, y= - 1, label = textMessage, color = 'white', family = font, lineheight = .75, size = 18, angle = 45) +
   theme_void() 

}
