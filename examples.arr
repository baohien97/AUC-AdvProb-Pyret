include image

uva-founding = 1623
vu-founding = 1880
if uva-founding == vu-founding:
  "He he he"
else if uva-founding < vu-founding:
  "Uva is older"
else: 
  "Vu is older"
end 


fun japan-flag(unit :: Number) -> Image:
  bg-width :: Number = unit * 3
  bg-height :: Number = unit * 2
  circ-rad :: Number = 3/5 * 1/2 * bg-height
  red-circ :: Image = circle(circ-rad, "solid", "red")
  white-rect :: Image = rectangle(bg-width, bg-height, "solid", "white")
  overlay(red-circ, white-rect)
end


fun moon-weight(earth-weight :: Number)->Number:
  earth-weight * 1/6
where:
  moon-weight(100) is 100 * 1/6
  moon-weight(150) is 150 * 1/6
  moon-weight(90) is 90 * 1/6
end

fun hours-to-wages(hours :: Number) -> Number:
  doc: "Compute total wage from hours, with overtime, at $10/hr base"
  if hours <= 40:
    hours * 10
  else:
    (40 * 10) + ((hours - 40) * (10 * 1.5))
  end
where:
  hours-to-wages(40) is 400
  hours-to-wages(40.5) is 407.5
  hours-to-wages(41) is 415
  hours-to-wages(0) is 0
  hours-to-wages(45) is 475
  hours-to-wages(20) is 200
end