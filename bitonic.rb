n = gets.to_i
pts = n.times.collect do
  x,y = gets.split.map(&:to_i)
  {x: x, y: y}
end

pts.sort_by!{|p| p[:x]}

def dist(p, q)
  Math.sqrt((p[:x]-q[:x])**2 + (p[:y]-q[:y])**2)
end

bitonics = [dist(pts[0], pts[1])]

(2..(n-2)).each do |i|
  # at this point bitonics[j] (j < i-1) contains the optimum with endpoints j and i-1
  x = bitonics.each_with_index.map{|d, j| d + dist(pts[i], pts[j])}.min
  delta = dist(pts[i-1], pts[i])
  bitonics.map!{|d| d + delta}.push(x)
  # now bitonics[j] (j < i) contains the optimum with endpoints j and i
end

puts bitonics.each_with_index.map{|d,j| d + dist(pts[n-1], pts[j]) + dist(pts[n-1], pts[n-2])}.min.ceil

