# coding: utf-8
# Problème "GroLopin" de Prologin 2013
# Soit un ensemble de nombres, renvoyer une antichaîne maximale pour la divisibilité

n = gets.to_i
nums = gets.split.map(&:to_i)
exit 1 unless nums.size == n

nums.uniq!
n = nums.size

greater = Array.new(n) { Array.new }
lower = Array.new(n) { Array.new }

n.times do |i|
  n.times do |j|
    if i != j and nums[j] % nums[i] == 0
      greater[i].push(j)
      lower[j].push(i)
    end
  end
end

matched = Array.new(n) { { left: nil, right: nil } }

ancestor = Array.new(n)

loop do
  queue = Array.new
  n.times do |i|
    queue.push(i) if matched[i][:left].nil?
  end
  ancestor.map!{nil}
  augmenting_path_end = catch :last do
    until queue.empty?
      i = queue.shift
      greater[i].each do |j|
        unless matched[i][:left] == j
          k = matched[j][:right]
          throw(:last, [i,j]) if k.nil?
          if ancestor[k].nil?
            ancestor[k] = i
            queue.push(k)
          end
        end
      end
      nil
    end
  end
  break if augmenting_path_end.nil?
  i, j = augmenting_path_end
  loop do
    oldmatch = matched[i][:left]
    matched[i][:left] = j
    matched[j][:right] = i
    break if oldmatch.nil?
    exit 1 if ancestor[i].nil?
    i, j = [ancestor[i], oldmatch]
  end
end

min_vertex_cover_size = matched.select{|x| not x[:left].nil?}.size
max_antichain_size = n - min_vertex_cover_size

queue = Array.new

frontier_left = Array.new(n, true)
frontier_right = Array.new(n, false)
visited = Array.new(n) { { left: false, right: false } }

n.times do |i|
  if matched[i][:left].nil?
    queue.push(i) 
    visited[i][:left] = true
    frontier_left[i] = false
  end
end

until queue.empty?
  i = queue.shift
  greater[i].each do |j|
    unless matched[i][:left] == j or visited[j][:right]
      visited[j][:right] = true
      frontier_right[j] = true
      k = matched[j][:right]
      unless k.nil? or visited[k][:left]
        visited[k][:left] = true
        frontier_left[k] = false
        queue.push(k)
      end
    end
  end
  nil
end

max_antichain = (0...n).select{|i| not (frontier_left[i] or frontier_right[i])}.map{|i| nums[i]}
exit 1 unless max_antichain.size == max_antichain_size
exit 2 unless max_antichain.all? do |p|
  max_antichain.all? do |q|
    p == q or (p % q != 0 and q % p != 0)
  end
end
print max_antichain
puts max_antichain.size

