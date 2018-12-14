recipes = '37'
elf1 = 0
elf2 = 1

find_this = '165061'

loop do
  break if (recipes.size % 10_000).zero? && recipes.include?(find_this)
  recipes << (recipes[elf1].to_i + recipes[elf2].to_i).to_s
  elf1 = (elf1 + recipes[elf1].to_i + 1) % recipes.size
  elf2 = (elf2 + recipes[elf2].to_i + 1) % recipes.size
end

puts recipes.index(find_this)
