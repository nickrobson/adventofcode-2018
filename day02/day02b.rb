#!/usr/bin/env ruby -wKU

words = Array.new

open('input.txt', 'r') { |io|
  io.each_line { |l| words << l.strip.chars }
}

def common_letters(a, b)
  a.each_index.reduce([]) { |acc,i|
    if a.at(i) == b.at(i)
      acc << a.at(i)
    end
    acc
  }
end

words.each { |a|
  words.each { |b|
    d = common_letters(a, b)
    if d.length == a.length - 1
      puts d.join
      exit
    end
  }
}