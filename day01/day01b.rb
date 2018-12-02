#!/usr/bin/env ruby -wKU

lines = Array.new
counts = Hash.new 0

open('input.txt', 'r') { |io|
  io.each_line { |l| lines << l.to_i }
}

velocity = 0
while true
  lines.each { |e|
    velocity += e
    counts[velocity] += 1
    if counts[velocity] == 2
      puts velocity
      exit
    end
  }
end