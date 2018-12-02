#!/usr/bin/env ruby -wKU

velocity = 0

open('input.txt', 'r') { |io|
  io.each_line { |l| velocity += l.to_i }
}

puts velocity