#!/usr/bin/env ruby -wKU

doubles = 0
triples = 0

open('input.txt', 'r') { |io|
  io.each_line { |l|
    counts = Hash.new 0
    l.each_char { |chr| counts[chr] += 1 }
    if counts.has_value? 2
      doubles += 1
    end
    if counts.has_value? 3
      triples += 1
    end
  }
}

puts doubles * triples