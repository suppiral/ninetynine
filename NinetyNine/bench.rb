#!/usr/bin/ruby
require 'benchmark'

cmd = gets.chomp
puts Benchmark.measure { puts "#{%x[#{cmd}]}" }


