arr = [1,2,3,4,5,6,7,8,9]

def iter(arr, thread_count)
  arr.each do |line|
    puts "#{line}"
  end
end

iter(arr, 2)
