class Test
  def initialize(*args)
	puts args[0][1]
  end
end

Test.new(
  :key1 => "value1", 
  :key2 => "value2"
)
