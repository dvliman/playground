require 'rubygems'
require 'redis'
require 'faker'

r = Redis.new
(0..10000000).each do
  name = Faker::Name.name
  content = Faker::Lorem.sentence
  r.hmset(name, name, content)
end
