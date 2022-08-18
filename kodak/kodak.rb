require 'open-uri'

# TODO: convert to unicode??
puts ""
puts "Welcome to the KodakGallery Ripper"
puts ""

# open the file
print "  enter filename name:"
fileName = Dir.getwd().to_s + "/" + gets.chomp

puts "opening " + fileName

regExpFilter = Regexp.new("http://images.kodakgallery.com/servlet/Images/.*?\.jpg")

photosToDownload = Array.new
numLines = 0

File.open(fileName, "r") do |aFile|

	#  iterate through each line
	aFile.each_line do |line|
		#puts line.to_s
		numLines = numLines + 1
		
		matchData = regExpFilter.match(line)
		
		if matchData
			photosToDownload.push(matchData[0])
		end
	
	end
end

# report out on what we found
puts "Scanned " + numLines.to_s + " lines"
puts "Downloaded " + photosToDownload.length.to_s + " photos"

if photosToDownload.length > 0

	# create the directory
	print "  enter directory to save images in: "
	folderName = gets.chomp

	Dir.mkdir(folderName) unless File.directory?(folderName)
	Dir.chdir(folderName)
		
	# download each image and save it in the designated folder
	i = 1
	for imageUrl in photosToDownload

		imageFileName = i.to_s + ".jpg"
		open(imageFileName, "wb").write(open(imageUrl).read)
		
		i = i + 1
		
	end
end