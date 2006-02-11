VERSION := 0.2.0

# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.
help_message.inc: help_message.txt
	file_to_pascal_string help_message.txt help_message.inc