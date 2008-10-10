#!/usr/bin/perl -w
#
# Takes the output of a cachegrind or countmem run and produces statev events

sub result {
	my $name = shift;
	my $value = shift;
	print "E;$name;$value\n"
}

while( defined($line = <STDIN>) ) {
	if($line =~ m/.* I\s+refs:\s+([0-9\,]+).*/) {
		$irefs = $1;
		$irefs =~ s/,//g;
		result("instructions", $irefs);
	}
	if($line =~ m/.* D\s+refs:\s+[0-9,]*\s+\(\s*([0-9,]+)\s+rd\s+\+\s+([0-9,]+).*/) {
		$reads = $1;
		$writes = $2;
		$reads =~ s/,//g;
		$writes =~ s/,//g;
		result("reads", $reads);
		result("writes", $writes);
	}
	if($line =~ m/.* D1\s+misses:\s+[0-9,]*\s+\(\s*([0-9,]+)\s+rd\s+\+\s+([0-9,]+).*/) {
		$reads = $1;
		$writes = $2;
		$reads =~ s/,//g;
		$writes =~ s/,//g;
		result("l1readmisses", $reads);
		result("l1writemisses", $writes);
	}
	if($line =~ m/.* L2d\s+misses:\s+[0-9,]*\s+\(\s*([0-9,]+)\s+rd\s+\+\s+([0-9,]+).*/)
	{
		$reads = $1;
		$writes = $2;
		$reads =~ s/,//g;
		$writes =~ s/,//g;
		result("l2readmisses", $reads);
		result("l2writemisses", $writes);
	}
	# CountMem format
	if($line =~ m/== ([a-zA-Z0-9\-]+)\s+([0-9,]+).*/) {
		$name = $1;
		$value = $2;
		$value =~ s/,//g;
		result($name, $value);
	}
}
