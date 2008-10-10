#!/usr/bin/perl -w

our $tikzinterval_x;
our $tikzinterval_y;

$tikzlen_x = "0.2cm";
$tikzlen_y = "0.2cm";
$labelspace_x = "0.3cm";
$labelspace_y = "0.3cm";

sub set_data_bounds_from_data {
	my $values = shift;

	foreach $aref ( @$values ) {
		my $x = @$aref[0];
		my $y = @$aref[1];
		if(!defined($minx)) {
			$minx = $maxx = $x;
			$miny = $maxy = $y;
		}
		$minx = $x if $x < $minx;
		$maxx = $x if $x > $maxx;
		$miny = $y if $y < $miny;
		$maxy = $y if $y > $maxy;
	}
}

sub set_data_bounds {
	($minx, $miny, $maxx, $maxy) = @_;
}

sub setup_transforms {
	$addx = -$minx;
	$addy = -$miny;
	$scalex = $box_width / ($maxx - $minx);
	$scaley = $box_height / ($maxy - $miny);
}

sub set_picture_size {
	($box_width, $box_height) = @_;
}

sub outputcoord {
	my $x = shift;
	my $y = shift;
	my $optional = shift;

	$x = ($x + $addx) * $scalex;
	$y = ($y + $addy) * $scaley;
	print("(");
	if(defined($optional)) {
		print("[$optional]");
	}
	print(" $x, $y)");
}

sub outputpoint {
	my $x = shift;
	my $y = shift;
	$x = ($x + $addx) * $scalex;
	$y = ($y + $addy) * $scaley;
	print "\\pgfpoint{${x}cm}{${y}cm}";
}

sub outputbox {
	my ($tikzinterval_x, $tikzinterval_y) = @_;
	setup_transforms();
print <<__EOF__;
	\\begin{scope}
		\\draw (0, 0) rectangle +($box_width, $box_height);
__EOF__

	my $t;
	for($t = $minx; $t < $maxx; $t += $tikzinterval_x) {
		print "\t\t\\node at ";
		outputcoord($t, $miny, "yshift=-$labelspace_x");
		print " { $t };\n";
		print "\t\t\\draw ";
		outputcoord($t, $miny);
		print "	-- ";
		outputcoord($t, $miny, "yshift=$tikzlen_x");
		print ";\n";
	}

	for($t = $miny; $t < $maxy; $t += $tikzinterval_y) {
		print "\t\t\\node at ";
		outputcoord($minx, $t, "xshift=-$labelspace_y");
		print " { $t };\n";
		print "\t\t\\draw ";
		outputcoord($minx, $t);
		print "	-- ";
		outputcoord($minx, $t, "xshift=$tikzlen_y");
		print ";\n";
	}

	print <<__EOF__;
	\\end{scope}
__EOF__
}

sub plot {
	my $values = shift;

	print <<__EOF__;
	\\pgfplotstreamstart
__EOF__

	VALUE: foreach $aref ( @$values ) {
		my $x = @$aref[0];
		my $y = @$aref[1];
		next VALUE if !defined($x) || !defined($y);
		next VALUE if $x < $minx or $x >= $maxx or $y < $miny or $y >= $maxy;

		print "\t\t\\pgfplotstreampoint{";
		outputpoint($x, $y);
		print "}\n";
	}

	print <<__EOF__;
	\\pgfplotstreamend
__EOF__
}

sub draw_regression {
	# see http://de.wikipedia.org/wiki/Regressionsanalyse#Berechnung_der_Regressionsgeraden
	my $values = shift;
	my $style  = shift;
	my $start_x = shift;
	my $end_x   = shift;

	if (!defined($start_x)) { $start_x = $minx; }
	if (!defined($end_x)) { $end_x = $maxx; }

	my $avg_x = 0.0;
	my $avg_y = 0.0;
	my $count = 0.0;
	V1: foreach $aref (@$values) {
		my $x = @$aref[0];
		my $y = @$aref[1];
		next V1 if (!defined($x) || !defined($y));

		if ($minx <= $x && $x <= $maxx && $miny <= $y && $y <= $maxy) {
			$avg_x += $x;
			$avg_y += $y;
			$count++;
		}
	}
	$avg_x /= $count;
	$avg_y /= $count;

	my $nominator   = 0.0;
	my $denominator = 0.0;
	V2: foreach $aref (@$values) {
		my $x = @$aref[0];
		my $y = @$aref[1];
		next V2 if (!defined($x) || !defined($y));

		if ($minx <= $x && $x <= $maxx && $miny <= $y && $y <= $maxy) {
			$nominator   += ($x - $avg_x) * ($y - $avg_y);
			$denominator += ($x - $avg_x) * ($x - $avg_x);
		}
	}
	my $b = $nominator / $denominator;
	my $a = $avg_y - $b * $avg_x;

	print "\t\\draw[$style] ";
	outputcoord($start_x, $a + $start_x * $b);
	print "	-- ";
	outputcoord($end_x, $a + $end_x * $b);
	print ";\n";
}
