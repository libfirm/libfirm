#!/usr/bin/perl -w

use strict;
use XML::Simple;
use Data::Dumper;

my $resfile_name = "result.xml";

my $old_file  = $ARGV[0]."/".$resfile_name;
my $new_file  = $ARGV[1]."/".$resfile_name;
my $diff_file = "results_diff.xml";

$| = 1; # perform flush after each write to STDOUT

print "reading $old_file ... ";
my $start = time;
my $res_old = XMLin("$old_file", forcearray => 1);
print "done (", time - $start," s)\n";

print "reading $new_file ... ";
$start = time;
my $res_new = XMLin("$new_file", forcearray => 1);
print "done (", time - $start," s)\n";

open(DIFF, ">$diff_file") or die "Could not open $diff_file, reason:$!\n";

print DIFF "<?xml version=\"1.0\"?>\n";
print DIFF "<results>\n";
print DIFF "    <files>\n";
print DIFF "        <OLD>$old_file</OLD>\n";
print DIFF "        <NEW>$new_file</NEW>\n";
print DIFF "    </files>\n";

if (! exists($res_old->{"environment"})) {
	print "environment missing in $old_file!\n";
}
elsif (! exists($res_new->{"environment"})) {
	print "environment missing in $new_file!\n";
}
else {
	print DIFF "    <environment>\n";
	print DIFF "        <OLD>\n";

	my %env = %{ @{ $res_old->{"environment"} }[0] };

	foreach (keys(%env)) {
		print DIFF "            <$_>", @{ $env{"$_"} }[0], "</$_>\n";
	}
	print DIFF "        </OLD>\n";
	print DIFF "        <NEW>\n";

	%env = %{ @{ $res_new->{"environment"} }[0] };

	foreach (keys(%env)) {
		print DIFF "            <$_>", @{ $env{"$_"} }[0], "</$_>\n";
	}
	print DIFF "        </NEW>\n";
	print DIFF "    </environment>\n";
}

my $section_old = $res_old->{"section"};
my $section_new = $res_new->{"section"};

foreach (keys(%{ $section_old })) {
	if (! exists($section_new->{"$_"})) {
		print "Section $_ missing in $new_file!\n";
		next;
	}

	print DIFF "    <section name=\"$_\">\n";

	my $old = $section_old->{"$_"}{"result"};
	my $new = $section_new->{"$_"}{"result"};

	foreach my $test (keys(%{ $old })) {
		my $entry_old = $old->{"$test"};
		my $entry_new = $new->{"$test"};
		my $has_diff = 0;

		if (! exists($new->{"$test"})) {
			foreach my $k1 (keys(%{ $entry_old })) {
				$entry_new->{"$k1"} = [ 'missing' ];
				$has_diff           = 1;
			}
		}
		else {
			foreach my $k (keys(%{ $entry_old })) {
				if (! exists($entry_new->{"$k"})) {
				    $entry_new->{"$k"} = [ 'missing' ];
					$has_diff = 1;
				}
				else {
					my @val_old = @{ $entry_old->{"$k"} };
					my @val_new = @{ $entry_new->{"$k"} };
					if ($val_old[0] ne $val_new[0]) {
						$has_diff = 1;
					}
				}
			}
		}

		if ($has_diff == 1) {
			print DIFF "        <result name=\"$test\">\n";
			print DIFF "            <OLD>\n";
			emit_entry($entry_old);
			print DIFF "            </OLD>\n";
			print DIFF "            <NEW>\n";
			emit_entry($entry_new);
			print DIFF "            </NEW>\n";
			print DIFF "        </result>\n";
		}
	}

	print DIFF "    </section>\n";
}

print DIFF "</results>\n";

close(DIFF);

print "processing results_diff.xml ... ";
`xsltproc --output results_diff.html makediffhtml.xslt results_diff.xml`;
print "results_diff.html created\n";

sub emit_entry {
	my $href = shift;

	foreach (keys(%{ $href })) {
		my @val = @{ $href->{"$_"} };
		print DIFF "                <$_>", $val[0], "</$_>\n";
	}
}
