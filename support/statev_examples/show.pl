#!/usr/bin/perl -w
use DBI;

# Connect to database
$dbase = "dbi:SQLite:dbname=example1.db";
$dbh   = DBI->connect($dbase, "", "", { RaiseError => 1}) or die $dbh->errstr;

# Do database query
$querys = "
SELECT c.benchmark,sum(e.instructions),sum(e.reads),sum(e.writes),sum(e.spills),sum(e.reloads),sum(e.remats)
FROM ctx as c, ev as e
WHERE c.id = e.id
GROUP BY c.benchmark";
$query = $dbh->prepare($querys) or die $dbh->errstr;
$query->execute() or die $dbh->errstr;
$rows  = $query->fetchall_arrayref();
undef $query;
$dbh->disconnect();

# Format output
print <<'__END__';
\begin{tabular}{|l|r|r|r|r|}
	\hline
	Benchmark & Insns & Reads & Writes & Spills & Reloads & Remats
	\hline
__END__

sub toM {
	my $val = shift;
	return sprintf("%1.1f", $val / 1000000000);
}
@col_formats = ( 'lc(@$row[0])', 'toM(@$row[1])', 'toM(@$row[2])',
		         'toM(@$row[3])','toM(@$row[4])','toM(@$row[5])','toM(@$row[6])');
foreach my $row (@$rows) {
	my @results = ();
	foreach my $format (@col_formats) {
		push(@results, eval($format.';'));
	}

	print("\t".join(" & ", @results) . " \\\\\n");
}

print <<'__END__';
	\hline
\end{tabular}
__END__
