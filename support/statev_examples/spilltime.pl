#!/usr/bin/perl -w

use DBI;

require "plottools.pl";

# Connect to database
$dbase = "dbi:SQLite:dbname=example2.db";
$dbh   = DBI->connect($dbase, "", "", { RaiseError => 1}) or die $dbh->errstr;

# Do database query
$querys = "SELECT sum(e.bemain_insns_before_ra), sum(e.bemain_time_ra_spill)
FROM ctx as c, ev as e
WHERE e.id = c.id
GROUP BY c.bemain_irg";
$query = $dbh->prepare($querys) or die $dbh->errstr;
$query->execute() or die $dbh->errstr;
$data  = $query->fetchall_arrayref();
undef $query;
$dbh->disconnect();

# Begin drawing picture
print <<'__END__';
\begin{tikzpicture}[baseline=(current bounding box.south)]%
__END__

$box_width = 6;
$box_height = 4;
# Create a picture with size 6cm x 4cm
set_picture_size($box_width, $box_height);
# Set min and max data values
set_data_bounds(0, 0, 1600, 25);
# alternatively: determine bounds from data
# set_data_bounds_from_data($data);

# Draw the box, with x tikz at 200 units distance, y tikz in 5 units
outputbox(200, 5);

# Draw an X and Y label
$label_x = $box_width / 2;
$label_y = $box_height / 2;
print <<__EOF__;
	\\node at ($label_x, -0.6) { Number of Instructions };
	\\node[rotate=90] at (-0.65, $label_y) { Spill Time [msec.] };
__EOF__

# Setup for plotting
print <<__EOF__;
	\\pgfplothandlermark{\\pgfuseplotmark{+}}
	\\color{black}
__EOF__

# Draw values
plot($data);

# Draw linear regression line
draw_regression($data, "densely dashed, color=white");

# Finish the picture
print <<__EOF__;
\\end{tikzpicture}%
__EOF__
