#$SNL = $ENV{'SLURM_NODELIST'};
#$nodelist = `scontrol show hostmame$SNL|paste -d, -s`;
#chomp($nodelist);

use List::Util qw[min max];

$min_proccess = 1;
$max_proccess = 16;

open(WF, "> ./output_fortran_matrix_vector.txt") or die;
for($i = 10000; $i <= 25000; $i+=1000) {
	for($l = 5000; $l <= 25000; $l+=5000) {
		print WF $i, " ", $l, " ";
		for($j = $min_proccess; $j <= $max_proccess; $j = $j*2) {
			$result = 100000.0;
			`mpiifort /Qcoarray:shared /Qcoarray-num-images:$j matrixvector.f90`;
			for($k = 0; $k < 5; $k++) {
				$result = min($result, `matrixvector $i $l` + 0.);
			}
			print WF $result, " ";
		}
		print WF " | ";
	}
	print WF "\n";
	close(WF);
}
