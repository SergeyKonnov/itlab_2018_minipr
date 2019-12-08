#$SNL = $ENV{'SLURM_NODELIST'};
#$nodelist = `scontrol show hostmame$SNL|paste -d, -s`;
#chomp($nodelist);

use List::Util qw[min max];

open(WF, "> ./output_fortran_matrix_vector.txt") or die;
for($i = 10000; $i <= 25000; $i+=1000) {
	for($j = 1; $j <= 4; $j = $j*2) {
		$result = 0.0;
		`mpiifort /Qcoarray:shared /Qcoarray-num-images:$j matrixvector.f90`
		for($k = 0; $k < 5; $k++) {
			$result += `matrixvector`
		}
		$result/=5
		print WF $result;
		print WF " ";
	}
	print WF "\n";
	close(WF);
}
