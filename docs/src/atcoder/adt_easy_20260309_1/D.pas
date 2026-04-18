program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, qq, i, j: int8;
	d, ans: int32;
	q, r: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do readln(q[i], r[i]);

	readln(qq);
	for j := 1 to qq do begin
		readln(i, d);
		ans := d + r[i] - d mod q[i];
		if ans < d then inc(ans, q[i]);
		writeln(ans);
	end;
end.
