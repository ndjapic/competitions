program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, i, j, k: int32;
	a, b, ans, bns: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]);
	readln;

	for j := 1 to m do read(b[j]);
	readln;

	i := 1;
	j := 1;
	for k := 1 to n+m do
		if (j > m) or (i <= n) and (a[i] < b[j]) then begin
			ans[i] := k;
			inc(i);
		end else begin
			bns[j] := k;
			inc(j);
		end;

	for i := 1 to n-1 do write(ans[i], ' ');
	writeln(ans[n]);

	for j := 1 to m-1 do write(bns[j], ' ');
	writeln(bns[m]);
end.
