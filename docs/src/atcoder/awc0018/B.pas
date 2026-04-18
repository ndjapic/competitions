program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, i, j, k, kk: int32;
	c, d: array [1 .. nn] of int32;
	p: array [1 .. nn] of array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do begin
		read(c[i]);
		d[j] := 0;
	end;
	readln;

	for i := 1 to n do begin
		readln(kk);
		setlength(p[i], kk);

		for k := 0 to kk-1 do begin
			read(p[i][k]);
			inc(d[p[i][k]]);
		end;
		readln;

	end;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.
