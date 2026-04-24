program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, d, p, q, r, s: int8;
	a, b: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p, q, r, s);

	for i := 1 to n do begin
		read(a[i]);
		b[i] := a[i];
	end;
	readln;

	d := q-p;
	for i := 0 to d do b[r+i] := a[p+i];
	for i := 0 to d do b[p+i] := a[r+i];

	for i := 1 to n-1 do write(b[i], ' ');
	writeln(b[n]);
end.
