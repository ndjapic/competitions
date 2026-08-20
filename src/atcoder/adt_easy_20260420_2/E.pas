program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i: int32;
	p, q: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(p[i]);
		q[p[i]] := i;
	end;
	readln;

	for i := 1 to n-1 do write(q[i], ' ');
	writeln(q[n]);
end.
