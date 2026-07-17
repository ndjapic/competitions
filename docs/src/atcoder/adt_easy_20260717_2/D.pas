program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	MM = 100;
var
	m, i, a, b: int32;
	d: array [0 .. MM] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m);

	d[0] := 0;
	for i := 1 to m do begin
		read(d[i]);
		inc(d[i], d[i-1]);
	end;
	readln;

	b := (d[m] + 1) div 2;
	a := 1;
	while d[a] < b do inc(a);
	dec(b, d[a-1]);

	writeln(a, ' ', b);
end.
