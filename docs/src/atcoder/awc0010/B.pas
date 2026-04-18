program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i: int32;
	s: int64;
	d: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	read(d[1]);
	s := d[1];
	for i := 2 to n do begin
		read(d[i]);
		if d[i-1] < d[i] then
			inc(s, d[i] div 2)
		else
			inc(s, d[i]);
	end;
	readln;

	writeln(s);
end.
