program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, l, r: int8;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for r := 1 to n do begin
		read(a[r]);
		l := r-1;
		while (l > 0) and (a[l] <= a[r]) do dec(l);
		if l = 0 then l := -1;
		writeln(l);
	end;
	readln;
end.
