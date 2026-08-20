program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 20;
var
	n, i, e: int8;
	m: int32;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m);

	n := 0;
	e := 0;
	while m > 0 do begin
		while m mod 3 > 0 do begin
			inc(n);
			a[n] := e;
			dec(m);
		end;
		m := m div 3;
		inc(e);
	end;

	writeln(n);
	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.
