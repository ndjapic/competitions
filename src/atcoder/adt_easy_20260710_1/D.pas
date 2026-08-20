program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	MM = 1000;
var
	n, m, i, j: int32;
	s: string;
	a: array [1 .. MM] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	m := 0;
	for i := 1 to n do
		if s[i] = '#' then begin
			inc(m);
			a[m] := i;
		end;

	for j := 1 to m do begin
		write(a[j]);
		if odd(j) then
			write(',')
		else
			writeln;
	end;
end.
