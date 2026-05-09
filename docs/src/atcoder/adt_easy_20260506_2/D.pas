program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, j, k: int8;
	a, s: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do read(a[j]);
	readln;

	k := 0;
	j := 1;
	for i := 1 to n do begin
		inc(k);
		s[k] := i;
		if (j <= m) and (i = a[j]) then
			inc(j)
		else
			while k > 0 do begin
				write(s[k], ' ');
				dec(k);
			end;
	end;
	writeln;
end.
