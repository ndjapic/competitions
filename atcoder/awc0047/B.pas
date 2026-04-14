program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i: int32;
	w: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n-1 do read(w[i]);
	readln;

	i := 1;
	while (i < n) and ((w[i] = 0) or (m > 0)) do begin
		if (w[i] = 1) and (m > 0) then dec(m);
		inc(i);
	end;

	writeln(i);
end.
