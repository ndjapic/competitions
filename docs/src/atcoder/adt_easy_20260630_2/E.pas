program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, k: int32;
	s: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	k := 0;
	for i := 1 to n do begin
		inc(k);
		read(s[k]);

		if (k >= 4) and (s[k] = s[k-1]) and (s[k] = s[k-2]) and (s[k] = s[k-3]) then
			dec(k, 4);
	end;
	readln;

	writeln(k);
end.
