program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	for i := 1 to n do
		if k = 0 then
			s[i] := 'x'
		else if s[i] = 'o' then
			dec(k);

	writeln(s);
end.
