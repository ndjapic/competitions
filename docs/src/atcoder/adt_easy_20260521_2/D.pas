program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	for i := 1 to n do
		if s[i] = 'x' then
		else if k > 0 then begin
			dec(k)
		end else
			s[i] := 'x';

	writeln(s);
end.

