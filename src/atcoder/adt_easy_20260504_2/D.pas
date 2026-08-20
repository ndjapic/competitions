program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, d, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(s);

	for i := n downto 1 do
		if (s[i] = '@') and (d > 0) then begin
			s[i] := '.';
			dec(d);
		end;

	writeln(s);
end.
