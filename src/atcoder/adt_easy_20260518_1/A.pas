program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for i := 1 to length(s) do
		case s[i] of
			'0': s[i] := '1';
			'1': s[i] := '0';
		end;

	writeln(s);
end.
