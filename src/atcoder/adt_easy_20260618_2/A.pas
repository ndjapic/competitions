program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch in s do
		case ch of
			'0': write('1');
			'1': write('0');
		end;
	writeln;
end.
