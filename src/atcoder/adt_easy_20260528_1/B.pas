program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	case s[2] of
		'o': writeln(5);
		'u': writeln(4);
		'e': writeln(3);
		'h': writeln(2);
		'r': writeln(1);
	end;
end.
