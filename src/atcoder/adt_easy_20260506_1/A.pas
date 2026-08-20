program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	s[1] := chr(ord(s[1]) - ord('A') + ord('a'));

	write('Of');
	writeln(s);
end.
