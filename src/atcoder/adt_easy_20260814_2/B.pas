program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bit #shift #learn
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for i := 4 downto 2 do s[i] := s[i-1];
	s[1] := '0';

	writeln(s);
end.
