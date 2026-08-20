program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 3;
var
	i: int8;
	s: array [1 .. NN] of string;
	t: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to NN do readln(s[i]);
	readln(t);

	for ch in t do begin
		i := ord(ch) - ord('0');
		write(s[i]);
	end;
	writeln;
end.
