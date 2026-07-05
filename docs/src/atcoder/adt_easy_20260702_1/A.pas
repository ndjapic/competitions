program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function hexd(o: int8): char;
begin
	if o < 10 then
		inc(o, ord('0'))
	else
		inc(o, ord('A') - 10);
	result := chr(o);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	setlength(s, 2);
	s[1] := hexd(n div 16);
	s[2] := hexd(n mod 16);
	writeln(s);
end.
