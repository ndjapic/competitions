program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 8;
var
	i: int8;
	ans: boolean;
	s: array [1 .. N] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	read(s[1]);

	ans := (100 <= s[1]) and (s[1] mod 25 = 0);

	for i := 2 to N do begin
		read(s[i]);
		if ans then
			ans := (s[i-1] <= s[i]) and (s[i] <= 675) and (s[i] mod 25 = 0);
	end;
	readln;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
