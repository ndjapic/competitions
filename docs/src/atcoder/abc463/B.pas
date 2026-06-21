program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j: int8;
	x: char;
	s: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x, x);

	ans := false;
	j := ord(x) - ord('A') + 1;
	for i := 1 to n do begin
		readln(s);
		if not ans then
			ans := s[j] = 'o';
	end;

	if ans then
		write('Yes')
	else
		write('No');
end.
