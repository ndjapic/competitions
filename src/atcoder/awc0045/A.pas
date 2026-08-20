program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils;
var
	n, i, p: int32;
	j: int8;
	t: string;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for i := 1 to n do begin
		readln(t);

		if t[1] = 'n' then
			j := 6
		else
			j := 4;

		p := strtoint(copy(t, j+2, length(t) - j-1));
		if t[1] = 'h' then p := p div 2;
		inc(ans, p);
	end;

	writeln(ans);
end.
