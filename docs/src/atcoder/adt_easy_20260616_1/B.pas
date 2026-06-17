program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, d, cookies: int32;
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(s);

	cookies := -d;
	for ch in s do
		if ch = '@' then inc(cookies);

	writeln(n - cookies);
end.
