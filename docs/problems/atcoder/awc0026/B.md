# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, ai: int32;
	k, t, a: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	t := 0;
	a := 0;
	for i := 1 to n do begin
		read(ai);
		if t+ai <= k then
			inc(t, ai)
		else
			inc(a, ai);
	end;
	readln;

	if t > a then
		writeln('Takahashi')
	else if a > t then
		writeln('Aoki')
	else
		writeln('Draw');
end.

```
