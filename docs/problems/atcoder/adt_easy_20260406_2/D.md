# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, ans: int8;
	lin: boolean;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	ans := 0;

	for i := 1 to n do begin
		readln(s);
		if s = 'login' then
			lin := true
		else if s = 'logout' then
			lin := false
		else if s = 'public' then
		else if not lin then
			inc(ans);
	end;

	writeln(ans);
end.

```
