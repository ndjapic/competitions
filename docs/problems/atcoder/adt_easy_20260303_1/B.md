# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 8;
var
	i: int8;
	ans: boolean;
	s: array [1 .. n] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to n do read(s[i]);
	readln;

	i := 1;
	while (i < n) and (s[i] <= s[i+1]) do inc(i);
	ans := i = n;

	if ans then
		ans := (100 <= s[1]) and (s[n] <= 675);

	if ans then begin
		i := 1;
		while (i <= n) and (s[i] mod 25 = 0) do inc(i);
		ans := i > n;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
