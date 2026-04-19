# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	ch: char;
	cond1, cond2, cond3: boolean;
	s: string;
	c: array ['a' .. 'z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	n := length(s);
	cond1 := not odd(n);

	for ch := 'a' to 'z' do c[ch] := 0;
	for i := 1 to n do inc(c[s[i]]);

	i := 2;
	while (i <= n) and (s[i-1] = s[i]) do inc(i, 2);
	cond2 := i > n;

	ch := 'a';
	while (ch <= 'z') and (c[ch] <= 2) and (c[ch] <> 1) do inc(ch);
	cond3 := ch > 'z';

	if cond1 and cond2 and cond3 then
		writeln('Yes')
	else
		writeln('No');
end.

```
