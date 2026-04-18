# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	i := 1;
	found := false;
	while (i < n) and not found do begin
		found := (s[i] = 'a') and (s[i+1] = 'b') or (s[i] = 'b') and (s[i+1] = 'a');
		inc(i);
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.

```
