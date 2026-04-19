# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, mx: int32;
	s: string;
	ch: char;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	mx := 0;
	for i := 1 to n do begin
		ch := s[i];
		inc(c[ch]);
		mx := max(mx, c[ch]);
	end;

	for i := 1 to n do begin
		ch := s[i];
		if c[ch] < mx then write(ch);
	end;
	writeln;
end.

```
