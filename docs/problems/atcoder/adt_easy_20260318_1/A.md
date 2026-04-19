# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	ch: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for i := 1 to 2 do
		if s[i] > s[i+1] then begin
			ch := s[i+1];
			s[i+1] := s[i];
			s[i] := ch;
		end;

	for i := 1 to 1 do
		if s[i] > s[i+1] then begin
			ch := s[i+1];
			s[i+1] := s[i];
			s[i] := ch;
		end;

	if s = 'ABC' then
		writeln('Yes')
	else
		writeln('No');
end.

```
