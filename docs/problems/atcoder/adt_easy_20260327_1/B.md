# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	ch: char;
	s: string;
	seen: array ['0' .. '9'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch := '0' to '9' do seen[ch] := false;

	for i := 1 to 9 do seen[s[i]] := true;

	for ch := '0' to '9' do
		if not seen[ch] then writeln(ch);
end.

```
