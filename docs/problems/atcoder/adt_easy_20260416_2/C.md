# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 8;
var
	x, y, k: int8;
	s: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	x := 1;
	y := n;
	while s[x] <> 'B' do inc(x);
	while s[y] <> 'B' do dec(y);

	ans := odd(x+y);

	if ans then begin

		x := 1;
		y := n;
		while s[x] <> 'R' do inc(x);
		while s[y] <> 'R' do dec(y);

		k := 1;
		while s[k] <> 'K' do inc(k);
		ans := ans and (x < k) and (k < y);

	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
