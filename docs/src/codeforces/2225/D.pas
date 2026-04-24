program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	prime = (998 * 1000 + 244) * 1000 + 353;
var
	notc, tci: int32;
	n, x, ans0, ans1, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function c0(x: int64): int64;
begin
	result := (x+5) div 4;
end;

function c1(x: int64): int64;
begin
	result := (x+3) div 4;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x);

		ans0 := (c0(n) - c0(x-1)) mod prime * (c0(x-1) mod prime);
		ans1 := (c1(n) - c1(x-1)) mod prime * (c1(x-1) mod prime);
		ans := (ans0 + ans1) mod prime;

		writeln(ans);

	end;
end.
