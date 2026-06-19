program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, m, l, r: int64;
	ans: uint64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: uint64; m: uint32): uint64;
begin
	result := x div m;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, m, l, r);

	dec(l, a);
	dec(r, a);

	if l > 0 then
		ans := f(r, m) - f(l-1, m)
	else if r < 0 then
		ans := f(-l, m) - f(-r-1, m)
	else
		ans := f(-l, m) + f(r, m) + 1;

	writeln(ans);
end.
