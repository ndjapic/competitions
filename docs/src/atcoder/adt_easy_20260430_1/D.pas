program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	b: int64;
	l, r, m: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function xpowx(x: int8): uint64;
var
	i: int8;
begin
	result := 1;
	for i := 1 to x do result := result * x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(b);

	l := 1;
	r := 16;

	while r-l > 1 do begin
		m := (l+r) div 2;
		if xpowx(m) > b then
			r := m
		else
			l := m;
	end;

	if xpowx(l) < b then l := -1;
	writeln(l);
end.
